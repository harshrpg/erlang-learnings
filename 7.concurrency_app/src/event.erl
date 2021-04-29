-module(event).
-compile(export_all).
-record(state, {server,
                name="",
                to_go=[]}).

start(EventName, Delay) ->
    spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
    spawn_link(?MODULE, init, [self(), EventName, Delay]).

% Event's innards
init(Server, EventName, DateTime) ->
    loop(#state{server=Server,
                name=EventName,
                to_go=time_to_go(DateTime)}).

loop(S = #state{server=Server, to_go=[T|Next]}) ->
    receive
        % cancel message
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after T*1000 -> % changint to_go to miliseconds
        if 
            Next =:= [] ->
                Server ! {done, S#state.name};
            Next =/= [] ->
                loop(S#state{to_go=Next})
        end
    end.

normalize(N) ->
    Limit = 49*24*60*60, % 49 days limit of timeout in erlang
    [N rem Limit | lists:duplicate(N div Limit, Limit)]. % Reproduce Limit as many times as N div Limit

cancel(Pid) ->
    % Monitor in case the process is already dead
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]), % flush options kills the DOWN message if it was sent before we had time to demonitor
            ok;
        {'DOWN', Ref, process, Pid, _Reason} -> % If monitor says that the process is dead then simply return ok
            ok
    end.

time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) - 
            calendar:datetime_to_gregorian_seconds(Now),
    Secs = if ToGo > 0 -> ToGo;
                ToGo =<0 -> 0
            end,
    normalize(Secs).