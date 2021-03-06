-module(ppool_serv).
-behaviour(gen_server).

%%% Functions of this ppool_server
%%% run() ->
%%% Running a task in the pool and indicating that it can’t be started if the pool is full
%%% sync_run() ->
%%% Running a task in the pool if there’s room; otherwise, keeping the calling
%%% process waiting while the task is in the queue, until it can be run
%%% async_run() ->
%%% Running a task asynchronously in the pool, as soon as possible; if no 
%%% place is available, queuing it up and running it whenever

%% API
-export([start/4, stop/1, start_link/4, code_change/3, terminate/2]).
-export([run/2, sync_queue/2, async_queue/2]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2]).

-define(SPEC(MFA),
        {worker_sup,
            {ppool_worker_sup, start_link, [MFA]},
            permanent,
            10000,
            supervisor,
            [ppool_worker_sup]}).

-record(state, {limit=0,
                sup,
                refs,
                queue = queue:new()}).


start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

stop(Name) ->
    gen_server:call(Name, stop).

run(Name, Args) ->
    % This starts running the task in the pool if available
    gen_server:call(Name, {run, Args}).

sync_queue(Name, Args) ->
    gen_server:call(Name, {sync, Args}, infinity).

async_queue(Name, Args) ->
    gen_server:cast(Name, {async, Args}).

init({Limit, MFA, Sup}) ->
    %% Find the PID of the worker supervisor from here
    self() ! {start_worker_supervisor, Sup, MFA}, % handled at handle_info/2
    {ok, #state{limit=Limit, refs=gb_sets:empty()}}.

handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{refs=Refs}) ->
    case gb_sets:is_element(Ref, Refs) of
        true ->
            handle_down_worker(Ref, S);
        false -> %% Not our responsibility
            {noreply, S}
    end;
handle_info({start_worker_supervisor, Sup, MFA}, S = #state{}) ->
    {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
    {noreply, S#state{sup=Pid}};
handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
    {noreply, State}.

handle_call({run, Args}, _From, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_call({run, _Args}, _From, S = #state{limit=N}) when N =< 0 ->
    {reply, noalloc, S};

handle_call({sync, Args}, _From, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok,Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_call({sync, Args},  From, S = #state{queue=Q}) ->
    {noreply, S#state{queue=queue:in({From, Args}, Q)}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({async, Args}, S=#state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {noreply, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_cast({async, Args}, S=#state{limit=N, queue=Q}) when N =< 0 ->
    {noreply, S#state{queue=queue:in(Args,Q)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_down_worker(Ref, S = #state{limit=L, sup=Sup, refs=Refs}) ->
    case queue:out(S#state.queue) of
        {{value, {From, Args}}, Q} ->
            {ok, Pid} = supervisor:start_child(Sup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref,Refs)),
            gen_server:reply(From, {ok, Pid}),
            {noreply, S#state{refs=NewRefs, queue=Q}};
        {{value, Args}, Q} ->
            {ok, Pid} = supervisor:start_child(Sup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref,Refs)),
            {noreply, S#state{refs=NewRefs, queue=Q}};
        {empty, _} ->
            {noreply, S#state{limit=L+1, refs=gb_sets:delete(Ref,Refs)}}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.