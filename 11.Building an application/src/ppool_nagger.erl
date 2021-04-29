%% demo module, a nagger for tasks,
%% because the previous one wasn't good enough
%%
%% Can take:
%% - a time delay for which to nag,
%% - an adress to say where the messages should be sent
%% - a message to send in the mailbox telling you what to nag,
%%   with an id to be able to call: ->
%% - a command to say the task is done 

%%% Init Args
%%% • Task is the thing to send as a message.
%%% • Delay is the time spent in between each sending.
%%% • Max is the number of times it’s going to be sent.
%%% • SendTo is a pid or a name where the message will go.

-module(ppool_nagger).
-behaviour(gen_server).

%% API
-export([stop/1, start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

stop(Pid) ->
    gen_server:call(Pid, stop).

start_link(Task, Delay, Max, SendTo) ->
    gen_server:start_link(?MODULE, {Task, Delay, Max, SendTo}, []).

init({Task, Delay, Max, SendTo}) ->
    {ok, {Task, Delay, Max, SendTo}, Delay}. %% After a certain delay, handle_info will be executed

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, {Task, Delay, Max, SendTo}) ->
    SendTo ! {self(), Task},
    if Max =:= infinity ->
        {noreply, {Task, Delay, Max, SendTo}, Delay};
    Max =< 1 ->
        {stop, normal, {Task, Delay, 0, SendTo}};
    Max > 1 ->
        {noreply, {Task, Delay, Max-1, SendTo}, Delay}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
