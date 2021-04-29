%%% This module holds all the generic parts of the kitty server
-module(my_server).
-compile(export_all).

%% Public API
start(Module, InitialState) ->
    spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
    spawn_link(fun() -> init(Module, InitialState) end).

%% Synchronous calls
call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid), % Monitor makes the calls sync
    Pid ! {sync, self(), Ref, Msg}, % atom `sync` is just an identifier
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

cast(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.

%%% Private stuff
init(Module, InitialState) ->
    loop(Module, Module:init(InitialState)).

loop(Module, State) ->
    receive
        {async, Msg} ->
            loop(Module, Module:handle_cast(Msg, State));
        {sync, Pid, Ref, Msg} ->
            loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
    end.

%%% When working with abstractions, references are a kind of leak.
%%% References are only used by the program to follow through the messages and 
%%% their replies

reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.
