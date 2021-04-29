-module(multiproc).
% -export([sleep/1, flush/0]).
-compile(export_all).

sleep(T) ->
    receive
    after T -> ok
    end.

flush() ->
    receive 
        _ -> flush()
    after 0 ->
        ok
    end.

% Selective Receives

important() ->
    receive
        {Priority, Message} when Priority > 10 ->
            [Message | important()]
    after 0 ->
        normal()
    end.

normal() ->
    receive
        {_, Message} ->
            [Message | normal()]
    after 0 ->
        []
    end.
