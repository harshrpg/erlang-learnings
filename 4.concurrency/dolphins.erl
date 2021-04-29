-module(dolphins).
-export([dolphin1/0, dolphin2/0, dolphin3/0]).

dolphin1() ->
    receive
        do_a_flip ->
            io:format("How about no>");
        fish ->
            io:format("Thanks");
        _ ->
            io:format("what?")
    end.

% receive
% Pattern1 when Guard1 -> Expr1;
% Pattern2 when Guard2 -> Expr2;
% Pattern3 -> Expr3
% end

dolphin2() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no>";
        {From, fish} ->
            From ! "Thanks";
        _ ->
            io:format("what?")
    end.

% If once a message has been received the process is terminated.
% In order to receive another message, the process needs to be respawned
% A workaround would be to use recursion

dolphin3() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no>",
            dolphin3();
        {From, fish} ->
            From ! "Thanks";
        _ ->
            io:format("what?"),
            dolphin3()
    end.