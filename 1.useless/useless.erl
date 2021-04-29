-module(useless).
-export([add/2, hello/0, greet_and_add_two/1]).
%% -import(io,[format/1]). allows to use format(...) instead of io:format !! Discouraged. Reduces readability !!
%% define compilation flag directly in the module
-compile([debug_info, export_all]).

add(A,B) ->
    A + B.

%% Shows greetings.
%% io:format/1 is the standard function used to output text.
hello() ->
    io:format("Hello, world!~n").

greet_and_add_two(X) ->
    hello(),
    add(X,2).