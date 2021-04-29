-module(functions).
-compile(export_all).

head([H|_]) -> H.

second([_,X|_]) -> X.

%% Method overloading
same(X,X) -> true;
same(_,_) -> false.

%% Working with dates
valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
    io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n", [Date, Y,M,D]),
    io:format("The Time tuple (~p) indicates: ~p:~p:~p,~n", [Time,H,Min,S]);
valid_time(_) ->
    io:format("Stop feeding me wrong data!~n").

%% Guards [Conditional Functions]
old_enough(X) when X >= 16, X =< 104 -> true; %% A Guard must always return true
%% `,` works as `and` called as `andalso` in erlang
old_enough(_) -> false.