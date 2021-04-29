-module(linkmon).
-export([myproc/0]).

myproc() ->
    timer:sleep(5000),
    exit(reason).