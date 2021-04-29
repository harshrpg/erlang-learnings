-module(recursion).
-export([fac/1]).

fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).

% Base case is when N = 0