-module(ppool_supersup).
-behaviour(supervisor).

%% API
-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).
-export([init/1]).

%%% The Top level process pool supervisor is called 
%%% ppool
%%% {local, Name} is an OTP convention about registering 
%%% gen_* processes on a node
start_link() ->
    supervisor:start_link({local, ppool}, ?MODULE, []).

%%% Technical supervisor cannot be killed in the normal way
%%% We will need to do it brutally
stop() ->
    case whereis(ppool) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.

%%% Since this is a top-level supervisor, its only task is to 
%%% hold pools in memory and supervise them
%%% right now its a childless supervispr
init([]) ->
    MaxRestart = 6,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

start_pool(Name, Limit, MFA) ->
    ChildSpec = {Name,
                {ppool_sup, start_link, [Name, Limit, MFA]},
                permanent, 10500, supervisor, [ppool_sup]},
    supervisor:start_child(ppool, ChildSpec).

stop_pool(Name) ->
    supervisor:teminate_child(ppool, Name),
    supervisor:delete_child(ppool, Name).