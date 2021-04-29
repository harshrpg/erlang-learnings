-module(ppool_supersup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    MaxRestart = 6,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.


%% Technically, a supervisor cannot be killed in an easy way.
%% Let's do it brutally!
stop() ->
    case whereis(ppool) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
        end.

start_pool(Name, Limit, MFA) ->
    io:format("###################################################~n"),
    io:format("in Super Supervisor~n"),
    io:format("Creating Child Spec~n"),
    ChildSpec = {Name,
                {ppool_sup, start_link, [Name, Limit, MFA]},
                permanent, 10500, supervisor, [ppool_sup]},
    io:format("Child Spec Created~p~n", [ChildSpec]),
    supervisor:start_child(?MODULE, ChildSpec).

stop_pool(Name) ->
    supervisor:terminate_child(ppool, Name),
    supervisor:delete_child(ppool, Name).