-module(ppool_sup).
-behaviour(supervisor).

%% API
-export([start_link/3]).
-export([init/1]).

start_link(Name, Limit, MFA) ->
    supervisor:start_link(?MODULE, {Name, Limit, MFA}).

init({Name, Limit, MFA}) ->
    MaxRestart = 1,
    MaxTime = 3600,
    % {ok, {{RestartStrategy, MaxRestart, MaxTime},[ChildSpecs]}}.
    % ChildSpec == {ChildId, StartFunc, Restart, Shutdown, Type, Modules}.
    % StartFunc = {Module, Function, [Args]}
    % Restart = permanent, temporary, transient
    % Type = worker or supervisor
    {ok, {{one_for_all, MaxRestart, MaxTime},
        [{serv,
            {ppool_serv, start_link, [Name, Limit, self(), MFA]}, % self() = Server's own PID
            permanent,
            5000, %Shutdown Time
            worker,
            [ppool_serv]}]}}.