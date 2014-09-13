
-module(myfirstserver_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() -> 
    io:format("starting sup link~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    io:format("starting sup init~n"),
    {ok, { {one_for_one, 5, 10},
        [{myfirstserver, {myfirstserver, start_link, []},
        permanent, brutal_kill, worker, [myfirstserver]}]
    } }.
