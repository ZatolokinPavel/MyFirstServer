-module(myfirstserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("starting app~n"),
    myfirstserver_sup:start_link().

stop(_State) ->
    io:format("stopping app~n"),
    ok.
