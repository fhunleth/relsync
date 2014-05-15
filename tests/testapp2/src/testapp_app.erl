-module(testapp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Hello from testapp2~n"),
    testapp_sup:start_link().

stop(_State) ->
    ok.
