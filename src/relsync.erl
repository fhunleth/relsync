-module(relsync).

-export([main/1]).

-spec main([string()]) -> ok.
main(CmdLine) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, CmdLine) of
	{ok, {Options, _NonOptArgs}} ->
	    Command = proplists:get_value(command, Options),
	    ok;
	{error, {Reason, Data}} ->
	    io:format("Error: ~s ~p~n~n", [Reason, Data]),
	    usage()
    end.

-spec usage() -> ok.
usage() ->
    getopt:usage(option_spec_list(), "relsync").

-spec option_spec_list() -> [getopt:option_spec()].
option_spec_list() ->
    [
     %% {Name,     ShortOpt,    LongOpt,    ArgSpec,    HelpMsg}
     {help,        $?,          "help",     undefined,  "Show program options"},
     {config,      $c,          "config",   {string, "fwtool.config"},     "Config file (create, run)"}
     ].
