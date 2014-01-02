-module(relsync).

-export([main/1]).

-spec main([string()]) -> ok.
main(CmdLine) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, CmdLine) of
	{ok, {Options, _NonOptArgs}} ->
	    io:format("Options were: ~p~n", [Options]),
	    target_syncer_sup:start_link(),
	    setup_networking(Options),
	    load_module(target_syncer),
	    update_nodes(Options),
	    ok;
	{error, {Reason, Data}} ->
	    io:format("Error: ~s ~p~n~n", [Reason, Data]),
	    usage()
    end.


%% Set ourselves up as an Erlang node and connect to the destination
-spec setup_networking([term()]) -> ok.
setup_networking(Options) ->
    set_node_name(Options),
    Cookie = proplists:get_value(cookie, Options),
    erlang:set_cookie(node(), list_to_atom(Cookie)),
    DestNode = list_to_atom(proplists:get_value(destnode, Options)),
    pong = net_adm:ping(DestNode),
    ok.

-spec set_node_name([term()]) -> ok.
set_node_name(Options) ->
    case proplists:get_value(sname, Options) of
	undefined ->
	    case proplists:get_value(name, Options) of
		undefined ->
		    io:format("Error: need to specify --sname or --name~n"),
		    exit(badargs);
		Name ->
		    {ok, _} = net_kernel:start([list_to_atom(Name), longnames])
	    end;
	SName ->
	    {ok, _} = net_kernel:start([list_to_atom(SName), shortnames])
    end,
    ok.

%% Load the target_syncer on all remote nodes
load_module(Module) ->
    {Mod, Bin, File} = code:get_object_code(Module),
    {Replies, BadNodes} = rpc:multicall(nodes(), code, load_binary, [Mod, File, Bin], 5000),
    ok = check_for_badnodes(BadNodes),
    ok = check_code_load_replies(Replies),
    ok.

check_for_badnodes(BadNodes) ->
    check_for_badnodes(BadNodes, ok).
check_for_badnodes([], IsOk) ->
    IsOk;
check_for_badnodes([BadNode|T], _IsOk) ->
    io:format("Bad node: ~p~n", [BadNode]),
    check_for_badnodes(T, {error, badnode}).

check_code_load_replies([Reply|T]) ->
    case Reply of
	{module, _} ->
	    check_code_load_replies(T);
	_ ->
	    io:format("Remote code load failed? ~p~n", Reply),
	    {error, codeloadfailed}
    end;
check_code_load_replies([]) -> ok.

update_nodes(Options) ->
    update_node(nodes(), Options).
update_node([], _Options) ->
    ok;
update_node([Node|T], Options) ->
    {ok, _} = target_syncer_sup:start_child(Node),
    DestPath = proplists:get_value(destpath, Options),
    %LocalPath = proplists:get_value(localpath, Options),
    io:format("Checking registered processes: ~p~n", [rpc:call(Node, erlang, registered, [], 5000)]),
    Hashes = target_syncer:get_file_hashes(Node, DestPath),
    io:format("~p: ~p~n", [Node, Hashes]),
    update_node(T, Options).

-spec usage() -> ok.
usage() ->
    getopt:usage(option_spec_list(), "relsync").

-spec option_spec_list() -> [getopt:option_spec()].
option_spec_list() ->
    [
     %% {Name,     ShortOpt, LongOpt,       ArgSpec,    HelpMsg}
     {destnode,    $d,       "destnode",    {string, "node@other"}, "Destination node"},
     {destpath,    $p,       "destpath",    {string, "/sys/erlang"},"Path to release on the destination"},
     {localpath,   $l,       "localpath",   {string, "./_rel"},     "Path to local release"},
     {script,      $s,       "script",      {string, "./relsync.script"}, "Script to run on the destination"},
     {cookie,      $c,       "cookie",      {string, "cookie"}, "Erlang magic cookie to use"},
     {sname,       $n,       "sname",       string, "Short name for the local node"},
     {name,        $m,       "name",        string, "Long name for the local node"}
     ].
