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

update_nodes(Options) ->
    update_node(nodes(), Options).
update_node([], _Options) ->
    ok;
update_node([Node|T], Options) ->
    {ok, _} = target_syncer_sup:start_child(Node),
    DestPath = proplists:get_value(destpath, Options),
    LocalPath = proplists:get_value(localpath, Options),
    DestFileHashes = target_syncer:get_file_hashes(Node, DestPath),
    LocalFileHashes = target_syncer:get_local_file_hashes(LocalPath),
    synchronize_node(Node, LocalPath, LocalFileHashes, DestPath, DestFileHashes),
    update_node(T, Options).

-spec normalize_path(string()) -> string().
normalize_path(Path) ->
    case lists:last(Path) of
	$/ -> Path;
	_ -> Path ++ "/"
    end.

-spec synchronize_node(atom(), string(),  [{string(), binary()}], string(), [{string(), binary()}]) -> ok.
synchronize_node(Node, LocalPath, LocalFileHashes, DestPath, DestFileHashes) ->
    SortedLocalHashes = lists:sort(LocalFileHashes),
    SortedDestHashes = lists:sort(DestFileHashes),
    sync_files(Node, normalize_path(LocalPath), SortedLocalHashes, normalize_path(DestPath), SortedDestHashes).

-spec sync_files(atom(), string(), [{string(), binary()}], string(), [{string(), binary()}]) -> ok.
sync_files(_Node, _LocalPath, [], _DestPath, []) -> ok;
sync_files(Node, LocalPath, [{LocalFile,_LocalHash} | LTail], DestPath, []) ->
    io:format("Creating ~p on ~p...~n", [LocalFile, Node]),
    {ok, Contents} = file:read_file(LocalPath ++ LocalFile),
    ok = target_syncer:copy_file(Node, DestPath ++ LocalFile, Contents),
    sync_files(Node, LocalPath, LTail, DestPath, []);
sync_files(Node, LocalPath, [], DestPath, [{DestFile,_DestHash} | DTail]) ->
    io:format("Deleting ~p from ~p...~n", [DestFile, Node]),
    target_syncer:rm_file(Node, DestPath ++ DestFile),
    sync_files(Node, LocalPath, [], DestPath, DTail);
sync_files(Node, LocalPath, [{LocalFile,LocalHash} | LTail], DestPath, [{DestFile,DestHash} | DTail]) when LocalFile =:= DestFile, LocalHash =:= DestHash ->
    sync_files(Node, LocalPath, LTail, DestPath, DTail);
sync_files(Node, LocalPath, [{LocalFile,LocalHash} | LTail], DestPath, [{DestFile,DestHash} | DTail]) when LocalFile =:= DestFile, LocalHash =/= DestHash ->
    io:format("Updating ~p on ~p...~n", [LocalFile, Node]),
    {ok, Contents} = file:read_file(LocalPath ++ LocalFile),
    ok = target_syncer:copy_file(Node, DestPath ++ LocalFile, Contents),
    sync_files(Node, LocalPath, LTail, DestPath, DTail);
sync_files(Node, LocalPath, [{LocalFile,LocalHash} | LTail], DestPath, [{DestFile,_DestHash} | DTail]) when LocalFile > DestFile ->
    io:format("Deleting ~p from ~p...~n", [DestFile, Node]),
    target_syncer:rm_file(Node, DestPath ++ DestFile),
    sync_files(Node, LocalPath, [{LocalFile,LocalHash} | LTail], DestPath, DTail);
sync_files(Node, LocalPath, [{LocalFile,_LocalHash} | LTail], DestPath, [{DestFile,DestHash} | DTail]) when LocalFile < DestFile ->
    io:format("Creating ~p on ~p...~n", [LocalFile, Node]),
    {ok, Contents} = file:read_file(LocalPath ++ LocalFile),
    ok = target_syncer:copy_file(Node, DestPath ++ LocalFile, Contents),
    sync_files(Node, LocalPath, LTail, DestPath, [{DestFile,DestHash} | DTail]).

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
