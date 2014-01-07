-module(relsync).

-export([main/1]).

-spec main([string()]) -> ok.
main(CmdLine) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, CmdLine) of
	{ok, {Options, _NonOptArgs}} ->
	    target_syncer_sup:start_link(),
	    ok = setup_local_node(Options),
	    ok = update_nodes(Options),
	    ok;
	{error, {Reason, Data}} ->
	    io:format("Error: ~s ~p~n~n", [Reason, Data]),
	    usage()
    end.


%% Use the command line parameters to setup the local
%% Erlang instance to be able to talk to remote nodes.
-spec setup_local_node([term()]) -> ok.
setup_local_node(Options) ->
    % First, make sure that Epmd is running
    case net_adm:names() of
	{ok, _} -> %% Epmd is running
	    ok;
	{error, address} ->
	    Epmd = os:find_executable("epmd"),
	    os:cmd(Epmd ++ " -daemon")
    end,

    % Next, start up net_kernel
    case net_kernel:start(options_to_netkernel(Options)) of
	{ok, _} ->
	    case is_alive() of
		true ->
		    Cookie = proplists:get_value(cookie, Options),
		    erlang:set_cookie(node(), list_to_atom(Cookie)),
		    ok;
		false ->
		    {error, set_cookie}
	    end;
	{error, Reason} ->
	    {error, net_kernel, Reason}
    end.

-spec options_to_netkernel([term()]) -> [atom()].
options_to_netkernel(Options) ->
    case proplists:get_value(sname, Options) of
	undefined ->
	    case proplists:get_value(name, Options) of
		undefined ->
		    exit({badargs, "Specify --sname or --name"});
		Name ->
		    [list_to_atom(Name), longnames]
	    end;
	SName ->
	    [list_to_atom(SName), shortnames]
    end.

update_nodes(Options) ->
    % Only support one node for now.
    DestNode = proplists:get_value(destnode, Options),
    update_node([list_to_atom(DestNode)], Options).
update_node([], _Options) ->
    ok;
update_node([Node|T], Options) ->
    % Ping the node to make sure that it is connected
    io:format("Updating ~p...~n", [Node]),
    pong = net_adm:ping(Node),

    % Start up the remote syncer
    {ok, _} = target_syncer_sup:start_child(Node),

    % Start syncing
    DestPath = proplists:get_value(destpath, Options),
    LocalPath = proplists:get_value(localpath, Options),
    Hooks = proplists:get_value(hooks, Options),
    ok = target_syncer:set_hooks(Node, Hooks),

    ok = target_syncer:notify_presync(Node),

    DestFileHashes = target_syncer:get_file_hashes(Node, DestPath),
    LocalFileHashes = target_syncer:get_local_file_hashes(LocalPath),
    synchronize_node(Node, LocalPath, LocalFileHashes, DestPath, DestFileHashes),
    update_node(T, Options),

    ok = target_syncer:notify_postsync(Node).

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
     {destpath,    $p,       "destpath",    {string, "/srv/erlang"},"Path to release on the destination"},
     {localpath,   $l,       "localpath",   {string, "./_rel"},     "Path to local release"},
     {hooks,       $h,       "hooks",       string, "Erlang module containing hooks to run on the destination"},
     {cookie,      $c,       "cookie",      {string, "cookie"}, "Erlang magic cookie to use"},
     {sname,       $s,       "sname",       string, "Short name for the local node"},
     {name,        $n,       "name",        string, "Long name for the local node"}
     ].
