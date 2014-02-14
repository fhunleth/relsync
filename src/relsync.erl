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

    DestFileInfos = target_syncer:get_file_listing(Node, DestPath),
    LocalFileInfos = target_syncer:get_local_file_listing(LocalPath),
    synchronize_node(Node, LocalPath, LocalFileInfos, DestPath, DestFileInfos),
    ok = target_syncer:notify_postsync(Node),

    % Do the next node.
    update_node(T, Options).


-spec normalize_path(string()) -> string().
normalize_path(Path) ->
    case lists:last(Path) of
	$/ -> Path;
	_ -> Path ++ "/"
    end.

% Return true if this is a safe file to synchronize
-spec safe_file({string(), {integer(),binary()}}) -> true | false.
safe_file({Filename,_Info}) ->
    filename:extension(Filename) =/= ".so".

% Synchronize the nodes by taking the local and remote file
% lists, filtering and sorting them, and then comparing them
% one by one to make sure that both sides are in sync.
-spec synchronize_node(atom(), string(),  [{string(), {integer(),binary()}}], string(), [{string(), {integer(),binary()}}]) -> ok.
synchronize_node(Node, LocalPath, LocalFileInfos, DestPath, DestFileInfos) ->
    FilteredLocalInfos = lists:filter(fun safe_file/1, LocalFileInfos),
    SortedLocalInfos = lists:sort(FilteredLocalInfos),
    FilteredDestInfos = lists:filter(fun safe_file/1, DestFileInfos),
    SortedDestInfos = lists:sort(FilteredDestInfos),
    sync_files(Node, normalize_path(LocalPath), SortedLocalInfos, normalize_path(DestPath), SortedDestInfos).

-spec sync_files(atom(), string(), [{string(), {integer(),binary()}}], string(), [{string(), {integer(),binary()}}]) -> ok.
sync_files(_Node, _LocalPath, [], _DestPath, []) -> ok;
sync_files(Node, LocalPath, [{LocalFile,LocalInfo} | LTail], DestPath, []) ->
    io:format("Creating ~p on ~p...~n", [LocalFile, Node]),
    {ok, Contents} = file:read_file(LocalPath ++ LocalFile),
    {Mode,_} = LocalInfo,
    ok = target_syncer:copy_file(Node, DestPath ++ LocalFile, Mode, Contents),
    sync_files(Node, LocalPath, LTail, DestPath, []);
sync_files(Node, LocalPath, [], DestPath, [{DestFile,_DestInfo} | DTail]) ->
    io:format("Deleting ~p from ~p...~n", [DestFile, Node]),
    target_syncer:rm_file(Node, DestPath ++ DestFile),
    sync_files(Node, LocalPath, [], DestPath, DTail);
sync_files(Node, LocalPath, [{LocalFile,LocalInfo} | LTail], DestPath, [{DestFile,DestInfo} | DTail]) when LocalFile =:= DestFile, LocalInfo =:= DestInfo ->
    sync_files(Node, LocalPath, LTail, DestPath, DTail);
sync_files(Node, LocalPath, [{LocalFile,LocalInfo} | LTail], DestPath, [{DestFile,DestInfo} | DTail]) when LocalFile =:= DestFile, LocalInfo =/= DestInfo ->
    io:format("Updating ~p on ~p...~n", [LocalFile, Node]),
    {ok, Contents} = file:read_file(LocalPath ++ LocalFile),
    {Mode,_} = LocalInfo,
    ok = target_syncer:copy_file(Node, DestPath ++ LocalFile, Mode, Contents),
    sync_files(Node, LocalPath, LTail, DestPath, DTail);
sync_files(Node, LocalPath, [{LocalFile,LocalInfo} | LTail], DestPath, [{DestFile,_DestInfo} | DTail]) when LocalFile > DestFile ->
    io:format("Deleting ~p from ~p...~n", [DestFile, Node]),
    target_syncer:rm_file(Node, DestPath ++ DestFile),
    sync_files(Node, LocalPath, [{LocalFile,LocalInfo} | LTail], DestPath, DTail);
sync_files(Node, LocalPath, [{LocalFile,LocalInfo} | LTail], DestPath, [{DestFile,DestInfo} | DTail]) when LocalFile < DestFile ->
    io:format("Creating ~p on ~p...~n", [LocalFile, Node]),
    {ok, Contents} = file:read_file(LocalPath ++ LocalFile),
    {Mode,_} = LocalInfo,
    ok = target_syncer:copy_file(Node, DestPath ++ LocalFile, Mode, Contents),
    sync_files(Node, LocalPath, LTail, DestPath, [{DestFile,DestInfo} | DTail]).

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
