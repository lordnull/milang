%%% @doc Managing the enviroment for the milang ui/compiler.
-module(milang_env).

-export(
	[ append_search_dir/1
	, prepend_search_dir/1
	, search_dirs/0
	, package/1
	, add_src_dir/1
	, src_dirs/0
	]).

search_dirs() ->
	application:get_env(milang, search_dirs, []).

append_search_dir(Path) ->
	Dirs = search_dirs(),
	NewDirs = Dirs ++ [ Path ],
	application:set_env(milang, search_dirs, NewDirs).

prepend_search_dir(Path) ->
	Dirs = search_dirs(),
	NewDirs = [Path | Dirs],
	application:set_env(milang, search_dirs, NewDirs).

package(PackageName) ->
	SearchDirs = search_dirs(),
	package(PackageName, SearchDirs).

package(_PackageName, []) ->
	{error, not_found};
package(PackageName, [ Dir | Tail]) ->
	FileName = filename:join([Dir, PackageName, <<"milang.package">>]),
	case filelib:is_dir(FileName) of
		true ->
			{ok, FileName};
		false ->
			package(PackageName, Tail)
	end.

src_dirs() ->
	application:get_env(milang, src_dirs, []).

add_src_dir(Dir) ->
	Dirs = src_dirs(),
	NewDirs = [ Dir | Dirs ],
	application:set_env(milang, src_dirs, NewDirs).
