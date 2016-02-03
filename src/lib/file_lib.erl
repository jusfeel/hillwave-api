-module(file_lib).
-compile(export_all).
-include_lib("kernel/include/file.hrl").

list_by_folder(Dir) ->
  {ok, FL} = file:list_dir(Dir),
  DL = lists:filter(fun(A) -> filelib:is_dir(Dir ++ "/" ++ A) end, FL),
  F = fun(Elem) -> 
        {ok, Files} = file:list_dir(Dir ++ "/" ++ Elem),
        
        {Elem, [Elem ++ "/" ++ X || X <- lists:sort(Files)]} 
        end,
  lists:map(F, lists:sort(DL)).

total_size(Dir) ->
  {ok, FL} = file:list_dir(Dir),
  lists:foldl(fun(Filename,SizeAcc) ->
                FileInfo = file:read_file_info(Dir ++ "/" ++ Filename),
                FileSize = FileInfo#file_info.size,
                SizeAcc + FileSize
            end, 0, FL).

file_info(Dir) ->
  {ok, FileInfo} = file:read_file_info(Dir),
  io:format("~p~n", [FileInfo]),
  FileSize = FileInfo#file_info.size,
  FileSize.
