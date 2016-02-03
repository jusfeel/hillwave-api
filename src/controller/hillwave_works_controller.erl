-module(hillwave_works_controller, [Req, SessionId]).
-compile(export_all).
-include_lib("kernel/include/file.hrl").
-include("hillwave.hrl").

before_(_,Method,_) when Method =:= 'OPTIONS' -> {ok,ok};
before_(_,Method,_) when Method =:= 'GET' -> {ok,ok};
before_(_,_,_) -> account_lib:require_login(Req).

index('OPTIONS', _, _) -> {200, "", ?OPTA};
index(_, _, {error, Reason}) -> hwdb:err(Reason);
index('GET', [Id], _) -> hwdb:find(Id);
index('GET', [], _) ->
  case Req:query_params() of
    [] -> hwdb:find(work, [], [{order_by, created}, {descending, true}]);
    Params ->
      Limit = list_to_integer(proplists:get_value("recent", Params)),

      hwdb:find(work, [], [{limit, Limit}, {order_by, created}, {descending, true}])
  end;
index('POST', [], {ok,_}) -> hwdb:post(Req);
index('PATCH', [Id], {ok,_}) -> hwdb:patch(Req);
index('DELETE', [Id], {ok,_}) -> hwdb:delete(Id).

upload('OPTIONS', _, _) -> {200, "", ?OPTA};
upload(_, _, {error, Reason}) -> hwdb:err(Reason);
upload('POST', [], {ok, CurrentMember}) ->
  Files = Req:post_files(),
  ?DEBUG("Files~p", Files),
  UploadedFile = hd(Files),
  TempFile = sb_uploaded_file:temp_file(UploadedFile),
  ?DEBUG("Temp file:~p", TempFile),
  %% @TODO check image type and thumbnailing
  NewImageName = uuid:to_string(uuid:uuid4()) ++ ".jpg",

  %% create new folder in case first time. folder name is the uploader id
  UploadFolder = ?UPLOAD_ROOT ++ "/" ++ CurrentMember:id(),
  file:make_dir(UploadFolder),
  NewFile = UploadFolder ++ "/" ++ NewImageName,

  case file:rename(TempFile, NewFile) of
    ok -> {json, [{"filename", CurrentMember:id() ++ "/" ++ NewImageName}], ?HEADERS};
    {error, Reason} -> ?DEBUG("rename failed~p", Reason), hwdb:err(Reason)
  end.






