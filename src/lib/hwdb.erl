-module(hwdb).
-compile(export_all).
-include("hillwave.hrl").

err(Reason) when is_atom(Reason) ->
  err(atom_to_list(Reason));
err(Reason) ->
  invalid_request(Reason).

invalid_request(Reason) ->
  {400, jsonapi:err(?ERR_INVALID_REQUEST, Reason), ?HEADERS}.

server_error(Reason) ->
  {500, jsonapi:err(?ERR_SERVER_ERROR, Reason), ?HEADERS}.

find(Id) ->
  case boss_db:find(Id) of
    {error, Reason} -> invalid_request(Reason);
    BossRecord -> {json, jsonapi:data(BossRecord), ?HEADERS}
  end.

find(Id, Extras) when is_list(Id), is_list(Extras) ->
  case boss_db:find(Id) of
    {error, Reason} -> invalid_request(Reason);
    BossRecord -> {json, jsonapi:data(BossRecord, [{atom_to_list(X), BossRecord:X()} || X <- Extras]), ?HEADERS}
  end;
find(Model, Cond) when is_atom(Model) ->
  Records = [jsonapi:res_object(X) || X <- boss_db:find(Model, Cond) ],
  {json, [{"data", Records}, {"total", length(Records)}], ?HEADERS}.
find(Model, Cond, Opts) when is_atom(Model) ->
  % per page
  Limit = proplists:get_value(limit, Opts),
  case Limit of
    undefined -> Size = 12;
    _ -> Size = Limit
  end,
  % starting page
  Offset = proplists:get_value(offset, Opts),
  case Offset of
    undefined -> Number = 1;
    _ -> Number = util:ceiling(Offset/Size) + 1
  end,

  Records = [jsonapi:res_object(X) || X <- boss_db:find(Model, Cond, Opts) ],
  TotalPages = util:ceiling(boss_db:count(Model) / Size),
  Links = jsonapi:links(Model, Number, Size, TotalPages),
  {json, [{"data", Records}, {"meta", [{"total_pages", TotalPages}]}, {"links", Links}], ?HEADERS}.

patch_save(Record) ->
  case Record:save() of
    {error, Reason} -> server_error(Reason);
    {ok, Saved}       -> {200, jsonapi:to_json(Saved), ?HEADERS}
  end.

patch(Req) ->
  ReqBody = jsx:decode(Req:request_body(), [{labels, atom}]),
  Data = proplists:get_value(data, ReqBody),
  Id = binary_to_list(proplists:get_value(id, Data)),
  F = fun(Elem) ->
        case is_binary(Elem) of
          true -> binary_to_list(Elem);
          _    -> Elem
        end
      end,

  Attributes = [{K, F(V)} || {K,V} <- proplists:get_value(attributes, Data)],
  case validation:attributes(Attributes) of
    {error, Errors} -> {422, jsonapi:proplist_to_json([{"errors", Errors}]), ?HEADERS};
    ok              ->
      case boss_db:find(Id) of
        {error, Reason} -> server_error(Reason);
        Record -> patch_save(Record:set(Attributes))
      end
  end.

post_save(Record) ->
  case Record:save() of
    {error, Reason} -> server_error(Reason);
    {ok, Saved}       -> {201, jsonapi:to_json(Saved), ?HEADERS}
  end.

post(Req) ->
  ReqBody = jsx:decode(Req:request_body(), [{labels, atom}]),
  Data = proplists:get_value(data, ReqBody),
  F = fun(Elem) ->
        case is_binary(Elem) of
          true -> binary_to_list(Elem);
          _    -> Elem
        end
      end,

  Attributes = [{K, F(V)} || {K,V} <- proplists:get_value(attributes, Data)],
  Type = binary_to_list(proplists:get_value(type, Data)),
  Model = list_to_atom(inflector:singularize(Type)),
  Record = boss_record:new(Model, Attributes),
  case validation:attributes(Attributes) of
    {error, Errors} -> {422, jsonapi:proplist_to_json([{"errors", Errors}]), ?HEADERS};
    ok              -> post_save(Record)
  end.

delete(Id) when Id =:= undefined -> invalid_request("");
delete(Id) ->
  case boss_db:delete(Id) of
    {error, Reason} -> server_error(Reason);
    ok              -> {204, [], ?HEADERS}
  end.



