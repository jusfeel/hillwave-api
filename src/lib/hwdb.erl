-module(hwdb).
-compile(export_all).
-include("hillwave.hrl").

patch_save(Record) ->
  case Record:save() of
    {error, Reason} -> {500, jsonapi:error(?ERR_SERVER_ERROR, Reason), ?HEADERS};
    {ok, Saved}       -> {200, jsonapi:to_json(Saved), ?HEADERS}
  end.

patch(Req) ->
  ReqBody = jsx:decode(Req:request_body(), [{labels, atom}]),
  Data = proplists:get_value(data, ReqBody),
  Id = binary_to_list(proplists:get_value(id, Data)),
  Attributes = [{K, binary_to_list(V)} || {K,V} <- proplists:get_value(attributes, Data)],
  case validation:attributes(Attributes) of
    {error, Errors} -> {422, jsonapi:proplist_to_json([{"errors", Errors}]), ?HEADERS};
    ok              ->
      case boss_db:find(Id) of
        {error, Reason} -> {403, jsonapi:error(?ERR_SERVER_ERROR, Reason), ?HEADERS};
        Record ->
          NewRecord = Record:set(Attributes),
          patch_save(NewRecord)
      end
  end.

post_save(Record) ->
  case Record:save() of
    {error, Reason} -> {500, jsonapi:error(?ERR_SERVER_ERROR, Reason), ?HEADERS};
    {ok, Saved}       -> {201, jsonapi:to_json(Saved), ?HEADERS}
  end.

post(Req) ->
  ReqBody = jsx:decode(Req:request_body(), [{labels, atom}]),
  Data = proplists:get_value(data, ReqBody),
  Attributes = [{K, binary_to_list(V)} || {K,V} <- proplists:get_value(attributes, Data)],
  Type = binary_to_list(proplists:get_value(type, Data)),
  Model = list_to_atom(inflector:singularize(Type)),
  Record = boss_record:new(Model, Attributes),
  post_save(Record).
