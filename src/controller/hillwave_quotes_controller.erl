-module(hillwave_quotes_controller, [Req, SessionId]).
-compile(export_all).
-include("hillwave.hrl").

before_(_,_,_) ->
  account_lib:require_login(Req).

index('OPTIONS', _, _) -> {json, [], ?OPTA};
index('GET', [Id], _) -> {json, jsonapi:data(boss_db:find(Id)), ?HEADERS};
index('GET', [], _) ->
  Quotes = [jsonapi:res_object(X) || X <- boss_db:find(quote, []) ],
  Json = [{"data", Quotes}],
  {json, Json, ?HEADERS};
index('POST', [], {error, Reason}) ->
  {401, jsonapi:error(?ERR_INVALID_REQUEST, Reason), ?HEADERS};
index('POST', [], {ok, CurrentMember}) ->
  ReqBody = jsx:decode(Req:request_body(), [{labels, atom}]),
  Content = jsonapi:attribute_value(quoted_content, ReqBody),
  PersonId = jsonapi:related_id(person, ReqBody),
  Quote = quote:new(id, Content, PersonId, CurrentMember:id(), util:now_iso()),
  case Quote:save() of
    {ok, NewQuote} -> {json, jsonapi:data(NewQuote), ?HEADERS};
    {error, Reason} -> {500, "{\"error\":\"" ++ Reason ++ "\"}", ?HEADERS}
  end.
