-module(hillwave_people_controller, [Req, SessionId]).
-compile(export_all).
-include("hillwave.hrl").

before_(_,Method,_) ->
  case Method of
    'GET' -> ok;
    _ -> account_lib:require_login(Req)
  end.

index('OPTIONS', _, _) -> {200, "", ?OPTA};
index('POST', [], {error, Reason}) ->
  {400, jsonapi:error(?ERR_INVALID_REQUEST, Reason), ?HEADERS};
index('DELETE', [Id], {error, Reason}) ->
  {400, jsonapi:error(?ERR_INVALID_REQUEST, Reason), ?HEADERS};
index('PATCH', [Id], {error, Reason}) ->
  {400, jsonapi:error(?ERR_INVALID_REQUEST, Reason), ?HEADERS};
index('GET', [Id], _) ->
  case boss_db:find(Id) of
    {error, _} -> {400, ?INVALID_REQUEST, ?HEADERS};
    P -> {json, jsonapi:data(P), ?HEADERS}
  end;
index('GET', [], _) ->
  P = boss_db:find(person, []),
  People = [jsonapi:res_object(X) || X <- P],
  Json = [{"data", People}],
  {json, Json, ?HEADERS};
index('POST', [], {ok, CurrentMember}) ->
  ?DEBUG("~p", Req:request_body()),
  hwdb:post(Req);
index('PATCH', [Id], {ok, CurrentMember}) ->
  %% Validate PATCH resource object
  ?DEBUG("~p", Req:request_body()),
  hwdb:patch(Req);

index('DELETE', [Id], {ok, _CurrentMember}) ->
  case Id of
    undefined -> {400, ?INVALID_REQUEST, ?HEADERS};
    PersonId ->
      case boss_db:delete(PersonId) of
        {error, Reason} -> {500, jsonapi:error(?ERR_SERVER_ERROR, Reason), ?HEADERS};
        ok              -> {204, [], ?HEADERS}
      end
  end.



