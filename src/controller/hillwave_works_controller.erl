-module(hillwave_works_controller, [Req, SessionId]).
-compile(export_all).
-include("hillwave.hrl").

index('OPTIONS', []) -> {json, [], ?OPTG};
index('GET', []) ->
  Works = boss_db:find(work, []),
  Json = [{"data", [jsonapi:res_object(X) || X <- Works]}],
  {json, Json, ?HEADERS}.

view('OPTIONS', [Id]) -> {json, [], ?OPTG};
view('GET', [Id]) ->
  case boss_db:find(Id) of
    {error, _} -> {400, ?INVALID_REQUEST, ?HEADERS};
    Work -> {json, jsonapi:data(Work), ?HEADERS}
  end.
