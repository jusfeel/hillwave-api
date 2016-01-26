-module(hillwave_proses_controller, [Req, SessionId]).
-compile(export_all).
-include("hillwave.hrl").

before_(_) ->
  account_lib:require_login(Req).

index('OPTIONS', _, _) -> {json, [], ?OPTA};
index('GET', [Id], _) -> {json, jsonapi:data(boss_db:find(Id)), ?HEADERS};
index('GET', [], _) ->
  W = work:new("work-1", "Work 1", "Description 1", "/upload/1/sushi_1_wanghulouzuishu.jpg", "writer-1", "prose-1", "member-1", true, util:now_iso()),
  {[[{id, WID1}]], WAttrs1} = proplists:split(W:attributes(), [id]),
  W1 = [{"type", "work"}, {"id", WID1}, {"attributes", WAttrs1}],

  X = work:new("work-2", "Work 2", "Description 2", "/upload/1/sushi_1_wanghulouzuishu.jpg", "writer-1", "prose-1", "member-1", true, util:now_iso()),
  {[[{id, WID2}]], WAttrs2} = proplists:split(X:attributes(), [id]),
  W2 = [{"type", "work"}, {"id", WID2}, {"attributes", WAttrs2}],

  Y = work:new("work-3", "Work 3", "Description 3", "/upload/1/sushi_1_wanghulouzuishu.jpg", "writer-1", "prose-1", "member-1", true, util:now_iso()),
  {[[{id, WID3}]], WAttrs3} = proplists:split(Y:attributes(), [id]),
  W3 = [{"type", "work"}, {"id", WID3}, {"attributes", WAttrs3}],

  P1 = prose:new("prose-1", "Prose 1", "Prose description 1", "author-1", "member-1", util:now_iso()),
  {[[{id, ID1}]], Attrs1} = proplists:split(P1:attributes(), [id]),
  Relates = [{"works", [{"data", [W1,W2,W3]}]}],
  A1 = [{"type", "prose"}, {"id", ID1}, {"attributes", Attrs1}, {"relationships", Relates}],

  P2 = prose:new("prose-2", "Prose 2", "Prose description 2", "author-1", "member-1", util:now_iso()),
  {[[{id, ID2}]], Attrs2} = proplists:split(P2:attributes(), [id]),
  Relates2 = [{"works", [{"data", [[{"type", "work"},{"id", "work-4"}]]}]}],
  A2 = [{"type", "prose"}, {"id", ID2}, {"attributes", Attrs2}, {"relationships", Relates2}],

  P3 = prose:new("prose-3", "Prose 3", "Prose description 3", "author-1", "member-1", util:now_iso()),
  {[[{id, ID3}]], Attrs3} = proplists:split(P3:attributes(), [id]),
  Relates3 = [{"works", [{"data", [[{"type", "work"},{"id", "work-5"}], [{"type", "work"},{"id", "work-6"}]]}]}],
  A3 = [{"type", "prose"}, {"id", ID3}, {"attributes", Attrs3}, {"relationships", Relates3}],

  C = [{"data", [A1, A2, A3]}],
  {json, C, ?HEADERS};

index('POST', [], _) ->
  ReqBody = jsx:decode(Req:request_body(), [{labels, atom}]),
  Data = proplists:get_value(data, ReqBody),
  Attrs = proplists:get_value(attributes, Data),
  Title = proplists:get_value(title, Attrs),
  Desc = proplists:get_value(description, Attrs),
  Created = proplists:get_value(created, Attrs),
  Prose = prose:new("prose-N", Title, Desc, "author-1", "member-1", Created),
  {[[{id, Id}]], Attrs1} = proplists:split(Prose:attributes(), [id]),
  Json = [{"data", [{"type", "prose"}, {"id", Id}, {"attributes", Attrs1}]}],
  {json, Json, ?HEADERS}.
