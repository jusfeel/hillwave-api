-module(account_lib).
-compile(export_all).
-include("hillwave.hrl").

register(Email, Username, Password) ->
  PasswordHash = security:generate_password_hash(Password),
  Id = "member-" ++ util:sha(Email),
  Member = member:new(Id, Email, PasswordHash, Username, true, false, util:now_iso()),
  case Member:save() of
    {ok, NewMember} ->
      {[[{id, Id}]], Attrs} = proplists:split(NewMember:attributes(), [id]),
      [{"data", [{"type", "member"}, {"id", Id}, {"attributes", Attrs}]}];
    {error, _Reason} ->
      [{"errors", [{"code", ?ERR_OPERATION_FAILURE}, {"error", "Operation failure"}]}]
  end.

authenticate(Uname, Password) ->
  case boss_db:find(member, [{email, 'eq', Uname}]) of
    [] -> {error, "Not found"};
    [Member] ->
      case security:check_password(Password, Member:password_hash()) of
        true  -> {ok, Member};
        false -> {error, "Bad password"}
      end
  end.

require_login(Req) ->
  case Req:header("Authorization") of
    undefined ->
      ?LOG("authorization is undefined"),
      Response = {error, "Invalid request"};
    AuthToken ->
      [_Bearer, Token] = re:split(AuthToken, "Bearer "),
      ?DEBUG("..~p", Token),
      Response = case oauth2:verify_access_token(Token, []) of
        {error, _} ->
          {error, "Invalid request"};
        {ok, {_AppCtx, GrantCtx}} ->
          {user, MemberId} = proplists:get_value(<<"resource_owner">>, GrantCtx),
          {ok, boss_db:find(MemberId)}
      end
  end,
  {ok, Response}.
