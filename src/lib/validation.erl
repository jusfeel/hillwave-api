-module(validation).
-export([
          validate_registration/4,
          attributes/1,
          api_key/1
        ]).
-include("hillwave.hrl").

api_key(Req) ->
  ?DEBUG("~p", Req:headers()),
  ?DEBUG("~p", Req:header("API_KEY")),
  valid_api(Req:header(api_key), Req:header(secret_word)).

valid_api(?API_KEY, ?SECRET_WORD) -> true;
valid_api(_,_) -> false.

validate_registration(Email, Username, Password1, Password2) ->
  F = fun(Result, Acc) ->
      case Result of
        {error, Error} -> [Error|Acc];
        ok -> Acc
      end
  end,
  Errors =  lists:foldl(F, [], [
                                validate_not_empty([Email, Username, Password1, Password2]),
                                validate_email(Email),
                                validate_passwords(Password1, Password2),
                                validate_password_length(Password1),
                                validate_username_length(Username),
                                validate_existed(Email)
                               ]),
  case length(Errors) of
    0 -> ok;
    _ -> {error, [{"errors", Errors}]}
  end.

attributes(Attrs) ->
  attributes(Attrs, []).
attributes([], []) ->
  ok;
attributes([], Acc) ->
  {error, Acc};
attributes([{K, Value}|T], Acc) ->
  Key = case is_atom(K) of
          true -> atom_to_list(K);
          false -> K
        end,
  ValueIsList = is_list(Value),
  if ValueIsList ->
      case length(Value) of
        0 ->
          NewAcc = [[{"detail", "The attribute '" ++ Key ++ "' is required"}, {"source", [{"pointer","data/attributes/" ++ Key}]}]|Acc],
          attributes(T, NewAcc);
        _ ->
          attributes(T, Acc)
      end;
    true ->
      %% boolean, number
      attributes(T, Acc)
  end.

validate_not_empty(List) ->
  F = fun(Elem) ->
        if Elem =:= "" -> true; true -> false end
      end,
  case lists:any(F, List) of
     true  -> {error, [{"code", ?ERR_INCOMPLETE_INPUT}, {"title", "Incomplete input."}]};
     false -> ok
  end.

validate_email(Email) ->
  case email_address:is_valid(Email) of
    true  -> ok;
    false -> {error, [{"code", ?ERR_INVALID_EMAIL_FORMAT}, {"title", "Email format is invalid."}]}
  end.

validate_passwords(P1, P2) ->
  if P1 =:= P2 -> ok;
     true      -> {error, [{"code", ?ERR_PASSWORDS_NOT_SAME}, {"title","Two passwords provided are different."}]}
  end.

validate_password_length(P) ->
  case length(P) < ?MIN_PASSWORD_LENGTH of
    true -> {error, [{"code", ?ERR_PASSWORD_TOO_SHORT}, {"title", "Password is less than the minimun length:" ++ integer_to_list(?MIN_PASSWORD_LENGTH)}]};
    false -> ok
  end.

validate_username_length(Username) ->
  case length(Username) > ?MAX_USERNAME_LENGTH of
    true -> {error, [{"code", ?ERR_USERNAME_TOO_LONG}, {"title", "Username is longer than the maximun length:" ++ integer_to_list(?MAX_USERNAME_LENGTH)}]};
    false -> ok
  end.

validate_existed(Email) ->
  case boss_db:count(member, [{email, 'eq', Email}]) of
    0 -> ok;
    _ -> {error, [{"code", ?ERR_ACCOUNT_EXISTS}, {"title", "Account exists already."}]}
  end.










