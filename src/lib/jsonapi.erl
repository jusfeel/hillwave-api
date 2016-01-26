-module(jsonapi).
-compile(export_all).
-include("hillwave.hrl").

res_object(Record) ->
  Relationships = get_relationships(Record),
  Type = atom_to_list(element(1, Record)),
  Data = get_data(Type, Record),
  lists:append(Data, Relationships).

data(Record) ->
  [{"data", res_object(Record)}].

is_proplist(List) ->
  is_proplist(List, true).

is_proplist([], true) ->
  true;
is_proplist(_, false) ->
  false;
is_proplist([{_,_}|T], true) ->
  is_proplist(T, true);
is_proplist([_|T], true) ->
  is_proplist(T, false);
is_proplist(_, true) ->
  false.

is_proplist_list(L) ->
  is_proplist_list(L, true).
is_proplist_list(_, false) ->
  false;
is_proplist_list([], true) ->
  true;
is_proplist_list([H|T], true) ->
  case is_proplist(H) of
    true -> is_proplist_list(T, true);
    false -> is_proplist_list(T, false)
  end;
is_proplist_list(_, true) ->
  false.

%% convert proplist to json
%%
%% rules:
%%
%% a proplist is a object
%% a list of proplist is a list of object
%% an object value can be either object or a list of objects
%% a item in list must be an object
%% the key is string, the value, if not a proplist, it must be string.
%%

proplist_to_json(List) ->
 List1 = [tuple_to_json(X) || X <- List],
 "{" ++ string:join(List1, ",") ++ "}".

tuple_to_json({Key, Value}) when is_atom(Key) ->
  tuple_to_json({atom_to_list(Key), Value});
tuple_to_json({Key, Value}) when is_list(Key) ->
  case is_proplist(Value) of
    true -> "\"" ++ Key ++ "\":" ++ proplist_to_json(Value);
    false ->
      case is_proplist_list(Value) of
        true -> "\"" ++ Key ++ "\":[" ++ string:join([proplist_to_json(X)|| X <- Value], ",") ++ "]";
        false -> "\"" ++ Key ++ "\":\"" ++ Value ++ "\""
      end
  end.

to_json(Record) ->
  escape(proplist_to_json(data(Record))).

escape(Json) ->
  re:replace(Json,"\n","\\\\n",[global,{return,list}]).

boss_record_ids(Type, Record) ->
  [{"data", [{"id", Record:id()}, {"type", Type}]}].

get_data(Type, Record) ->
  Attrs = proplists:delete("id", [{atom_to_list(K), V} || {K,V} <- Record:attributes()]),
  [{"id", Record:id()}, {"type", Type}, {"attributes", Attrs}].

get_relationships(Record) ->
  case Record:belongs_to() of
    [] -> [];
    B -> process_belongs_to(B, [])
  end.

process_belongs_to([], Acc) -> [{"relationships", Acc}];
process_belongs_to([{Model, Record}|T], Acc) ->
  M = atom_to_list(Model),
  NewAcc = [{M, boss_record_ids(M, Record)}|Acc],
  process_belongs_to(T, NewAcc).

parse_data_attributes(Keys, Object) ->
  Data = proplists:get_value(data, Object),
  Attrs = proplists:get_value(attributes, Data),
  attributes(Keys, Attrs).

parse_data_id(Object) ->
  Data = proplists:get_value(data, Object),
  proplists:get_value(id, Data).

attributes(KeyList, Attrs) ->
  F = fun(Key) -> string:strip(utf8cn:bin_to_list(proplists:get_value(Key, Attrs))) end,
  lists:map(F, KeyList).

attribute_value(Field, Body) ->
  Data = proplists:get_value(data, Body),
  Attrs = proplists:get_value(attributes, Data),
  utf8cn:bin_to_list(proplists:get_value(Field, Attrs)).

related_id(Model, Body) ->
  Data = proplists:get_value(data, Body),
  Rela = proplists:get_value(relationships, Data),
  Pers = proplists:get_value(Model, Rela),
  Data1 = proplists:get_value(data, Pers),
  utf8cn:bin_to_list(proplists:get_value(id, Data1)).

error(Code, Title) ->
  "{\"errors\": [{\"code\":\"" ++ Code ++ "\", \"title\":\"" ++ Title ++ "\"}]}".







