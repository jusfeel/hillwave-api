-module(jsonapi).
-compile(export_all).
-include("hillwave.hrl").

res_object(Record) ->
  Relationships = get_relationships(Record),
  Type = atom_to_list(element(1, Record)),
  Data = get_data(Type, Record),
  lists:append(Data, Relationships).

res_object(Record, HasManys) ->
  Relationships = [{"relationships", lists:append(proplists:get_value("relationships", get_relationships(Record)), HasManys)}],
  Type = atom_to_list(element(1, Record)),
  Data = get_data(Type, Record),
  lists:append(Data, Relationships).

data(Record) ->
  [{"data", res_object(Record)}].

data(Record, HasManys) ->
  data(Record, HasManys, true).

data(Record, HasManys, _) when HasManys =:= [] ->
  data(Record);
data(Record, HasManys, false) ->
  F = fun({Key, Records}) -> {Key, boss_record_ids(Records)} end,
  [{"data", res_object(Record, [F(X) || X <- HasManys])}];
data(Record, HasManys, true) ->
  F = fun({Key, Records}) -> {Key, boss_record_ids(Records)} end,

  Fold = fun(Elem, Acc) -> lists:append(Elem, Acc) end,
  F2 = fun({_, Records}) -> lists:map(fun(Elem) -> res_object(Elem) end, Records) end,
  Included = lists:foldl(Fold, [], [F2(X) || X <- HasManys]),
  [{"data", res_object(Record, [F(X) || X <- HasManys])}, {"included", Included}].


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
        false when is_boolean(Value) ->
          "\"" ++ Key ++ "\":" ++ bool_to_list(Value);
        false ->
          DoubleQuotesEscaped = re:replace(Value,"\"","\\\\\"",[global,{return,list}]),
          "\"" ++ Key ++ "\":\"" ++ DoubleQuotesEscaped ++ "\""
      end
  end.

bool_to_list(Boolean) when Boolean =:= true -> "true";
bool_to_list(Boolean) when Boolean =:= false -> "false".

to_json(Record) ->
  escape(proplist_to_json(data(Record))).

escape(Json) ->
  re:replace(Json,"\n","\\\\n",[global,{return,list}]).

boss_record_ids(Record) when is_tuple(Record) ->
  Type = atom_to_list(element(1, Record)),
  [{"data", [{"id", Record:id()}, {"type", inflector:pluralize(Type)}]}];
boss_record_ids(List) when is_list(List) ->
  F = fun(Record) ->
        Type = inflector:pluralize(atom_to_list(element(1, Record))),
        [{"id", Record:id()}, {"type", inflector:pluralize(Type)}]
      end,
  [{"data", [F(X) || X <- List ]}].


get_data(Type, Record) ->
  Attrs = proplists:delete("id", [{atom_to_list(K), V} || {K,V} <- Record:attributes()]),
  [{"id", Record:id()}, {"type", Type}, {"attributes", Attrs}].

get_relationships(Record) ->
  case Record:belongs_to() of
    [] -> [];
    B -> process_belongs_to(B, [])
  end.

process_belongs_to([], Acc) -> [{"relationships", Acc}];
process_belongs_to([{BelongsToModel, Record}|T], Acc) ->
  M = atom_to_list(BelongsToModel),
  NewAcc = [{M, boss_record_ids(Record)}|Acc],
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

err(Code, Title) ->
  "{\"errors\": [{\"code\":\"" ++ Code ++ "\", \"title\":\"" ++ Title ++ "\"}]}".

links(Model, Number, Size, Total) ->
  Base = ?DOMAIN ++ "/" ++ inflector:pluralize(atom_to_list(Model)) ++  "?",
  Params = fun(Num, S) -> "page[number]=" ++ integer_to_list(Num) ++ "&page[size]=" ++ integer_to_list(S) end,

  Self = {"self", Base ++ Params(Number, Size)},
  Last = {"last", Base ++ Params(Total, Size)},
  case Number < Total of
    true ->
      Next = {"next", Base ++ Params(Number + 1, Size)},
      [Self, Next, Last];
    _ ->
      [Self, Last]
  end.






