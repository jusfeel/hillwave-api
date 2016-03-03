%% -------------------------------------------------------------------
%%
%% app_lib: application library
%%
%% Copyright (c) 2016 Hillwave Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @doc A utility library to provide a set of API for facilitating
%% install, setup, backup, restoration
%% @end

-module(app_lib).
-export([setup/0,
         reindex/0
        ]).

%% @doc Setup the application
%%
%% - set allow_mult on bucket type to false
%% - setup all the models
setup() ->
  BucketType = list_to_binary(boss_env:get_env(riaks2_rt,[])),
  Pid = riakdb:connect_to_db(),
  {ok, Proplist} = riakc_pb_socket:get_bucket_type(Pid, BucketType),

  %% check allow_mult
  case proplists:get_value(allow_mult, Proplist) of
    true ->
      io:format("First, change allow_multi to false on bucket type first by running this command by root:~n~n
      riak-admin bucket-type update ~p '{\"props\":{\"allow_mult\":false}}'~n~n", [list_to_atom(boss_env:get_env(riaks2_rt,[]))]);
    false ->
      %% setup models
 	 	  F = fun (Elem) ->
            io:format("<~p>~n", [Elem]),
            timer:sleep(1000),
			 		  boss_db_adapter_riaks2:setup_model(list_to_atom(Elem)),
            io:format("done!~n")
				 end,
		  ML = boss_web:get_all_models(),
		  lists:map(F, ML)
  end,
  riakdb:disconnect_db(Pid).

reindex() ->
  Pid = riakdb:connect_to_db(),
	F = fun (Elem) ->
				io:format("Boss model: ~p~n", [Elem]),
				boss_db_adapter_riaks2:reindex(list_to_atom(Elem))
		 end,
	lists:map(F, boss_web:get_all_models()),
  riakdb:disconnect_db(Pid).

