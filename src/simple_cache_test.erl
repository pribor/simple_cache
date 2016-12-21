-module(simple_cache_test).
-include_lib("eunit/include/eunit.hrl").
-author("pribor").

%% API
-export([]).

-define(CACHE_NAME, test_cache).
-define(SETUP(F), {setup, fun setup/0, fun cleanup/1, F}).
-define(SETUP_SINGLE(Expr),
        begin
          setup(),
          try Expr of
            _ -> ok
          catch
            error:Error -> erlang:error(Error)
          after
            cleanup(undefined)
          end
        end).

setup() ->
  simple_cache_expirer:start_link(),
  simple_cache:init(?CACHE_NAME).

cleanup(_) ->
  simple_cache_expirer:stop(),
  simple_cache:delete(?CACHE_NAME).

insert_test() ->
  ?SETUP_SINGLE(
    begin
      V = simple_cache:get(?CACHE_NAME, infinity, k1, fun() -> 1 end),
      ?assertEqual(1, V)
    end).

expiry_test() ->
  ?SETUP_SINGLE(
    begin
      V1 = simple_cache:get(?CACHE_NAME, 10, k1, fun() -> 1 end),
      ?assertEqual(1, V1),
      timer:sleep(20),
      V2 = simple_cache:get(?CACHE_NAME, infinity, k1, fun() -> 2 end),
      ?assertEqual(2, V2)
    end
  ).

persistence_test() ->
  ?SETUP_SINGLE(
    begin
      V1 = simple_cache:get(?CACHE_NAME, infinity, k1, fun() -> 1 end),
      ?assertEqual(1, V1),
      ok = simple_cache:close(?CACHE_NAME),

      simple_cache:init(?CACHE_NAME),
      V2 = simple_cache:get(?CACHE_NAME, infinity, k1, fun() -> 2 end),
      ?assertEqual(1, V2)
    end).

remove_expired_test() ->
  ?SETUP_SINGLE(
    begin
      A1 = simple_cache:get(?CACHE_NAME, 100, k1, fun() -> 1 end),
      B1 = simple_cache:get(?CACHE_NAME, 2500, k2, fun() -> 1 end),
      C1 = simple_cache:get(?CACHE_NAME, infinity, k2, fun() -> 1 end),
      ?assertEqual(1, A1),
      ?assertEqual(1, B1),
      ?assertEqual(1, C1),
      simple_cache_expirer:stop(),
      ok = simple_cache:close(?CACHE_NAME),
      timer:sleep(150),

      setup(),
      A2 = simple_cache:get(?CACHE_NAME, infinity, k1, fun() -> 2 end),
      B2 = simple_cache:get(?CACHE_NAME, infinity, k2, fun() -> 2 end),
      C2 = simple_cache:get(?CACHE_NAME, infinity, k2, fun() -> 2 end),
      ?assertEqual(2, A2),
      ?assertEqual(1, B2),
      ?assertEqual(1, C2)
    end).

filename_test() ->
  Filename = "./test_cache",
  simple_cache:init(?CACHE_NAME, Filename),
  simple_cache:close(?CACHE_NAME),
  ?assertEqual(ok, file:delete(Filename)).

save_million_events_test_() ->
      {timeout, 300,
        ?_test(
          ?SETUP_SINGLE(
            begin
              lists:foreach(
                fun(X) -> simple_cache:get(?CACHE_NAME, 10000, X, fun() -> true end) end,
                lists:seq(1, 100000)
              ),
              timer:sleep(10000),
              simple_cache_expirer:stop(),
              ok = simple_cache:close(?CACHE_NAME),
              {ok, Ref} = setup(),
              Info = dets:info(Ref),
              error_logger:info_msg("Cache info: ~p~n", [Info])
            end
          )
        )
      }.

