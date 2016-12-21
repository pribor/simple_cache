%%% @doc Main module for simple_cache.
%%%
%%% Copyright 2013 Marcelo Gornstein &lt;marcelog@@gmail.com&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Marcelo Gornstein <marcelog@gmail.com>
%%% @author Marcelo Gornstein <marcelog@gmail.com>
%%%
-module(simple_cache).
-author('marcelog@gmail.com').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(DETS_TID, atom_to_list(?MODULE)).
-define(NAME(N), list_to_atom(?DETS_TID ++ "_" ++ atom_to_list(N))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([init/1]).
-export([get/4]).
-export([flush/1, flush/2]).
-export([close/1, delete/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Initializes a cache.
-spec init(atom()) -> {ok, term()}.
init(CacheName) ->
  RealName = ?NAME(CacheName),
  {ok, Ref} = dets:open_file(RealName, []),
  remove_expired(Ref),
%%  RealName = dets:new(RealName, [
%%    named_table, {read_concurrency, true}, public, {write_concurrency, true}
%%  ]),
  {ok, Ref}.

-spec remove_expired(atom()) -> non_neg_integer() | {'error', term()}.
remove_expired(CacheRef) ->
  Timestamp = timestamp(),
  dets:select_delete(CacheRef, [{{'_', '_', '$1'},[{'<', '$1', Timestamp}], [true]}]).

%% @doc Deletes the keys that match the given dets:matchspec() from the cache.
-spec flush(atom(), term()) -> true.
flush(CacheName, Key) ->
  RealName = ?NAME(CacheName),
  dets:delete(RealName, Key).

%% @doc Deletes all keys in the given cache.
-spec flush(atom()) -> true.
flush(CacheName) ->
  RealName = ?NAME(CacheName),
  true = dets:delete_all_objects(RealName).

%% @doc Tries to lookup Key in the cache, and execute the given FunResult
%% on a miss.
-spec get(atom(), infinity|pos_integer(), term(), function()) -> term().
get(CacheName, LifeTime, Key, FunResult) ->
  RealName = ?NAME(CacheName),
  case dets:lookup(RealName, Key) of
    [] ->
      % Not found, create it.
      V = FunResult(),
      ExpireTime = case LifeTime of
                     infinity -> infinity;
                     _ -> expire_timestamp(LifeTime)
                   end,
      dets:insert(RealName, {Key, V, ExpireTime}),
      LifeTime =/= infinity andalso erlang:send_after(
        LifeTime, simple_cache_expirer, {expire, CacheName, Key}
      ),
      V;
    [{Key, R, _}] -> R % Found, return the value.
  end.

-spec expire_timestamp(term()) -> term().
expire_timestamp(LifeTime) ->
  timestamp() + LifeTime.

-spec timestamp() -> non_neg_integer().
timestamp() ->
  os:system_time(millisecond).


-spec close(term()) -> ok | {error, term()}.
close(CacheName) ->
  dets:close(?NAME(CacheName)).

-spec delete(term()) -> ok | {error, term()}.
delete(CacheName) ->
  ok = close(CacheName),
  file:delete(?NAME(CacheName)).