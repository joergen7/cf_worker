%% -*- erlang -*-
%%
%% A Cuneiform worker implementation.
%%
%% Copyright 2015-2018 Jörgen Brandt
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.5
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cf_worker_sup ).
-behaviour( supervisor ).


%%====================================================================
%% Exports
%%====================================================================

-export( [init/1] ).
-export( [start_link/5] ).


%%====================================================================
%% API functions
%%====================================================================

start_link( CreNode, NWrk, WrkDir, RepoDir, DataDir ) ->

  supervisor:start_link( {local, cf_worker_sup},
                         ?MODULE,
                         {CreNode, NWrk, WrkDir, RepoDir, DataDir} ).

%%====================================================================
%% Supervisor callback functions
%%====================================================================

init( {CreNode, NWrk, WrkDir, RepoDir, DataDir} )
when is_atom( CreNode ),
     is_integer( NWrk ), NWrk > 0,
     is_list( WrkDir ),
     is_list( RepoDir ),
     is_list( DataDir ) ->

  F =
    fun() ->    
      cre:pid( CreNode )
    end,

  Id =
    fun( I ) ->
      S = lists:flatten( io_lib:format( "wrk-~p", [I] ) ),
      list_to_atom( S )
    end,

  % connect with CRE
  pong =
    case CreNode of
      'nonode@nohost' -> pong;
      _               -> net_adm:ping( CreNode )
    end,

  SupFlags = #{
               strategy  => one_for_one,
               intensity => 0,
               period    => 5
              },

  WorkerNodeSpec = #{
                     start    => {cf_worker_process,
                                  start_link,
                                  [F, WrkDir, RepoDir, DataDir]},
                     restart  => permanent,
                     shutdown => 5000,
                     type     => worker,
                     modules  => [cf_worker_process]
                    },

  SpecLst = [WorkerNodeSpec#{ id => Id( I ) } || I <- lists:seq( 1, NWrk )],

  {ok, {SupFlags, SpecLst}}.

