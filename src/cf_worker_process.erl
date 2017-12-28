%% -*- erlang -*-
%%
%% A common runtime environment (CRE) for distributed workflow languages.
%%
%% Copyright 2015-2017 Jörgen Brandt
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
%% @version 0.1.0
%% @copyright 2015-2017 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cf_worker_process ).
-behaviour( cre_worker ).


%%====================================================================
%% Exports
%%====================================================================

-export( [do_stagein/3, do_stageout/3, init/1, run/2, stagein_lst/2,
          stageout_lst/3, error_to_expr/3] ).

-export( [start_link/4] ).

-record( cf_worker_state, {wrk_dir, repo_dir, data_dir} ).

%%====================================================================
%% API functions
%%====================================================================

start_link( F, WrkDir, RepoDir, DataDir ) when is_function( F, 0 ) ->
  case F() of
    {ok, CreName} -> start_link( CreName, WrkDir, RepoDir, DataDir );
    {error, E}    -> {error, E}
  end;

start_link( CreName, WrkDir, RepoDir, DataDir ) ->
  cre_worker:start_link( CreName, ?MODULE, {WrkDir, RepoDir, DataDir} ).


%%====================================================================
%% CRE worker callback functions
%%====================================================================

-spec do_stagein( A :: _, F :: _, UsrInfo :: _ ) -> ok | {error, enoent}.

do_stagein( _A, _F, _UsrInfo ) ->
  io:format( "TODO: do_stagein~n" ),
  ok.


-spec do_stageout( A :: _, F :: _, UsrInfo :: _ ) -> ok | {error, enoent}.

do_stageout( _A, _F, _UsrInfo ) ->
  io:format( "TODO: do_stageout~n" ),
  ok.


-spec init( {WrkDir, RepoDir, DataDir} ) -> #cf_worker_state{}
when WrkDir  :: string(),
     RepoDir :: string(),
     DataDir :: string().

init( {WrkDir, RepoDir, DataDir} ) ->
  #cf_worker_state{ wrk_dir  = WrkDir,
                    repo_dir = RepoDir,
                    data_dir = DataDir }.


-spec run( A :: _, UsrInfo :: _ ) -> {ok, R :: _} | {error, Reason :: _}.

run( Request, CfWorkerState ) ->

  Dir = effi_wrk_dir( Request, CfWorkerState ),

  ok = filelib:ensure_dir( Dir++"/" ),

  Reply = effi:handle_request( Request, Dir ),
  
  case Reply of
    #{ result := #{ status := <<"ok">> } }    -> {ok, Reply};
    #{ result := #{ status := <<"error">> } } -> {error, Reply}
  end.


-spec stagein_lst( A :: _, UsrInfo :: _ ) -> [F :: _].

stagein_lst( _A, _UsrInfo ) ->
  io:format( "TODO: stagein_lst~n" ),
  [].


-spec stageout_lst( A :: _, R :: _, UsrInfo :: _ ) -> [F :: _].

stageout_lst( _A, _R, _UsrInfo ) ->
  io:format( "TODO: stageout_lst~n" ),
  [].

-spec error_to_expr( A, Reason, UsrInfo ) -> _
when A       :: _,
     Reason  :: {stagein | stageout, [_]} | {run, _},
     UsrInfo :: _.

error_to_expr( _A, Reason, _UsrInfo ) ->
  Reason.




effi_wrk_dir( #{ app_id := AppId }, #cf_worker_state{ wrk_dir = WorkDir } ) ->
  string:join( [WorkDir, binary_to_list( AppId )], "/" ).