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
%% @version 0.1.0
%% @copyright 2015-2018 Jörgen Brandt
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

-export( [init/1, prepare_case/2, stagein_lst/2, do_stagein/3, run/2, 
          stageout_lst/3, do_stageout/3, error_to_expr/3, cleanup_case/2] ).

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

-spec init( {WrkDir, RepoDir, DataDir} ) -> #cf_worker_state{}
when WrkDir  :: string(),
     RepoDir :: string(),
     DataDir :: string().

init( {WrkDir, RepoDir, DataDir} ) ->
  #cf_worker_state{ wrk_dir  = WrkDir,
                    repo_dir = RepoDir,
                    data_dir = DataDir }.


-spec prepare_case( A :: _, CfWorkerState :: _ ) -> ok.

prepare_case( A, CfWorkerState ) ->

  error_logger:info_report(
    [{info, "receiving request"},
     {application, cf_worker},
     {cf_worker_process_pid, self()},
     {app, A}] ),

  Dir = effi_wrk_dir( A, CfWorkerState ),
  ok = filelib:ensure_dir( Dir++"/" ).  


-spec stagein_lst( A :: _, UsrInfo :: _ ) -> [F :: _].

stagein_lst( _A, _UsrInfo ) ->
  ok = error_logger:warning_msg( "TODO: stagein_lst" ),
  [].


-spec do_stagein( A :: _, F :: _, UsrInfo :: _ ) -> ok | {error, enoent}.

do_stagein( _A, _F, _UsrInfo ) ->
  ok = error_logger:warning_msg( "TODO: do_stagein" ),
  ok.


-spec run( A :: _, UsrInfo :: _ ) -> {ok, R :: _} | {error, Reason :: _}.

run( A, CfWorkerState ) ->

  Dir = effi_wrk_dir( A, CfWorkerState ),
  Reply = effi:handle_request( A, Dir ),
  
  case Reply of
    #{ result := #{ status := <<"ok">> } }    -> {ok, Reply};
    #{ result := #{ status := <<"error">> } } -> {error, Reply}
  end.


-spec stageout_lst( A :: _, R :: _, UsrInfo :: _ ) -> [F :: _].

stageout_lst( _A, _R, _UsrInfo ) ->
  ok = error_logger:warning_msg( "TODO: stageout_lst" ),
  [].


-spec do_stageout( A :: _, F :: _, UsrInfo :: _ ) -> ok | {error, enoent}.

do_stageout( _A, _F, _UsrInfo ) ->
  ok = error_logger:warning_msg( "TODO: do_stageout" ),
  ok.


-spec error_to_expr( A, Reason, UsrInfo ) -> _
when A       :: _,
     Reason  :: {stagein | stageout, [_]} | {run, _},
     UsrInfo :: _.

error_to_expr( _A, {run, Reason}, _UsrInfo ) ->
  Reason.


-spec cleanup_case( A :: _, CfWorkerState :: _ ) -> ok.

cleanup_case( A, CfWorkerState ) ->

  #{ app_id := AppId } = A,

  ok =
    error_logger:info_report(
      [{info, "sending reply"},
       {application, cf_worker},
       {cf_worker_process_pid, self()},
       {app_id, AppId}] ),
  
  Dir = effi_wrk_dir( A, CfWorkerState ),
  ok = delete_dir( Dir ).


%%====================================================================
%% Internal functions
%%====================================================================

-spec effi_wrk_dir( A, CfWorkerState ) -> string()
when A             :: #{ app_id => binary() },
     CfWorkerState :: #cf_worker_state{}.

effi_wrk_dir( #{ app_id := AppId }, #cf_worker_state{ wrk_dir = WorkDir } )
when is_binary( AppId ),
     is_list( WorkDir ) ->

  string:join( [WorkDir, binary_to_list( AppId )], "/" ).


-spec delete_dir( Dir :: string() ) -> ok.

delete_dir( Dir )
when is_list( Dir )->

  F =
    fun( File ) when is_list( File ) ->
      case filelib:is_dir( File ) of

        true  ->
          ok = delete_dir( File );

        false ->
          ok = file:delete( File )

      end
    end,

  {ok, FileLst} = file:list_dir( Dir ),
  ok = lists:foreach( F, FileLst ),
  ok = file:del_dir( Dir ).