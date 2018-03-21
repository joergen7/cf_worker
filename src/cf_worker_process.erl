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
%% @version 0.1.1
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
          stageout_lst/3, do_stageout/3, error_to_expr/3, cleanup_case/3] ).

-export( [start_link/4] ).


%%====================================================================
%% Record definitions
%%====================================================================

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
  
  Dir = get_work_dir( A, CfWorkerState ),
  
  ok =
    case delete_dir( Dir ) of
      ok -> ok;
      {error, enoent} -> ok
    end,

  ok = filelib:ensure_dir( Dir++"/" ).  


-spec stagein_lst( A :: _, UsrInfo :: _ ) -> [F :: _].

stagein_lst( A, _CfWorkerState ) ->

  #{ lambda := Lambda, arg_bind_lst := ArgBindLst } = A,
  #{ arg_type_lst := ArgTypeLst } = Lambda,

  get_stage_lst( ArgTypeLst, ArgBindLst ).


-spec do_stagein( A :: _, F :: _, UsrInfo :: _ ) -> ok | {error, enoent}.

do_stagein( A, F, CfWorkerState ) ->

  #cf_worker_state{ repo_dir = RepoDir,
                    data_dir = DataDir } = CfWorkerState,

  try

    RepoFile = string:join( [RepoDir, binary_to_list( F )], "/" ),
    case filelib:is_regular( RepoFile ) of
      true  -> throw( {stagein, RepoFile} );
      false -> ok
    end,

    DataFile = string:join( [DataDir, binary_to_list( F )], "/" ),
    case filelib:is_regular( DataFile ) of
      true  -> throw( {stagein, DataFile} );
      false -> ok
    end,

    {error, enoent}

  catch
    throw:{stagein, SrcFile} ->
      Dir = get_work_dir( A, CfWorkerState ),
      DestFile = string:join( [Dir, filename:basename( binary_to_list( F ) )], "/" ),
      {ok, _} = file:copy( SrcFile, DestFile ),
      ok
  end.


-spec run( A :: _, UsrInfo :: _ ) -> {ok, R :: _} | {error, Reason :: _}.

run( A, CfWorkerState ) ->

  Dir = get_work_dir( A, CfWorkerState ),
  Reply = effi:handle_request( A, Dir ),
  
  case Reply of
    #{ result := #{ status := <<"ok">> } }    -> {ok, Reply};
    #{ result := #{ status := <<"error">> } } -> {error, Reply}
  end.


-spec stageout_lst( A :: _, R :: _, UsrInfo :: _ ) -> [F :: _].

stageout_lst( A, R, _CfWorkerState ) ->

  #{ lambda := Lambda } = A,
  #{ ret_type_lst := RetTypeLst } = Lambda,
  #{ result := Result } = R,
  #{ ret_bind_lst := RetBindLst } = Result,

  get_stage_lst( RetTypeLst, RetBindLst ).


-spec do_stageout( A :: _, F :: _, UsrInfo :: _ ) -> ok | {error, enoent}.

do_stageout( A, F, CfWorkerState ) ->

  #{ app_id := AppId } = A,
  #cf_worker_state{ repo_dir = RepoDir } = CfWorkerState,

  F1 = binary_to_list( update_value( filename:basename( F ), AppId ) ),
  DestFile = string:join( [RepoDir, F1], "/" ),

  Dir = get_work_dir( A, CfWorkerState ),
  SrcFile = string:join( [Dir, binary_to_list( F )], "/" ),

  case filelib:is_regular( SrcFile ) of

    true ->
      ok = filelib:ensure_dir( DestFile ),
      {ok, _} = file:copy( SrcFile, DestFile ),
      ok;

    false ->
      {error, enoent}

  end.



-spec error_to_expr( A, Reason, UsrInfo ) -> _
when A       :: #{ atom() => _ },
     Reason  :: {stagein | stageout, [binary()]} | {run, _},
     UsrInfo :: _.

error_to_expr( #{ app_id := AppId }, {stagein, FileLst}, _UsrInfo ) ->
  #{ app_id => AppId,
     result => #{ status   => <<"error">>,
                  stage    => <<"stagein">>,
                  file_lst => FileLst } };

error_to_expr( #{ app_id := AppId }, {stageout, FileLst}, _UsrInfo ) ->
  #{ app_id => AppId,
     result => #{ status   => <<"error">>,
                  stage    => <<"stageout">>,
                  file_lst => FileLst } };

error_to_expr( _A, {run, Reason}, _UsrInfo ) ->
  Reason.


-spec cleanup_case( A, R, CfWorkerState ) -> R1 :: #{ atom() => _ }
when A             :: #{ atom() => _ },
     R             :: #{ atom() => _ },
     CfWorkerState :: #cf_worker_state{}.

cleanup_case( A, R, CfWorkerState ) ->

  % delete current working directory
  Dir = get_work_dir( A, CfWorkerState ),
  ok = delete_dir( Dir ),

  #{ lambda := Lambda } = A,
  #{ ret_type_lst := RetTypeLst } = Lambda,
  #{ app_id := AppId,
     result := Result } = R,
  #{ status := Status } = Result,

  % update return value
  case Status of

    <<"ok">> ->
      #{ ret_bind_lst := RetBindLst } = Result,
      RetBindLst1 = update_ret_bind_lst( RetTypeLst, RetBindLst, AppId ),
      R#{ result => Result#{ ret_bind_lst => RetBindLst1 } };

    _ ->
      R

  end.


%%====================================================================
%% Internal functions
%%====================================================================

-spec get_work_dir( A, CfWorkerState ) -> string()
when A             :: #{ atom() => _ },
     CfWorkerState :: #cf_worker_state{}.

get_work_dir( #{ app_id := AppId }, #cf_worker_state{ wrk_dir = WorkDir } )
when is_binary( AppId ),
     is_list( WorkDir ) ->

  string:join( [WorkDir, binary_to_list( AppId )], "/" ).


-spec delete_dir( Dir :: string() ) -> ok | {error, _}.

delete_dir( Dir )
when is_list( Dir )->

  F =
    fun( F ) when is_list( F ) ->

      File = string:join( [Dir, F], "/" ),

      case filelib:is_dir( File ) of

        true  ->
          ok = delete_dir( File );

        false ->
          ok = file:delete( File )

      end
    end,

  case file:list_dir( Dir ) of

    {ok, FileLst} ->
      ok = lists:foreach( F, FileLst ),
      ok = file:del_dir( Dir );

    {error, Reason} ->
      {error, Reason}

  end.


-spec get_stage_lst( TypeLst, BindLst ) -> [string()]
when TypeLst :: [#{ atom() => _ }],
     BindLst :: [#{ atom() => _ }].

get_stage_lst( TypeLst, BindLst ) ->

  F =
    fun( Binding, Acc ) ->

      #{ arg_name := ArgName,
         value    := Value } = Binding,

      TypeInfo = effi:get_type_info( ArgName, TypeLst ),

      #{ arg_type := ArgType,
         is_list  := IsList } = TypeInfo,

      case ArgType of

        <<"File">> ->
          case IsList of
            false -> [Value|Acc];
            true  -> Value++Acc
          end;

        _ ->
          Acc

      end

    end,

  lists:foldl( F, [], BindLst ).


-spec update_ret_bind_lst( RetTypeLst, RetBindLst, AppId ) ->
        [#{ atom() => _ }]
when RetTypeLst :: [#{ atom() => _ }],
     RetBindLst :: [#{ atom() => _ }],
     AppId      :: binary().

update_ret_bind_lst( RetTypeLst, RetBindLst, AppId )
when is_list( RetTypeLst ),
     is_list( RetBindLst ),
     is_binary( AppId ) ->

  F =
    fun( RetBinding ) ->

      #{ arg_name := ArgName,
         value    := Value } = RetBinding,

      TypeInfo = effi:get_type_info( ArgName, RetTypeLst ),

      #{ arg_type := ArgType,
         is_list  := IsList } = TypeInfo,

      case ArgType of

        <<"File">> ->
          case IsList of

            false ->
              RetBinding#{ value := update_value( Value, AppId ) };

            true ->
              RetBinding#{ value := [update_value( B, AppId ) || B <- Value] }

          end;

        _ ->
          RetBinding

      end

    end,

  [F( Binding ) || Binding <- RetBindLst].


-spec update_value( Value, AppId ) -> binary()
when Value :: binary(),
     AppId :: binary().

update_value( Value, AppId )
when is_binary( Value ),
     is_binary( AppId ) ->

  Skip = byte_size( AppId )-7,
  <<_:Skip/binary, B/binary>> = AppId,
  <<B/binary, "_", Value/binary>>.
