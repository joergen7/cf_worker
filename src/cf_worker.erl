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
%% @version 0.1.2
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cf_worker ).
-behaviour( application ).


%%====================================================================
%% Exports
%%====================================================================

-export( [start/2, stop/1] ).
-export( [start/0] ).
-export( [main/1] ).


%%====================================================================
%% Definitions
%%====================================================================

-define( VSN, "0.1.2" ).

%%====================================================================
%% API functions
%%====================================================================

start() ->
  application:start( ?MODULE ).


%%====================================================================
%% Application callback functions
%%====================================================================

start( _StartType, _StartArgs ) ->

  {ok, DefaultMap} = application:get_env( ?MODULE, default_map ),
  {ok, GlobalFile} = application:get_env( ?MODULE, global_file ),
  {ok, UserFile}   = application:get_env( ?MODULE, user_file ),

  SupplFile =
    case application:get_env( ?MODULE, suppl_file ) of
      {ok, S}   -> S;
      undefined -> undefined
    end,

  FlagMap =
    case application:get_env( ?MODULE, flag_map ) of
      {ok, M}   -> M;
      undefined -> #{}
    end,

  ConfMap = lib_conf:create_conf( DefaultMap, GlobalFile, UserFile, SupplFile,
                                  FlagMap ),

  NWrk =
    case maps:get( n_wrk, ConfMap ) of

      <<"auto">> ->
        case erlang:system_info( logical_processors_available ) of
          unknown -> 1;
          N       -> N
        end;

      N when is_integer( N ), N > 0 ->
        N

    end,

  CreNode =
    case maps:get( cre_node, ConfMap ) of

      <<"node">> ->
        node();
              
      B when is_binary( B ) ->
        binary_to_atom( B, utf8 )

    end,

  WrkDir = binary_to_list( maps:get( wrk_dir, ConfMap ) ),
  RepoDir = binary_to_list( maps:get( repo_dir, ConfMap ) ),
  DataDir = binary_to_list( maps:get( data_dir, ConfMap ) ),


  error_logger:info_report( [{info, "starting Cuneiform worker pool"},
                             {application, cf_worker},
                             {vsn,         ?VSN},
                             {node,        node()},
                             {n_wrk,       NWrk},
                             {cre_node,    CreNode},
                             {wrk_dir,     WrkDir},
                             {repo_dir,    RepoDir},
                             {data_dir,    DataDir}] ),

  cf_worker_sup:start_link( CreNode, NWrk, WrkDir, RepoDir, DataDir ).




stop( _State ) ->
  ok.


%%====================================================================
%% Escript main function
%%====================================================================

main( Args ) ->

  try

    case getopt:parse( get_optspec_lst(), Args ) of

      {error, R1} ->
        throw( {error, R1} );

      {ok, {OptLst, []}} ->

        % break if version needs to be displayed
        case lists:member( version, OptLst ) of
          false -> ok;
          true  -> throw( version )
        end,

        % break if help needs to be displayed
        case lists:member( help, OptLst ) of
          false -> ok;
          true  -> throw( help )
        end,

        % extract supplement configuration file
        SupplFile =
          case lists:keyfind( suppl_file, 1, OptLst ) of
            false            -> undefined;
            {suppl_file, S1} -> S1
          end,

        % set supplement file
        ok = application:set_env( ?MODULE, suppl_file, SupplFile ),

        % extract CRE node name
        M1 =
          case lists:keyfind( cre_node, 1, OptLst ) of
            false               -> #{};
            {cre_node, CreNode} -> #{ cre_node => CreNode }
          end,

        % extract number of workers
        M2 =
          case lists:keyfind( n_wrk, 1, OptLst ) of
            false                 -> M1;
            {n_wrk, 0}            -> M1#{ n_wrk => <<"auto">> };
            {n_wrk, N} when N > 0 -> M1#{ n_wrk => N };
            A                     -> throw( {error, {invalid_arg, A}} )
          end,

        % extract working directory
        M3 =
          case lists:keyfind( wrk_dir, 1, OptLst ) of
            false        -> M2;
            {wrk_dir, W} -> M2#{ wrk_dir => W }
          end,

        % extract repository directory
        M4 =
          case lists:keyfind( repo_dir, 1, OptLst ) of
            false         -> M3;
            {repo_dir, R} -> M3#{ repo_dir => R }
          end,

        % extract data directory
        M5 =
          case lists:keyfind( data_dir, 1, OptLst ) of
            false         -> M4;
            {data_dir, D} -> M4#{ data_dir => D }
          end,

        % set flag map
        ok = application:set_env( ?MODULE, flag_map, M5 ),

        % start worker application
        ok =
          case start() of

            ok ->
              ok;

            {error, {{shutdown, {failed_to_start_child, _, R2}}, _}} ->
              throw( {error, R2} );

            {error, R2} ->
              throw( {error, R2} )

          end,

        % attach escript process
        MonitorRef = monitor( process, cf_worker_sup ),

        % wait indefinitely
        receive
          {'DOWN', MonitorRef, process, _Object, _Info} ->
            ok
        end;

      {ok, {_, L}} ->
        throw( {error, {bad_arg, L}} )

    end

  catch

    throw:version ->
      ok = print_version();

    throw:help ->
      ok = print_help();

    throw:{error, Reason} ->
      ok = io:format( "~n~p~n", [Reason] )

  after
    ok = timer:sleep( 1000 )
  end.


%%====================================================================
%% Internal functions
%%====================================================================

get_optspec_lst() ->
  [
   {version,    $v, "version",    undefined, "Show cf_worker version."},
   {help,       $h, "help",       undefined, "Show command line options."},
   {suppl_file, $s, "suppl_file", binary,    "Supplementary configuration file."},
   {cre_node,   $c, "cre_node",   binary,    "Erlang node running the CRE application (must be specified)."},
   {n_wrk,      $n, "n_wrk",      integer,   "Number of worker processes to start. 0 means auto-detect available processors."},
   {wrk_dir,    $w, "wrk_dir",    binary,    "Working directory in which workers store temporary files."},
   {repo_dir,   $r, "repo_dir",   binary,    "Repository directory for intermediate and output data."},
   {data_dir,   $d, "data_dir",   binary,    "Data directory where input data is located."}
  ].

print_help() ->
  getopt:usage( get_optspec_lst(), "cf_worker" ).

print_version() ->
  io:format( "cf_worker ~s~n", [?VSN] ).