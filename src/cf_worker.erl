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

-define( VSN, "0.1.0" ).
-define( BUILD, "2017-12-27" ).

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


  error_logger:info_report( [{info, "starting application"},
                             {application, cf_worker},
                             {node,        node()},
                             {n_wrk,       NWrk},
                             {cre_node,    CreNode},
                             {wrk_dir,    WrkDir},
                             {repo_dir,    RepoDir},
                             {data_dir,    DataDir}] ),

  cf_worker_sup:start_link( CreNode, NWrk, WrkDir, RepoDir, DataDir ).




stop( _State ) ->
  ok.


%%====================================================================
%% Escript main function
%%====================================================================

main( CmdLine ) ->

  try

    case getopt:parse( get_optspec_lst(), CmdLine ) of

      {error, Reason} ->
        error( Reason );

      {ok, {OptLst, []}} ->

        % break if version needs to be displayed
        case lists:member( version, OptLst ) of
          false -> ok;
          _     -> throw( version )
        end,

        % break if help needs to be displayed
        case lists:member( help, OptLst ) of
          false -> ok;
          _     -> throw( help )
        end,

        % extract supplement configuration file
        SupplFile =
          case lists:keyfind( conf, 1, OptLst ) of
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
            A                     -> error( {invalid_arg, A} )
          end,

        % set flag map
        ok = application:set_env( ?MODULE, flag_map, M2 ),

        % start worker application
        ok = start(),

        % wait indefinitely
        receive
          _ -> ok
        end;

      {ok, {_, L}} ->
        error( {bad_arg, L} )

    end

  catch
    throw:version -> print_version();
    throw:help    -> print_help()
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
   {n_wrk,      $n, "n_wrk",      integer,   "Number of worker processes to start. 0 means auto-detect available processors."}
  ].

print_help() ->
  getopt:usage( get_optspec_lst(), "cf_worker" ).

print_version() ->
  io:format( "application: cf_worker~nversion:     ~s~nbuild:       ~s~n",
             [?VSN, ?BUILD] ).