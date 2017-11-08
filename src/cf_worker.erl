-module( cf_worker ).
-behaviour( application ).


%%====================================================================
%% Exports
%%====================================================================

-export( [start/2, stop/1] ).
-export( [start/0, setup_app/2] ).
-export( [main/1] ).


%%====================================================================
%% API functions
%%====================================================================


start() ->

  CreNode = node(),

  NWrk =
    case erlang:system_info( logical_processors_available ) of
      unknown -> 1;
      N       -> N
    end,

  setup_app( CreNode, NWrk ).


setup_app( CreNode, NWrk )
when is_atom( CreNode ),
     is_integer( NWrk ), NWrk > 0 ->

  application:set_env( cf_worker, cre_node, CreNode ),
  application:set_env( cf_worker, n_wrk, NWrk ),
  application:start( cf_worker ).


%%====================================================================
%% Application callback functions
%%====================================================================

start( _StartType, _StartArgs ) ->

  case application:get_env( cf_worker, n_wrk ) of

    undefined ->
      {error, {env_var_undefined, n_wrk}};

    {ok, NWrk} ->
      case application:get_env( cf_worker, cre_node ) of

        undefined ->
          {error, {env_var_undefined, cre_node}};

        {ok, CreNode} ->
          cf_worker_sup:start_link( CreNode, NWrk )

      end
  end.


stop( _State ) ->
  ok.


%%====================================================================
%% Escript main functions
%%====================================================================

main( [CreNodeStr] )
when is_list( CreNodeStr ) ->

  CreNode = list_to_atom( CreNodeStr ),

  io:format( "application:     cf_worker~nnode name:       ~p~n", [node()] ),

  NWrk =
    case erlang:system_info( logical_processors_available ) of
      unknown -> 1;
      N       -> N
    end,

  io:format( "available processors: ~p~n", [NWrk] ),

  % start worker application
  ok = setup_app( CreNode, NWrk ),

  io:format( "connected nodes: ~p~n", [nodes()] ),
  io:format( "state:           ok~n" ),

  % wait indefinitely
  receive
    _ -> ok
  end.

