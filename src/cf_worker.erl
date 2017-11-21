-module( cf_worker ).
-behaviour( application ).


%%====================================================================
%% Exports
%%====================================================================

-export( [start/2, stop/1] ).
-export( [start/0, setup_env/2] ).
-export( [main/1] ).


%%====================================================================
%% API functions
%%====================================================================

start() ->
  application:start( ?MODULE ).


setup_env( CreNode, NWrk )
when is_atom( CreNode ),
     is_integer( NWrk ), NWrk > 0 ->

  ok = application:set_env( ?MODULE, cre_node, CreNode ),
  ok = application:set_env( ?MODULE, n_wrk, NWrk ).


%%====================================================================
%% Application callback functions
%%====================================================================

start( _StartType, _StartArgs ) ->

  NWrk =
    case application:get_env( ?MODULE, n_wrk ) of

      undefined ->
        case erlang:system_info( logical_processors_available ) of
          unknown -> 1;
          N       -> N
        end;

      {ok, N} ->
        N

    end,

  CreNode =
    case application:get_env( ?MODULE, cre_node ) of

      undefined ->
        node();
              
      {ok, C} ->
        C

    end,

  cf_worker_sup:start_link( CreNode, NWrk ).


stop( _State ) ->
  ok.


%%====================================================================
%% Escript main function
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
  ok = setup_env( CreNode, NWrk ),
  ok = start(),

  io:format( "connected nodes: ~p~n", [nodes()] ),
  io:format( "state:           ok~n" ),

  % wait indefinitely
  receive
    _ -> ok
  end.

