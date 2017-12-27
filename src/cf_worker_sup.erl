-module( cf_worker_sup ).
-behaviour( supervisor ).


%%====================================================================
%% Exports
%%====================================================================

-export( [init/1] ).
-export( [start_link/2] ).


%%====================================================================
%% API functions
%%====================================================================

start_link( CreNode, NWrk )
when is_atom( CreNode ),
     is_integer( NWrk ), NWrk > 0 ->
  supervisor:start_link( ?MODULE, {CreNode, NWrk} ).

%%====================================================================
%% Supervisor callback functions
%%====================================================================

init( {CreNode, NWrk} )
when is_atom( CreNode ),
     is_integer( NWrk ), NWrk > 0 ->

  F =
    fun() ->    
      cre:pid( CreNode )
    end,

  Id =
    fun( I ) ->
      S = lists:flatten( io_lib:format( "wrk-~p", [I] ) ),
      list_to_atom( S )
    end,

  pong =
    case CreNode of
      'nonode@nohost' -> pong;
      _               -> net_adm:ping( CreNode )
    end,

  SupFlags = #{
               strategy  => one_for_one,
               intensity => 1,
               period    => 5
              },

  WorkerNodeSpec = #{
                     start    => {cf_worker_process,
                                  start_link,
                                  [F]},
                     restart  => permanent,
                     shutdown => 5000,
                     type     => worker,
                     modules  => [cf_worker_process]
                    },

  SpecLst = [WorkerNodeSpec#{ id => Id( I ) } || I <- lists:seq( 1, NWrk )],

  {ok, {SupFlags, SpecLst}}.

