-module( cf_worker_sup ).

-behaviour( supervisor ).

-export( [start_link/0] ).
-export( [init/1] ).

-define( SERVER, ?MODULE ).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link( {local, ?SERVER}, ?MODULE, [] ).

start_tcp_session( Socket ) ->
  ChildSpec = #{
                id      => {cf_worker_tcpenv,
                            erlang:unique_integer( [positive, monotonic] )},
                start   => {cf_worker_tcpenv, start_link, [Socket]},
                restart => temporary,
                type    => worker
               },
  supervisor:start_child( ?SERVER, ChildSpec ).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init( [] ) ->
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================

start_link() ->
  supervisor:start_link( {local, ?SERVER}, ?MODULE, [] ).
