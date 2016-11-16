%% -*- erlang -*-
%%
%% Cuneiform: A Functional Language for Large Scale Scientific Data Analysis
%%
%% Copyright 2016 Jörgen Brandt, Marc Bux, and Ulf Leser
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% @author Jörgen Brandt <brandjoe@hu-berlin.de>

-module( cf_worker_tcpenv ).
-author( "Jörgen Brandt <brandjoe@hu-berlin.de>" ).

-behavior( gen_server ).
-behavior( cf_scheduler ).

-export( [start_link/1] ).

%%==========================================================
%% Record definitions
%%==========================================================

-record( state_data, { socket, session } ).

%%==========================================================
%% API functions
%%==========================================================


start_link( Socket ) ->
  gen_server:start_link( ?MODULE, Socket, [] ).

%%==========================================================
%% Generic server callback functions
%%==========================================================

%%==========================================================
%% Scheduler callback functions
%%==========================================================

reply( Reply, {?MODULE, Ref} ) ->
  gen_server:cast( Ref, Reply ).