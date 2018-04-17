# cf_worker
###### A Cuneiform worker implementation.

[![hex.pm](https://img.shields.io/hexpm/v/cf_worker.svg?style=flat-square)](https://hex.pm/packages/cf_worker) [![Build Status](https://travis-ci.org/joergen7/cf_worker.svg?branch=master)](https://travis-ci.org/joergen7/cf_worker)

cf_worker is a worker implementation for the common runtime environment (CRE). The worker uses the Erlang foreign function interface (Effi) to execute tasks and interacts with the (distributed) filesystem via a Posix interface.

## Usage

### Adding the Cuneiform worker to a Project

Although the Cuneiform worker application can be imported also directly from GitHub, we recommend adding a dependency via [hex.pm](https://hex.pm). Here, we show how this can be done using the build tools [rebar3](https://www.rebar3.org) or mix.

#### rebar3

To integrate the Cuneiform worker application into a rebar3-managed project change the `deps` entry in your application's `rebar.config` file to include the tuple `{cf_worker, "0.1.4"}`.

```erlang
{deps, [{cf_worker, "0.1.4"}]}.
```

#### mix

```elixir
{:cf_worker, "~> 0.1.4"}

```
### Compiling

Having rebar3 available on your system, compile the project as an Erlang project by entering

    rebar3 compile

If you want to drive the project from the command line please compile the project by entering

    rebar3 escriptize

### Starting the Cuneiform Worker

The Cuneiform worker application can be started in several different ways. It can be started from the command line, as an Erlang application, as a supervision tree hosting workers under a single supervisor, or directly as a process. In all cases there has to be a way for the workers to find the CRE instance and connect to it.

#### Starting from the Command Line

Compiling the Cuneiform client using `escriptize` creates an Erlang script file `cf_worker` which allows starting the Cuneiform client via the command line.

To display a help text enter

    ./cf_worker --help

This will show the command line synopsis, which looks like the following:

    Usage: cf_worker [-v] [-h] [-s <suppl_file>] [-c <cre_node>] [-n <n_wrk>]
                     [-w <wrk_dir>] [-r <repo_dir>] [-d <data_dir>]

      -v, --version     Show cf_worker version.
      -h, --help        Show command line options.
      -s, --suppl_file  Supplementary configuration file.
      -c, --cre_node    Erlang node running the CRE application (must be 
                        specified).
      -n, --n_wrk       Number of worker processes to start. 0 means 
                        auto-detect available processors.
      -w, --wrk_dir     Working directory in which workers store temporary 
                        files.
      -r, --repo_dir    Repository directory for intermediate and output data.
      -d, --data_dir    Data directory where input data is located.


To start the worker application from the command line and connect with a running CRE instance enter

    ./cf_worker -c cre@my_node

Here, we assume that the CRE runs on an Erlang node identified as `cre@my_node`.

#### Starting as an Erlang Application

If a CRE instance is already running on the same Erlang node you can start the Cuneiform worker application by calling

```erlang
cf_worker:start().
```

Which is exactly the same as calling

```erlang
application:start( cf_worker ).
```

#### Starting Under the Default Supervisor

To start the Cuneiform worker default supervisor under a custom supervision tree enter

```erlang
CreNode = node().
NWrk    = 4.
WrkDir  = "./_cuneiform/wrk".
RepoDir = "./_cuneiform/repo".
DataDir = "./".

cf_client_sup:start_link( CreNode, NWrk, WrkDir, RepoDir, DataDir ).
```

This starts a worker supervisor with four workers using `WrkDir` to store temporal data, `RepoDir` for intermediate and output data, and `DataDir` to look up input data. Also, we expect a CRE to be running on the same node.

#### Starting Directly

The Cuneiform client process can be started directly. There are several ways to do this. The first is to start the process with a function that allows it to locate the CRE:

```erlang
CreNode = node().
F = fun() -> cre:pid( CreNode ) end.

WrkDir  = "./_cuneiform/wrk".
RepoDir = "./_cuneiform/repo".
DataDir = "./".

{ok, WorkerPid} = cf_worker_process:start_link( F, WrkDir, RepoDir, DataDir ).
```

Giving a function instead of a plain CRE process identifier has the advantage, that if the CRE crashes, taking the Cuneiform worker with it, the restarted worker instance uses the output of the function, which offers the possibility of locating the CRE under its new process identifier.

If this is too tedious, one can start it giving the CRE process identifier directly:

```erlang
CrePid = cre:pid( node() ).

WrkDir  = "./_cuneiform/wrk".
RepoDir = "./_cuneiform/repo".
DataDir = "./".

{ok, WorkerPid} = cf_worker_process:start_link( CrePid ).
```

Both previous direct starting methods do not register the Cuneiform client with any registry service.

## System Requirements

- [Erlang](https://www.erlang.org) OTP 18.0 or higher
- [Rebar3](https://www.rebar3.org) 3.0.0 or higher

## Resources

- [joergen7/cre](https://github.com/joergen7/cre). A common runtime environment (CRE) for distributed workflow languages.
- [joergen7/cuneiform](https://github.com/joergen7/cuneiform). A functional language for large-scale data analysis whose distributed execution environment is implemented on top of the CRE.
- [joergen7/effi](https://github.com/joergen7/effi). Erlang foreign function interface.


## Authors

- Jörgen Brandt ([@joergen7](https://github.com/joergen7/)) [joergen.brandt@onlinehome.de](mailto:joergen.brandt@onlinehome.de)

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)