# cf_worker
###### A Cuneiform worker implementation.

[![hex.pm](https://img.shields.io/hexpm/v/cf_worker.svg?style=flat-square)](https://hex.pm/packages/cf_worker) [![Build Status](https://travis-ci.org/joergen7/cf_worker.svg?branch=master)](https://travis-ci.org/joergen7/cf_worker)

cf_worker is a worker implementation for the common runtime environment (CRE). The worker uses the Erlang foreign function interface (Effi) to execute tasks and interacts with the (distributed) filesystem via a Posix interface.

## Usage

### Adding the Cuneiform worker to a Project

Although the Cuneiform worker application can be imported also directly from GitHub, we recommend adding a dependency via [hex.pm](https://hex.pm). Here, we show how this can be done using the build tools [rebar3](https://www.rebar3.org) or mix.


#### rebar3

To integrate the Cuneiform worker application into a rebar3-managed project change the `deps` entry in your application's `rebar.config` file to include the tuple `{cf_worker, "0.1.0"}`.

```erlang
{deps, [{cf_worker, "0.1.0"}]}.
```


#### mix

```elixir
{:cf_worker, "~> 0.1.0"}
```


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