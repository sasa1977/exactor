# ExActor

[![Hex.pm](https://img.shields.io/hexpm/v/exactor.svg?style=flat-square)](https://hex.pm/packages/exactor)
[![Hex.pm](https://img.shields.io/hexpm/dt/exactor.svg?style=flat-square)](https://hex.pm/packages/exactor)

Simplifies implementation of `GenServer` based processes in Elixir.

`ExActor` helps removing the boilerplate that typically occurs when using `GenServer` behaviour. In particular, `ExActor` can be useful in following situations:

- `start` function just packs all arguments into a tuple which it forwards to `init/1` via `GenServer.start`.
- Calls and casts interface functions just forward all arguments to the server process via `GenServer.call` and `GenServer.cast`.
- Process is registered and all interface functions rely on this property.
- Some `handle_*` functions don't need the state.
- All handlers need to specify timeout or hibernate.
- More liberal grouping of handler functions (you don't need to group calls and casts separately)

For other cases, you may need to use plain `GenServer` functions (which can be used together with `ExActor` macros). `ExActor` is not meant to fully replace `GenServer`. It just tries to reduce boilerplate in most common cases.

If you're new to Elixir, Erlang, and OTP, and are not familiar on how `GenServer` works, I strongly suggest you learn about it first. It's really not that hard, and you can use [Elixir docs](http://elixir-lang.org/docs/stable/elixir/GenServer.html) as the starting point. It's also worth going through [Mix/OTP getting started guide](http://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).
Once you're familiar with `GenServer`, you can consider using `ExActor` to reduce the boilerplate.

Online documentation is available [here](http://hexdocs.pm/exactor).

The stable package is available on [hex](https://hex.pm/packages/exactor).

## Basic usage

Be sure to include a dependency in your `mix.exs`:

```elixir
deps: [{:exactor, "~> 2.2.3", warn_missing: false}, ...]
```

`ExActor` is a compile-time dependency only. No need to add it into the list of dependent applications. All code transformations are performed at compile time. If you're using exrm to build OTP releases, you may need to supply the `warn_missing: false` option to prevent warnings about a missing application dependency.


```elixir
defmodule Calculator do
  use ExActor.GenServer

  defstart start_link, do: initial_state(0)

  defcast inc(x), state: state, do: new_state(state + x)
  defcast dec(x), state: state, do: new_state(state - x)

  defcall get, state: state, do: reply(state)

  defcast stop, do: stop_server(:normal)
end
```

This module be used in a typical fashion:

```elixir
{:ok, calculator} = Calculator.start_link
Calculator.inc(calculator, 10)
Calculator.dec(calculator, 3)
Calculator.get(calculator)

Calculator.stop(calculator)
```

The module definition above is translated at compile-time into something like:

```elixir
defmodule Calculator do
  use GenServer

  def start_link, do: GenServer.start_link(__MODULE__, nil)
  def stop(pid), do: GenServer.cast(pid, :stop)

  def inc(pid, x), do: GenServer.cast(pid, {:inc, x})
  def dec(pid, x), do: GenServer.cast(pid, {:dec, x})
  def get(pid), do: GenServer.call(pid, :get)

  def init(_), do: {:ok, 0}

  def handle_cast({:inc, x}, state), do: {:noreply, state + x}
  def handle_cast({:dec, x}, state), do: {:noreply, state - x}
  def handle_cast(:stop, state), do: {:stop, :normal, state}

  def handle_call(:get, _, state), do: {:reply, state, state}
end
```

A bit more complex and feature rich example is presented [here](#a-more-involved-example).

## Predefines

To use `ExActor` macros, you must choose a predefine module and `use` it into your own module. A predefine is an `ExActor` module that provides some default implementations for `GenServer` callbacks.

Following predefines are currently provided:

* `ExActor.GenServer` - All `GenServer` callbacks are provided by `GenServer` from Elixir standard library.
* `ExActor.Strict` - All `GenServer` callbacks are provided. The default implementations for all except `code_change` and `terminate` will cause the server to be stopped.
* `ExActor.Tolerant` - All `GenServer` callbacks are provided. The default implementations ignore all messages without stopping the server.
* `ExActor.Empty` - No default implementation for `GenServer` callbacks are provided.

It is up to you to decide which predefine you want to use. See online docs for detailed description.
You can also build your own predefine. Refer to the source code of the existing ones as a template.

## Process registration

```elixir
defmodule Calculator do
  use ExActor.GenServer, export: :calculator

  # you can also use via, and global
  # use ExActor.GenServer, export: {:global, :calculator}
  # use ExActor.GenServer, export: {:via, :gproc, :calculator}

  ...
end

# all functions defined via defcall and defcast will take
# advantage of the export option
Calculator.start
Calculator.inc(5)
Calculator.get
```

## Handling of return values

```elixir
defstart start_link, do: initial_state(arg)

defcall foo, do: set_and_reply(new_state, response)
defcast bar, do: new_state(new_state)

defhandleinfo :stop, do: stop(normal)
defhandleinfo _, do: noreply
```

See [here](https://hexdocs.pm/exactor/ExActor.Responders.html#summary) for detailed list.

## Simplified initialization

```elixir
defstart start_link(x, y, z) do
  # Generates start_link function and `init/1` clause. The code runs in init/1 function.
  initial_state(x + y + z)
end
```

By default, corresponding `GenServer` function is deduced from the function name, so you can use either `start_link` or `start`. If you want a custom function name, you need to provide explicit `:link` option:

```elixir
defstart my_start(...), link: true do
  ...
end
```

### Dynamic start parameters

```elixir
defmodule Calculator do
  use ExActor.GenServer

  # gen_server_opts: :runtime will add additional argument to the start
  # function. This argument will be passed as options to the `GenServer` start
  # function.
  defstart start_link(x), gen_server_opts: :runtime, do: ...
end

# You can pass `name: :foo` due to `gen_server_opts: :runtime` option in the starter
Calculator.start_link(x, name: :foo)

# Or in the supervisor specification:
Supervisor.start_link(
  [
    worker(Calculator, [x, [name: :foo]]),
    # ...
  ]
)
```

## Cluster support

```elixir
defmodule Database do
  use ExActor.GenServer, export: :database

  defabcast store(key, value), do: ...
  defmulticall get(key), do: ...
end

# called on all nodes
Database.store(key, value)
Database.get(key)

# called on specified nodes
Database.store(some_nodes, key, value)
Database.get(some_nodes, key)
```

## Private interface functions

There are private versions available in form of `defstartp`, `defcallp`, `defcastp`, `defmulticallp`, and `defabcastp`. The only difference here is that interface functions are defined with `defp`. This can help you when you need to include some custom logic before or after the operation. See [here](#a-more-involved-example) for an example.

## Pattern matching

```elixir
defstart start_link(1), do:
defstart start_link(2), do:
defstart start_link(x), when: x < 5, do:

defcall a(1), do: ...
defcall a(2), do: ...
defcall a(x), state: 1, do: ...
defcall a(x), when: x > 1, do: ...
defcall a(_), do: ...

defhandleinfo :msg, state: {...}, when: ..., do: ...
```

All matches take place on both interface and handler functions.

Default arguments are also supported:

```elixir
defcall inc(x \\ 1), ...
```

In this case, we'll end up with two `inc` interface functions, and a single `handle_call` function that matches on `{:inc, x}`.

## Implementing just handlers

Can be useful do handle messages:

```elixir
defhandleinfo :some_message, do:
defhandleinfo :another_message, state: ..., do:
```

Or to pattern match on the state:

```elixir
# Body-less clause defines only the interface function
defcast inc

# Handle clauses pattern match on the state
defhandlecast inc, state: state, when: is_number(state),
  do: new_state(state + 1)

defhandlecast inc, do: new_state(0)
```

## Using from

```elixir
defcall my_request(...), from: from do
  ...
  spawn_link(fn ->
    ...
    GenServer.reply(from, ...)
  end)

  noreply
end
```

## Server-wide timeouts and hibernate

Timeout:

```elixir
defstart ... do
  # Instructs `ExActor` to include timeout in all responses made via responder
  # macros, such as `new_state` or `noreply`. As the result, a `:timeout` message
  # will be sent to the server after specified inactivity time.
  timeout_after(:timer.seconds(10))
end
```

Hibernation:

```elixir
defstart ... do
  # Instructs `ExActor` to include `:hibernate` in all responses made via responder
  # macros, such as `new_state` or `noreply`.
  hibernate
end
```

## Dynamic code generation friendliness

May be useful if you need to dynamically generate your requests. For example, if calls/casts simply delegate to some module, we could do something like:

```elixir
defmodule DynActor do
  use ExActor.GenServer

  for op <- [:op1, :op2] do
    defcall unquote(op), state: state do
      SomeModule.unquote(op)(state)
    end
  end
end
```

## A more involved example

In the following code, `ExActor` is used to implement a simple ETS based cache with basic cluster replication:

```elixir
defmodule Cache do
  use ExActor.GenServer

  # Starter allows clients to specify cache name. Notice how this is used
  # in `gen_server_opts` as a registered name of the server.
  defstart start(cache_name, timeout_after \\ :infinity),
    gen_server_opts: [name: cache_name]
  do
    # Specifies timeout which will be used in all handler responses
    timeout_after(timeout_after)
    :ets.new(cache_name, [:named_table, :set, :protected])
    initial_state(cache_name)
  end

  # Looks up the cache in the client process
  def get(cache_name, key) do
    case :ets.lookup(cache_name, key) do
      [{^key, value}] -> value
      [] -> nil
    end
  end

  # An example of a more complex interface function. A get attempt is made
  # in the client process, and then we optionally issue a private call request.
  def get_or_create(cache_name, key, fun) do
    case get(cache_name, key) do
      nil -> server_get_or_create(cache_name, key, fun)
      existing -> existing
    end
  end

  # Private call request used from `get_or_create`
  defcallp server_get_or_create(key, fun), state: cache_name do
    case get(cache_name, key) do
      nil ->
        new = fun.()
        store(cache_name, key, new)
        # Makes a distributed call to all other nodes
        set(Node.list, cache_name, key, new)
        new

      existing -> existing
    end
    |> reply
  end

  # Distributed setter - stores to all nodes in the cluster
  defmulticall set(key, value), state: cache_name do
    store(cache_name, key, value)
    reply(:ok)
  end

  defp store(cache_name, key, value) do
    :ets.insert(cache_name, {key, value})
  end

  # Stops the server on timeout message
  defhandleinfo :timeout, do: stop_server(:normal)
  defhandleinfo _, do: noreply
end
```
