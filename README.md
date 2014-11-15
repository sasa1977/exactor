# ExActor

Simplified implementation and usage of `gen_server` based actors in Elixir.
This library is inspired by (though not depending on) [GenX](https://github.com/yrashk/genx), but in addition, removes some more boilerplate, and changes some semantics of the handle_call/cast responses.

If you're new to Erlang, and are not familiar on how gen_server works, I strongly suggest you learn about it first. It's really not that hard, and you can use [Elixir docs](http://elixir-lang.org/docs/stable/elixir/GenServer.html) as the starting point. Once you're familiar with gen_server, you can use ExActor to make your actors (gen_servers) more compact.

Status: I use it in production.

Online documentation is available [here](http://hexdocs.pm/exactor).

The stable package is also available on [hex](https://hex.pm/packages/exactor).

## Basic usage

Be sure to include a dependency in your `mix.exs`:

```elixir
deps: [{:exactor, "~> 2.0.0"}, ...]
```


```elixir
defmodule Calculator do
  use ExActor.GenServer

  defstart start_link do
    initial_state(0)
  end

  defcast inc(x), state: state, do: new_state(state + x)
  defcast dec(x), state: state, do: new_state(state - x)

  defcall get, state: state, do: reply(state)
end

{:ok, calculator} = Calculator.start_link
Calculator.inc(calculator, 10)
Calculator.dec(calculator, 3)
Calculator.get(calculator)
# 2
```

## Predefines

A predefine is an ExActor mixin that provides some default implementations for
`gen_server` callbacks. Following predefines are currently provided:

* `ExActor.GenServer` - All `gen_server` callbacks are provided by GenServer from Elixir standard library.
* `ExActor.Strict` - All `gen_server` callbacks are provided. The default implementations for all except `code_change` and `terminate` will cause the server to be stopped.
* `ExActor.Tolerant` - All `gen_server` callbacks are provided. The default implementations ignore all messages without stopping the server.
* `ExActor.Empty` - No default implementation for `gen_server` callbacks are provided.

It is up to you to decide which predefine you want to use. See online docs for detailed description.
You can also build your own predefine. Refer to the source code of the existing ones as a template.

## Singleton actors

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
defstart start_link do: initial_state(arg)          # sets initial state
defstart start_link do: {:ok, arg}                  # standard gen_server response

defcall a, state: state, do: reply(response)        # responds but doesn't change state
defcall b, do: set_and_reply(new_state, response)   # responds and changes state
defcall c, do: {:reply, response, new_state}        # standard gen_server response

defcast c, do: noreply                              # doesn't change state
defcast d, do: new_state(new_state)                 # sets new state
defcast f, do: {:noreply, new_state}                # standard gen_server response

defhandleinfo c, do: noreply                        # doesn't change state
defhandleinfo d, do: new_state(new_state)           # sets new state
defhandleinfo f, do: {:noreply, new_state}          # standard gen_server response
```

## Simplified initialization

```elixir
defstart start_link(x, y, z) do
  # Generates start_link function and `init/1` clause.

  # The code runs in init/1
  initial_state(x + y + z)
end
```

By default, corresponding `GenServer` function is mapped from the function name, so you can use either `start_link` or `start`. If you want a custom function name, you need to provide explicit `:link` option:

```elixir
defstart my_start(...), link: true, do
  ...
end
```

### Dynamic start parameters

```elixir
defmodule Calculator do
  use ExActor.GenServer

  defstart start_link(x), gen_server_opts: :runtime, do: ...
end

# gen_server_opts: :runtime will add additional argument to the start
# function. This argument will be passed as options to the `GenServer` start
# function.
Calculator.start_link(x, name: :foo)
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

## Private wrappers

There are private versions available in form of `defstartp`, `defcallp`, `defcastp`, `defmulticallp`, and `defabcastp`. The only difference here is that interface functions are defined with `defp`. This can help you when you need to include some custom logic before or after the operation:

```elixir
defmodule Cache do
  use ExActor.GenServer, export: :cache

  defstart start_link do
    :ets.new(:cache, [:protected, :named_table, :set])
    initial_state(nil)
  end

  # Custom wrapper around internal `maybe_create` request.
  def get_or_create(key, fun) do
    case value(key) do
      nil -> maybe_create(key, fun)
      existing -> existing
    end
  end

  # Private request
  defcallp maybe_create(key, fun) do
    case value(key) do
      nil ->
        value = fun.()
        :ets.insert(:cache, {key, value})
        value

      existing -> existing
    end
    |> reply
  end

  defp value(key) do
    case :ets.lookup(:cache, key) do
      [{^key, value}] -> value
      _ -> nil
    end
  end
end

Cache.start_link
Cache.get_or_create(:foo, fn -> 1 end)  # 1
Cache.get_or_create(:foo, fn -> 2 end)  # 1
```


## Handling messages

```elixir
defhandleinfo :some_message, do:
defhandleinfo :another_message, state: ..., do:
```

## Pattern matching

```elixir
defstart start_link(1), do:
defstart start_link(2), do:
defstart start_link(x), when: x < 5, do:

defcall a(1), do: ...
defcall a(2), do: ...
defcall a(x), state: 1, do: ...
defcall a(x), when: x > 1, do: ...
defcall a(x), state: state, when: state > 1, do: ...
defcall a(_), do: ...

defhandleinfo :msg, state: {...}, when: ..., do: ...
```

Note: all start/call/cast matches take place at the `handle_*` callbacks. Generated interface functions simply pass the arguments to appropriate `gen_server` function. Consequently, if a match fails, the server will crash.

If you want to match on interface functions, you could define a single private wrapper, and then plain exported functions which delegate to it:

```elixir
def my_request(...), do: do_my_request(...)
def my_request(...), do: do_my_request(...)
...

defcallp do_my_request(...), do: ...
```


## Using from

```
defcall my_request(...), from: from do
  ...
  spawn_link(fn ->
    ...
    GenServer.reply(from, ...)
  end)

  noreply
end
```

## Runtime friendliness

May be useful if calls/casts simply delegate to some module/functions.

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


## Simplified data abstraction delegation

Macro `delegate_to` is provided to shorten the definition when the state is implemented as a functional data abstraction, and operations simply delegate to that module. Here's an example:

```elixir
defmodule HashDictActor do
  use ExActor.GenServer
  import ExActor.Delegator

  defstart start_link, do: initial_state(HashDict.new)

  delegate_to HashDict do
    query get/2
    trans put/3
  end
end

{:ok, pid} = HashDictActor.start_link
HashDictActor.put(pid, 1, 2)
HashDictActor.get(pid, 1)
```

This is equivalent of:

```elixir
defmodule HashDictActor do
  use ExActor.GenServer

  defstart start_link, do: initial_state(HashDict.new)

  defcall get(k), state: state do
    HashDict.get(state, k)
  end

  defcast put(k, v), state:state do
    HashDict.put(state, k, v)
    |> new_state
  end
end
```

You can freely mix `delegate_to` with other macros, such as `defcall`, `defcast`, and others.