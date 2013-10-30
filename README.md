# ExActor

Simplified implementation and usage of `gen_server` based actors in Elixir.
This library is inspired by (though not depending on) [GenX](https://github.com/yrashk/genx), but in addition, removes some more boilerplate, and changes some semantics of the handle_call/cast responses.

If you're new to Erlang, and are not familiar on how gen_server works, I strongly suggest you learn about it first. It's really not that hard, and you can use [Elixir docs](http://elixir-lang.org/docs/stable/GenServer.Behaviour.html) as the starting point. Once you're familiar with gen_server, you can use ExActor to make your actors (gen_servers) more compact.

Status: I use it in production.

## Basic usage

```elixir
defmodule Actor do
  use ExActor
  
  defcast inc(x), state: state, do: new_state(state + x)
  defcall get, state: state, do: state
end

# initial state is set to start argument
{:ok, act} = Actor.start(1)
Actor.get(act)         # 1

Actor.inc(act, 2)
Actor.get(act)         # 3
```

## Singleton actors

```elixir
defmodule SingletonActor do
  # The actor process will be locally registered under an alias
  # given via export option
  use ExActor, export: :some_registered_name

  defcall get, state: state, do: state
  defcast set(x), do: new_state(x)
end

SingletonActor.start
SingletonActor.set(5)
SingletonActor.get
```

## Handling of return values

```elixir
defcall a, state: state, do: 5                # responds 5, doesn't change state
defcall b, do: set_and_reply(6, 5)            # responds 5, sets new state to 6
defcall c, do: {:reply, response, new_state}  # standard gen_server response is left intact

defcast c, do: :ok                            # ignores response, doesn't change state
defcast d, do: new_state(new_state)           # sets new state
defcast f, do: {:noreply, new_state}          # standard gen_server response is left intact

def init(arg), do: initial_state(arg)         # sets initial state
def init(arg), do: {:ok, arg}                 # standard gen_server response    
```
    
## Simplified starting
    
```elixir
Actor.start         # same as Actor.start(nil)
Actor.start(init_arg)
Actor.start(init_arg, options)

Actor.start_link
Actor.start_link(init_arg)
Actor.start_link(init_arg, options)
```

## Simplified initialization

```elixir
# define initial state
use ExActor, initial_state: HashDict.new

# alternatively as the function
definit do: HashSet.new

# using the input argument
definit x do
  x + 1
end
```

## Handling messages

```elixir
definfo :some_message do
end

definfo :another_message, state: ... do
end
```

## Pattern matching

```elixir
defcall a(1), do: ...
defcall a(2), do: ...
defcall a(x), state: 1, do: ...
defcall a(x), when: x > 1, do: ...
defcall a(x), state: state, when: state > 1, do: ...
defcall a(_), do: ...

definit :something, do: ...
definit x, when: ..., do: ...

definfo :msg, state: {...}, when: ..., do: ...
```

Note: all call/cast matches take place at the `handle_call` or `handle_cast` level. The interface function simply passes the arguments to appropriate `gen_server` function. Consequently, if a match fails, the server will crash.

## Skipping interface funs

```elixir
# interface fun will not be generated, just handle_call clause
defcall unexported, export: false, do: :unexported
```

## Using from

```
defcall a(...), from: {from_pid, ref} do
  ...
end
```

## Runtime friendliness

May be useful if calls/casts simply delegate to some module/functions.

```elixir
defmodule DynActor do
  use ExActor

  lc op inlist [:op1, :op2] do
    defcall unquote(op), state: state do
      SomeModule.unquote(op)(state)
    end
  end
end
```