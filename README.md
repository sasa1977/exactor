ExActor
=======
Macros for easier implementation and usage of gen_server based actors (processes) in Elixir.
This library is inspired by (though not depending on) [GenX](https://github.com/yrashk/genx), but in addition, removes some more boilerplate and changes some semantics of the handle_call/cast responses.

# Examples

## Basic usage

    defmodule Actor do
      use ExActor
      
      defcast inc(x), state: state, do: new_state(state + x)
      defcall get, state: state, do: state
    end
    
    {:ok, act} = Actor.start(1)
    Actor.get(act)         # 1
    
    Actor.inc(act, 2)
    Actor.get(act)         # 3

## Singleton actors

    defmodule SingletonActor do
      use ExActor, export: :singleton   # The actor process will be locally registered

      defcall get, state: state, do: state
      defcast set(x), do: new_state(x)
    end

    SingletonActor.start
    SingletonActor.set(5)
    SingletonActor.get

## Handling of return values

    defcall a, state: state, do: 5                # responds 5, doesn't change state
    defcall b, do: reply(5, 6)                    # responds 5, sets new state to 6
    defcall c, do: {:reply, response, newstate}   # standard gen_server response is left intact
    
    defcast c, do: :ok                            # ignores response, doesn't change state
    defcast d, do: new_state(:ok)                 # sets new state
    defcast f, do: {:noreply, newstate}           # standard gen_server response is left intact
    
    def init(arg), do: initial_state(arg)         # sets initial state
    def init(arg), do: {:ok, arg}                 # standard gen_server response    
    
## Simplified starting
    
    Actor.start         # same as Actor.start(nil)
    Actor.start(args)
    Actor.start(args, options)
    
    Actor.start_link
    Actor.start_link(args)
    Actor.startLink(args, options)
    
## Tuple modules support
    
    actor = Actor.actor_start_link(0)

    # alternatively, from pid:
    actor = Actor.actor(pid)
    
    # operations can be called directly on actor which is a tuple module
    actor.inc(1)
    
    # cast returns the actor on which it operates, so you can chain calls
    actor.
      inc(5).
      inc(10).
      get
