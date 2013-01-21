ExActor
=======
Macros for easier implementation and usage of gen_server based actors (processes) in Elixir.
Built on top of [GenX](https://github.com/yrashk/genx).

__Warning__: not thoroughly tested, use at your own risk.

# Examples

## Functional style
    import ExActor
    
    actor Actor do
      defcast inc(x), state: state, do: new_state(state + x)
      defcall get, state: state, do: state
    end
    
    {:ok, act} = Actor.start(1)
    Actor.get(act)         # 1
    
    Actor.inc(act, 2)
    Actor.get(act)         # 3
    
## "Objectified" style
    require Objectify
    
    Objectify.transform do
      {:ok, act} = Actor.new(1)
      act.get                 # 1
    
      act.inc(2)
      act.get                 # 3
    end
    
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
    
    # functional:
    Actor.start         # same as Actor.start(nil)
    Actor.start(args)
    Actor.start(args, options)
    
    Actor.start_link
    Actor.start_link(args)
    Actor.startLink(args, options)
    
    #objectified:
    Actor.new       # like start
    Actor.new_link  # like start_link