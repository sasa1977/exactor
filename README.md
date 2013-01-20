ExActor
=======
Macros for easier implementation and usage of gen_server based actors (processes) in Elixir.
Built on top of [GenX](https://github.com/yrashk/genx).

__Warning__: not thoroughly tested, use at your own risk.

# Examples

## Functional style
    import ExActor.Functional
    
    actor Actor do
      defcast inc(x), state: state, do: new_state(state + x)
      defcall get, state: state, do: state
    end
    
    act = Actor.start(1)
    Actor.get(act)         # 1
    
    Actor.inc(act, 2)      # this is gen_server:cast
    Actor.get(act)         # 3
    
## "Objectified" style

    import ExActor.Objectified
    
    actor Actor do
      defcast inc(x), state: state, do: new_state(state + x)
      defcall get, state: state, do: state
    end
    
    ...
    
    require Objectify
    
    Objectify.transform do
      act = Actor.start(1)
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
    
## Custom call/cast handlers

    actor Actor do
      defcast inc(x), state: state, do: new_state(state + x)
      defcall get, state: state, do: state
      
      def handle_call(:custom_call, _from, state), do: ... 
      def handle_cast(:custom_cast, state), do: ...
    end