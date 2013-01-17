ExActor
=======
Macros for easier implementation and usage of gen_server based actors (processes) in Elixir.  
__Warning__: not thoroughly tested, use at your own risk.

# Examples

## Functional style
    import ExActor.Functional
    
    actor Actor do
      defcast inc(state, x) do new_state(state + x) end
      defcall get(state) do state end
    end
    
    act = Actor.start(1)
    Actor.get(act)         # 1
    
    Actor.inc(act, 2)      # this is gen_server:cast
    Actor.get(act)         # 3
    
## "Objectified" style

    require Objectify
    import ExActor.Objectified
    
    actor Actor do
      defcast inc(state, x) do new_state(state + x) end
      defcall get(state) do state end
    end
    
    Objectify.transform do
      act = Actor.start(1)
      act.get                 # 1
    
      act.inc(2)
      act.get                 # 3
    end
    
## Objectified chaining

    Objectify.transform do
      Actor.start(1).inc(2).get   # 3
    end

## Handling of return values

    defcall a(state) do 5 end                 # doesn't change state
    defcall b(state) do reply(5, 6)           # responds 5, sets new state to 6
    
    defcast c(_) do :ok end                   # doesn't change state
    defcast d(_) do new_state(:ok) end        # sets new state
    
    def init(arg) do initial_state(arg) end   # sets initial state
    
    # standard gen_server:call/cast responses are left intact

## Explicit call/cast

    actor Actor do
      defcast inc(state, x) do new_state(state + x) end
      defcall get(state) do state end
    end

    act.cast({:inc, 1})
    act.call({:get})
    
## Custom call/cast handlers

    actor Actor do
      defcast inc(state, x) do new_state(state + x) end
      defcall get(state) do state end
      
      def handle_call(:custom_call, _from, state) do ... end
      def handle_cast(:custom_cast, state) do ... end
    end
    
## Pattern match on args

    actor Actor do
      defcast set(state, 1) do new_state(:one) end
      defcast set(state, 2) do new_state(:two) end
    end

## Pattern match on state

    actor Actor do
      defcall get(1) do :one end
      impcall get(2) do :two end
    end