defmodule ExActor do
  defmodule Functional do
    @moduledoc """
    Provides syntactic sugar for defining and creating actors.
    Note: this code is built for demonstration purposes only. Do not use it in production.
  
    Examples:
      import ExActor.Functional
    
      actor Calculator do
        defcast inc(x), state: state, do: new_state(state + x)
        defcast dec(x), state: state, do: new_state(state - x)
        defcall get, state: state, do: state
      end
    
      calculator = Calculator.start(0)
      Calculator.inc(calculator, 10)
      Calculator.dec(calculator, 5)
      IO.puts(Calculator.get(calculator))
    """
  
    import ActorBuilder
  
    defmacro actor(name, [do: definition]) do
      quote do
        defmodule unquote(name) do
          unquote(pure_actor_interface_funs)
          unquote(transform(definition))
        end
      end
    end
  end
  
  defmodule Objectified do
    @moduledoc """
    Provides syntactic sugar for defining and creating objectify friendly actors.
    Note: this code is built for demonstration purposes only. Do not use it in production.

    Examples:
      import ExActor.Objectified

      actor Calculator do
        defcast inc(x), state: state, do: new_state(state + x)
        defcast dec(x), state: state, do: new_state(state - x)
        defcall get, state: state, do: state
      end

      Objectify.transform do
        actor = Calculator.start(0)
        actor.inc(10)
        actor.dec(5)
        IO.puts(actor.get)
      end
    """
    
    import ActorBuilder
  
    defmacro actor(name, [do: definition]) do
      quote do
        defmodule unquote(name) do
          unquote(objectified_actor_interface_funs)
          unquote(transform(definition))
        end
      end
    end
  end
  
  defmodule Privates do
    def reply(response, new_state) do {:reply, response, new_state} end
    def initial_state(state) do {:ok, state} end
    def new_state(state) do {:noreply, state} end

    def handle_call_response({:reply, _, _} = r, _) do r end
    def handle_call_response({:reply, _, _, _} = r, _) do r end
    def handle_call_response({:noreply, _} = r, _) do r end
    def handle_call_response({:noreply, _, _} = r, _) do r end
    def handle_call_response({:stop, _, _} = r, _) do r end
    def handle_call_response({:stop, _, _, _} = r, _) do r end
    def handle_call_response(reply, state) do {:reply, reply, state} end

    def handle_cast_response({:noreply, _} = r, _) do r end
    def handle_cast_response({:noreply, _, _} = r, _) do r end
    def handle_cast_response({:stop, _, _} = r, _) do r end
    def handle_cast_response(_, state) do {:noreply, state} end
  end
end
