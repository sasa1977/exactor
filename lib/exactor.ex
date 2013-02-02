defmodule ExActor do
  @moduledoc """
  Syntactic sugar for defining and creating actors.

  Examples:
    defmodule Calculator do
      use ExActor
      
      defcast inc(x), state: state, do: new_state(state + x)
      defcast dec(x), state: state, do: new_state(state - x)
      defcall get, state: state, do: state
    end
    
    {:ok, calculator} = Calculator.start(0)
    Calculator.inc(calculator, 10)
    Calculator.dec(calculator, 5)
    IO.puts(Calculator.get(calculator))
    
  OO approach (based on tuple modules):
    import ExActor
    
    defactor Calculator do
      defcast inc(x), state: state, do: new_state(state + x)
      defcast dec(x), state: state, do: new_state(state - x)
      defcall get, state: state, do: state
    end
    
    {:ok, calculator} = Calculator.start(0)
    IO.puts(calculator.inc(10).dec(5).get)
  """
  
  defmacro __using__(_opts) do
    quote do
      use GenServer.Behaviour
      require GenX.GenServer
      import ExActor.Privates
      unquote(interface_funs)
    end
  end

  def interface_funs do
    quote do
      def start, do: start(nil)
      def start(args), do: start(args, [])
      def start(args, options) do
        :gen_server.start(__MODULE__, args, options)
      end
      
      def start_link, do: start_link(nil)
      def start_link(args), do: start_link(args, [])
      def start_link(args, options) do
        :gen_server.start_link(__MODULE__, args, options)
      end
    end
  end
  
  defmodule Privates do
    defmacro defcast(cast, body) do
      wrap_and_delegate(:defcast, cast, body)
    end
    
    defmacro defcast(cast, options, body) do
      wrap_and_delegate(:defcast, cast, Keyword.from_enum(options ++ body))
    end
    
    defmacro defcall(call, body) do
      wrap_and_delegate(:defcall, call, body)
    end
    
    defmacro defcall(call, options, body) do
      wrap_and_delegate(:defcall, call, Keyword.from_enum(options ++ body))
    end
    
    defp wrap_and_delegate(type, name, options) do
      {state_arg, state_identifier} = get_state_identifier([], options[:state] || {:_, [], :quoted})
      handler_body = options[:do]
      
      options = (options |>
        Keyword.put(:state, state_arg) |>
        Keyword.put(:do, wrap_handler_body(wrapper(type), state_identifier, handler_body))
      )

      (quote do
        GenX.GenServer.unquote(type)(unquote(name), unquote(options))
      end)
    end
    
    
    defp get_state_identifier(_, {:=, _, [_, state_identifier]} = state_arg) do
      {state_arg, state_identifier}
    end

    defp get_state_identifier(line, any) do
      get_state_identifier(line, {:=, line, [any, {:___generated_state, line, nil}]})
    end
    
    defp wrap_handler_body(handler, state_identifier, body) do
      quote do
        unquote(handler)((fn() -> unquote(body) end).(), unquote(state_identifier))
      end
    end
    
    defp wrapper(:defcast), do: :handle_cast_response
    defp wrapper(:defcall), do: :handle_call_response
    
    
    
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
  
  
  
  defmacro defactor(name, [do: definition]) do
    quote do
      defmodule unquote(name) do
        defmodule :__Impl__ do
          use ExActor
          unquote(definition)
        end
        
        def start, do: start(nil)
        def start(args), do: start(args, [])
        def start(args, options) do
          decorate_start_response(:__Impl__.start(args, options))
        end
        
        def start_link, do: start_link(nil)
        def start_link(args), do: start_link(args, [])
        def start_link(args, options) do
          decorate_start_response(:__Impl__.start_link(args, options))
        end
        
        def this, do: actor(self)
        def pid({module, pid}) when module === __MODULE__, do: pid
        def actor(pid), do: {__MODULE__, pid}
        
        defp decorate_start_response({:ok, pid}), do: {:ok, actor(pid)}
        defp decorate_start_response(any), do: any
        
        import ExActor.ObjWrapper
        unquote(definition)
      end
    end
  end
  
  defmodule ObjWrapper do
    defmacro defcast(cast, _) do
      wrap_and_delegate(:defcast, cast)
    end
    
    defmacro defcast(cast, _, _) do
      wrap_and_delegate(:defcast, cast)
    end
    
    defmacro defcall(call, _) do
      wrap_and_delegate(:defcall, call)
    end
    
    defmacro defcall(call, _, _) do
      wrap_and_delegate(:defcall, call)
    end
    
    defp wrap_and_delegate(type, {name, _, args}) do
      call_args = [quote(do: pid) | (args || [])]
      args = (args || []) ++ [quote(do: {module, pid})]
      
      (quote do
        def unquote(name)(unquote_splicing(args)) when module === __MODULE__ do
          unquote(make_delegator(type, name, call_args))
        end
      end)
    end
    
    defp make_delegator(:defcall, name, args) do
      delegate(name, args)
    end
    
    defp make_delegator(:defcast, name, args) do
      quote do
        case unquote(delegate(name, args)) do
          :ok -> {module, pid}
          other -> other
        end
      end
    end
    
    defp delegate(name, args) do
      quote do: :__Impl__.unquote(name)(unquote_splicing(args))
    end
  end
end
