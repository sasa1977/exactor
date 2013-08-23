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
    defmodule Calculator do
      use ExActor
      
      defcast inc(x), state: state, do: new_state(state + x)
      defcast dec(x), state: state, do: new_state(state - x)
      defcall get, state: state, do: state
    end
    
    calculator = Actor.actor_start_link(0)
    IO.puts(calculator.inc(10).dec(5).get)
  """
  
  defmacro __using__(opts) do
    Module.put_attribute(__CALLER__.module, :exactor_global_options, opts || [])
    
    quote do
      use GenServer.Behaviour
      import ExActor
      unquote(interface_funs(__CALLER__))
      
      exported = HashSet.new
    end
  end
  
  def interface_funs(caller) do
    quote do
      def start(args // nil, options // []) do
        :gen_server.start(unquote_splicing(start_args(caller)))
      end
      
      def start_link(args // nil, options // []) do
        :gen_server.start_link(unquote_splicing(start_args(caller)))
      end

      def actor_start(args // nil, options // []) do
        start(args, options) |> response_to_actor
      end

      def actor_start_link(args // nil, options // []) do
        start_link(args, options) |> response_to_actor
      end

      defp response_to_actor({:ok, pid}), do: actor(pid)
      defp response_to_actor(any), do: any
      
      def this, do: actor(self)
      def pid({module, pid}) when module === __MODULE__, do: pid
      def actor(pid), do: {__MODULE__, :exactor_tupmod, pid}

      unquote(def_initializer(caller))
    end
  end

  defp start_args(caller) do
    defargs = [quote(do: __MODULE__), quote(do: args), quote(do: options)]
    case Module.get_attribute(caller.module, :exactor_global_options)[:export] do
      default when default in [nil, false, true] -> defargs
      
      local_name when is_atom(local_name) ->
        [quote(do: {:local, unquote(local_name)}) | defargs]
      
      {:local, local_name} ->
        [quote(do: {:local, unquote(local_name)}) | defargs]
      
      {:global, global_name} ->
        [quote(do: {:global, unquote(global_name)}) | defargs]
      
      _ -> defargs
    end
  end

  defp def_initializer(caller) do
    case Module.get_attribute(caller.module, :exactor_global_options)[:initial_state] do
      nil -> nil
      state ->
        quote do
          def init(_), do: initial_state(unquote(state))
        end
    end
  end

  defmacro definit(opts) do
    quote do
      opts = unquote(Macro.escape(opts, unquote: true))
      
      def(
        :init,
        [opts[:input] || quote(do: _)],
        [],
        do: quote do
          initial_state(unquote(opts[:do]))
        end
      )
    end
  end
  
  
  defmacro defcast(cast, body) do
    generate_funs(:defcast, cast, body ++ [module: __CALLER__.module])
  end
  
  defmacro defcast(cast, options, body) do
    generate_funs(:defcast, cast, Keyword.from_enum(options ++ body  ++ [module: __CALLER__.module]))
  end
  
  defmacro defcall(call, body) do
    generate_funs(:defcall, call, body ++ [module: __CALLER__.module])
  end
  
  defmacro defcall(call, options, body) do
    generate_funs(:defcall, call, Keyword.from_enum(options ++ body ++ [module: __CALLER__.module]))
  end
  
  defp generate_funs(type, name, options) do
    quote do
      unquote(transfer_options(type, name, options))
      unquote(define_interface(type))
      unquote(define_handler(type))
    end
  end



  defp transfer_options(type, name, options) do
    quote do
      {name, args} = case unquote(Macro.escape(name, unquote: true)) do
        name when is_atom(name) -> {name, []}
        {name, _, args} -> {name, args || []}
      end
      
      options = 
        Module.get_attribute(__MODULE__, :exactor_global_options)
        |> Keyword.merge(unquote(Macro.escape(options, unquote: true)))

      type = unquote(Macro.escape(type, unquote: true))
      msg = ExActor.msg_payload(name, args)
    end
  end

  def msg_payload(function, nil), do: function
  def msg_payload(function, []), do: function
  def msg_payload(function, args), do: quote(do: {unquote_splicing([function | args])})



  defp define_interface(type) do
    quote do
      unless options[:export] == false or HashSet.member?(exported, {name, args}) do
        server_fun = unquote(server_fun(type))
        unquote(def_tupmod_interface)
        unquote(def_fun_interface)

        arity = length(args) + case options[:export] do
          nil -> 1
          true -> 1
          _ -> 0
        end
        
        defoverridable [{name, arity}]

        exported = HashSet.put(exported, {name, args})
      end
    end
  end

  defp def_tupmod_interface do
    quote do
      def(
        name, 
        ExActor.interface_args_obj(args), 
        [],
        do: quote do
          server_fun = unquote(server_fun)
          
          result = :gen_server.unquote(server_fun)(unquote_splicing(ExActor.server_args(options, :obj, type, msg)))
          
          case server_fun do
            :cast -> actor(pid)
            :call -> result
          end
        end
      )
    end
  end

  defp def_fun_interface do
    quote do
      def(
        name, 
        ExActor.interface_args_fun(args, options), 
        [],
        do: quote do
          :gen_server.unquote(server_fun)(unquote_splicing(ExActor.server_args(options, :fun, type, msg)))
        end
      )
    end
  end

  def interface_args_fun(args, options), do: server_arg_fun(options) ++ args
  def interface_args_obj(args), do: args ++ server_arg_obj

  defp server_arg_fun(options) do
    cond do
      (options[:export] || true) == true -> [quote(do: server)]
      true -> []
    end
  end
  
  defp server_arg_obj do
    [quote(do: {module, :exactor_tupmod, pid})]
  end
  
  defp server_fun(:defcast), do: :cast
  defp server_fun(:defcall), do: :call
  
  def server_args(options, tupmod, type, msg) do
    [server_ref(options, tupmod), msg] ++ timeout_arg(options, type)
  end

  defp server_ref(_, :obj), do: quote(do: pid)
  defp server_ref(options, :fun) do
    case options[:export] do
      default when default in [nil, false, true] -> quote(do: server)
      local when is_atom(local) -> local
      {:local, local} -> local
      {:global, _} = global -> global
    end 
  end
  
  defp timeout_arg(options, type) do
    case {type, options[:timeout]} do
      {:defcall, timeout} when timeout != nil ->
        [timeout]
      _ -> []
    end
  end
  

  defp define_handler(type) do
    quote do
      {state_arg, state_identifier} = ExActor.get_state_identifier(options[:state] || {:_, [], :quoted})
      {handler_name, handler_args} = ExActor.handler_sig(type, msg, state_arg)
      guard = options[:when]
      if guard, do: guard = [guard], else: guard = []
      handler_body = ExActor.wrap_handler_body(unquote(wrapper(type)), state_identifier, options[:do])

      def(handler_name, handler_args, guard, do: handler_body)
    end
  end

  def handler_sig(:defcall, msg, state_arg) do
    {:handle_call, [msg, quote(do: _from), state_arg]}
  end

  def handler_sig(:defcast, msg, state_arg) do
    {:handle_cast, [msg, state_arg]}
  end

  def get_state_identifier({:=, _, [_, state_identifier]} = state_arg) do
    {state_arg, state_identifier}
  end

  def get_state_identifier(any) do
    get_state_identifier({:=, [], [any, {:___generated_state, [], nil}]})
  end
  
  def wrap_handler_body(handler, state_identifier, body) do
    quote do
      (
        unquote(body)
      ) |>
      unquote(handler)(unquote(state_identifier))
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
