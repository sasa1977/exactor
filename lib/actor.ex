defmodule ActorBuilder do
  def common_interface_funs do
    quote do
      def start do start(nil) end
      
      def call(server, msg) do
        actor_invoke(server, :call, msg)
      end
      
      def cast(server, msg) do
        actor_invoke(server, :cast, msg)
      end
    end
  end
  
  def pure_actor_interface_funs do
    quote do
      def start(initial_state) do
        {:ok, pid} = :gen_server.start(__MODULE__, initial_state, [])
        pid
      end
      
      defp actor_invoke(server, type, msg) do
        apply(:gen_server, type, [server, msg])
      end
      
      unquote(common_interface_funs)
    end
  end
  
  def objectified_actor_interface_funs do
    quote do
      require Objectify
      
      def start(initial_state) do
        {:ok, pid} = :gen_server.start(__MODULE__, initial_state, [])
        instance(pid)
      end
      
      defp actor_invoke(server, :cast, msg) do
        apply(:gen_server, :cast, [server, msg])
        instance(server)
      end
      
      defp actor_invoke(server, type, msg) do
        apply(:gen_server, type, [server, msg])
      end
      
      def this do instance(self) end
      defp instance(pid) do Objectify.wrap(__MODULE__, pid) end
      
      unquote(common_interface_funs)
    end
  end
  
  # root of transformation
  def transform({:__block__, line, main_block}) do
    {actor_definitions, rest} = get_actor_definitions(main_block)
    
    rest = [
      (quote do
        use GenServer.Behaviour
        import Actor.Privates
      end) | 
      rest ++ generate_actor_funs(actor_definitions)
    ]

    {:__block__, line, rest}
  end
  
  def transform(tuple) when is_tuple(tuple) do
    # transform of a single clause block
    transform({:__block__, elem(tuple, 1), [tuple]})
  end
  
  def transform(any) do any end
  
  def get_actor_definitions(clauses) do
    Enum.partition(clauses, function(:actor_fun_def?, 1))
  end
  
  defp actor_fun_def?({:defcall, _, _}) do true end
  defp actor_fun_def?({:defcast, _, _}) do true end
  defp actor_fun_def?({:impcall, _, _}) do true end
  defp actor_fun_def?({:impcast, _, _}) do true end
  defp actor_fun_def?({:def, _, [{:handle_call, _, _}, _]}) do true end
  defp actor_fun_def?({:def, _, [{:handle_cast, _, _}, _]}) do true end
  defp actor_fun_def?(_) do false end
  
  defp generate_actor_funs(actor_definitions) do
    make_funs_definitions(actor_definitions) /> 
    flatten_definitions
  end
  
  def flatten_definitions(groupped_definitions) do
    :dict.fold(
      fn(_, defs, acc) -> acc ++ defs end, 
      [], groupped_definitions
    )
  end
  
  def make_funs_definitions(actor_definitions) do
    List.foldr(actor_definitions, :dict.new(), fn(actor_definition, acc) ->
      implement_funs(acc, actor_definition)
    end)
  end
  
  def implement_funs(acc, {:def, _, _} = pure_fun) do
    add_definition(acc, pure_fun)
  end
  
  def implement_funs(acc, {:impcall, _, _} = actor_definition) do
    add_definition(acc, implementation_fun(setelem(actor_definition, 0, :defcall)))
  end
  
  def implement_funs(acc, {:impcast, _, _} = actor_definition) do
    add_definition(acc, implementation_fun(setelem(actor_definition, 0, :defcast)))
  end
  
  def implement_funs(acc, actor_definition) do
    acc /> 
    add_definition(interface_fun(actor_definition)) />
    add_definition(implementation_fun(actor_definition))
  end
  
  def add_definition(groupped_defs, definition) do
    :dict.update(fun_name(definition), 
      fn(olddefs) -> [definition | olddefs] end,
      [definition],
      groupped_defs
    )
  end
  
  defp fun_name({:def, _, [{name, _, _}, _]}) do
    name
  end
  
  defp interface_fun({:defcall, _, _} = spec) do
    make_interface_fun(:call, spec)
  end
  
  defp interface_fun({:defcast, _, _} = spec) do
    make_interface_fun(:cast, spec)
  end
  
  # Generates interface fun which will be used as a wrapper for sending message to the actor
  defp make_interface_fun(gen_server_op, {_, line, [{fun_name, _, args}, _]}) do
    [_state_arg | impl_args] = args
    final_args = [{:server, line, nil} | impl_args]
    
    def_fun(fun_name, line, final_args, quote do
      actor_invoke(
        unquote({:server, line, nil}), 
        unquote(gen_server_op),
        {unquote(fun_name), unquote_splicing(impl_args)}
      )
    end)
  end
  
  # Generation of a handle_call clause
  defp implementation_fun({:defcall, line, [{fun_name, _, args}, [do: body]]}) do
    [state_arg | impl_args] = args
    {state_arg, state_identifier} = get_state_identifier(line, state_arg)
    
    def_fun(:handle_call, line, 
      [{:"{}", line, [fun_name | impl_args]}, {:_from, line, :nil}, state_arg],
      exec_and_handle_response(:handle_call_response, state_identifier, body)
    )
  end
  
  # generation of a handle_cast clause
  defp implementation_fun({:defcast, line, [{fun_name, _, args}, [do: body]]}) do
    [state_arg | impl_args] = args
    {state_arg, state_identifier} = get_state_identifier(line, state_arg)

    def_fun(:handle_cast, line, 
      [{:"{}", line, [fun_name | impl_args]}, state_arg],
      exec_and_handle_response(:handle_cast_response, state_identifier, body)
    )
  end
  
  defp def_fun(fun_name, line, args, body) do
    # Manual generation of functions, so I can insert correct line number
    {:def, line, [{fun_name, line, args}, [do: body]]}
  end
  
  defp exec_and_handle_response(handler, state_identifier, body) do
    quote do
      unquote(handler)((fn() -> unquote(body) end).(), unquote(state_identifier))
    end
  end
  
  # Gets or makes a named identifier for a state parameter. This identifier will be used
  # for generic response handling.
  defp get_state_identifier(_, {:=, _, [_, state_identifier]} = state_arg) do
    {state_arg, state_identifier}
  end
  
  defp get_state_identifier(line, any) do
    get_state_identifier(line, {:=, line, [any, {:___generated_state, line, nil}]})
  end
end

defmodule Actor do
  defmodule Functional do
    @moduledoc """
    Provides syntactic sugar for defining and creating actors.
    Note: this code is built for demonstration purposes only. Do not use it in production.
  
    Examples:
      import Actor.Functional
    
      actor Calculator do
        defcast inc(state, x) do new_state(state + x) end
        defcast dec(state, x) do new_state(state - x) end
        defcall get(state) do state end
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
      import Actor.Objectified

      actor Calculator do
        defcast inc(state, x) do new_state(state + x) end
        defcast dec(state, x) do new_state(state - x) end
        defcall get(state) do state end
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
