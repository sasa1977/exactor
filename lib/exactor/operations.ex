defmodule ExActor.Operations do
  @moduledoc """
  Macros that can be used for simpler definition of `gen_server` operations
  such as casts or calls.
  """

  @doc false
  defmacro definit(opts), do: do_definit(opts)

  @doc """
  Defines the initializer callback.

  Examples:

      # ignoring the input argument
      definit do: HashSet.new

      # using the input argument
      definit x do
        x + 1
      end

      # pattern matching
      definit x, when: ..., do: ...
  """
  defmacro definit(arg, opts), do: do_definit([{:arg, arg} | opts])

  defp do_definit(opts) do
    quote bind_quoted: [opts: Macro.escape(opts, unquote: true)] do
      if (opts[:when]) do
        def init(unquote_splicing([opts[:arg] || quote(do: _)])) when unquote(opts[:when]) do
          unquote(opts[:do])
        end
      else
        def init(unquote_splicing([opts[:arg] || quote(do: _)])) do
          unquote(opts[:do])
        end
      end
    end
  end


  @doc false
  defmacro defcast(cast, body) do
    generate_funs(:defcast, cast, body ++ [module: __CALLER__.module])
  end

  @doc """
  Defines the cast callback clause and a corresponding interface fun.

  Examples:

      defcast operation, do: noreply
      defcast inc(x), state: state, do: new_state(state + x)

      # omitting interface fun
      defcast operation, export: false, do: ...

      # pattern matching
      defcast a(1), do: ...
      defcast a(2), do: ...
      defcast a(x), state: 1, do: ...
      defcast a(x), when: x > 1, do: ...
      defcast a(x), state: state, when: state > 1, do: ...
      defcast a(_), do: ...
  """
  defmacro defcast(cast, options, body) do
    generate_funs(:defcast, cast, options ++ body ++ [module: __CALLER__.module])
  end



  @doc false
  defmacro defcall(call, body) do
    generate_funs(:defcall, call, body ++ [module: __CALLER__.module])
  end

  @doc """
  Defines the call callback clause and a corresponding interface fun.

  Examples:

      defcall operation, do: reply(response)
      defcall get, state: state, do: reply(state)
      defcall inc, state: state, do: set_and_reply(state + 1, response)

      # timeout option
      defcall long_call, state: state, timeout: :timer.seconds(10), do: ...

      # omitting interface fun
      defcall operation, export: false, do: ...

      # pattern matching
      defcall a(1), do: ...
      defcall a(2), do: ...
      defcall a(x), state: 1, do: ...
      defcall a(x), when: x > 1, do: ...
      defcall a(x), state: state, when: state > 1, do: ...
      defcall a(_), do: ...
  """
  defmacro defcall(call, options, body) do
    generate_funs(:defcall, call, options ++ body ++ [module: __CALLER__.module])
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
      msg = ExActor.Helper.msg_payload(name, args)
    end
  end


  defp define_interface(type) do
    quote do
      arity = length(args) + case options[:export] do
        nil -> 1
        true -> 1
        _ -> 0
      end

      unless options[:export] == false or HashSet.member?(@exported, {name, arity}) do
        server_fun = unquote(server_fun(type))
        unquote(quoted_interface)

        @exported HashSet.put(@exported, {name, arity})
      end
    end
  end

  defp quoted_interface do
    quote bind_quoted: [] do
      {server_arg, interface_args} = ExActor.Helper.interface_args(args, options)
      send_msg = ExActor.Helper.msg_payload(name, interface_args)
      interface_args = case server_arg do
        nil -> interface_args
        _ -> [server_arg | interface_args]
      end

      def unquote(name)(unquote_splicing(interface_args)) do
        GenServer.unquote(server_fun)(
          unquote_splicing(ExActor.Helper.server_args(options, type, send_msg))
        )
      end
    end
  end


  defp server_fun(:defcast), do: :cast
  defp server_fun(:defcall), do: :call


  defp define_handler(type) do
    quote bind_quoted: [type: type] do
      state_arg = ExActor.Helper.get_state_identifier(options[:state])
      {handler_name, handler_args} = ExActor.Helper.handler_sig(type, options, msg, state_arg)
      guard = options[:when]

      if guard do
        def unquote(handler_name)(
          unquote_splicing(handler_args)
        ) when unquote(guard), do: unquote(options[:do])
      else
        def unquote(handler_name)(
          unquote_splicing(handler_args)
        ), do: unquote(options[:do])
      end
    end
  end


  @doc false
  defmacro definfo(msg, options), do: impl_definfo(msg, options)

  @doc """
  Defines the info callback clause. Responses work just like with casts.

  Examples:

      definfo :some_message, do: ...
      definfo :another_message, state: ..., do:
  """
  defmacro definfo(msg, opts, body) do
    impl_definfo(msg, opts ++ body)
  end

  defp impl_definfo(msg, options) do
    quote bind_quoted: [
      msg: Macro.escape(msg, unquote: true),
      options: Macro.escape(options, unquote: true)
    ] do

      state_arg = ExActor.Helper.get_state_identifier(options[:state])

      if options[:when] do
        def handle_info(unquote(msg), unquote(state_arg)) when unquote(options[:when]) do
          unquote(options[:do])
        end
      else
        def handle_info(unquote(msg), unquote(state_arg)) do
          unquote(options[:do])
        end
      end
    end
  end
end