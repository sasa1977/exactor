defmodule ExActor.Delegator do
  @moduledoc """
  Provides `delegate_to/2` macro that can be used to simplify cases when
  call/cast operations delegate to another module.
  """

  @doc """
  Creates wrapper operations around the `target_module`.

  For example:

      defmodule HashDictServer do
        use ExActor.GenServer
        import ExActor.Delegator

        defstart start_link, do: initial_state(HashDict.new)

        delegate_to HashDict do
          query get/2
          trans put/3
        end
      end

  This is the same as:

      defmodule HashDictServer do
        use ExActor.GenServer

        defstart start_link, do: initial_state(HashDict.new)

        defcall get(k), state: state do
          HashDict.get(state, k)
          |> reply
        end

        defcast put(k, v), state:state do
          HashDict.put(state, k, v)
          |> new_state
        end
      end
  """
  defmacro delegate_to(target_module, opts) do
    statements(opts[:do])
    |> Enum.map(&(parse_instruction(target_module, &1)))
  end

  defp statements({:__block__, _, statements}), do: statements
  defp statements(statement), do: [statement]


  defp parse_instruction(target_module, {:init, _, _}) do
    quote do
      definit do
        unquote(target_module).new
        |> initial_state
      end
    end
  end

  defp parse_instruction(target_module, {:query, _, [{:/, _, [{fun, _, _}, arity]}]}) do
    make_delegate(:defcall, fun, arity,
      quote do
        unquote(forward_call(target_module, fun, arity))
        |> reply
      end
    )
  end

  defp parse_instruction(target_module, {:trans, _, [{:/, _, [{fun, _, _}, arity]}]}) do
    make_delegate(:defcast, fun, arity,
      quote do
        unquote(forward_call(target_module, fun, arity))
        |> new_state
      end
    )
  end


  defp make_delegate(type, fun, arity, code) do
    quote do
      unquote(type)(
        unquote(fun)(unquote_splicing(make_args(arity))),
        state: state,
        do: unquote(code)
      )
    end
  end


  defp forward_call(target_module, fun, arity) do
    full_args = [quote(do: state) | make_args(arity)]

    quote do
      unquote(target_module).unquote(fun)(unquote_splicing(full_args))
    end
  end


  defp make_args(arity) when arity > 0 do
    1..arity
    |> Enum.map(&{:"arg#{&1}", [], nil})
    |> tl
  end
end