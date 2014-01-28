defmodule ExActor.Delegator do
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