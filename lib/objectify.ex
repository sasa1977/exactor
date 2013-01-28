defmodule Objectify do
  @moduledoc """
  Provides syntactic sugar for combining data and code into 'instances' which resemble objects.
  Note: this code is built for demonstration purposes only. Do not use it in production.
  
  Example:
    Objectify.transform do
      list = Objectify.wrap(List, [1,2,3])
      IO.puts list.last
    end
  """
  
  defmacro transform([do: block]) do  
    do_transform(block)
  end
  
  defp do_transform({{:., line, [left, right]}, _, args} = expr) do
    case should_wrap(left) do
      true -> wrap_call(line, left, right, args)
      false -> transform_tuple(expr)
    end
  end
  
  defp do_transform(list) when is_list(list) do
    lc element inlist list do do_transform(element) end
  end
  
  defp do_transform(tuple) when is_tuple(tuple) do
    transform_tuple(tuple)
  end
  
  defp do_transform(any) do any end
  
  defp transform_tuple(tuple) do list_to_tuple(do_transform(tuple_to_list(tuple))) end
  
  defp should_wrap({{:., _, _}, _, _}) do true end              # chained call
  defp should_wrap({:__aliases__, _, _}) do false end           # module call
  defp should_wrap({mod, _, _}) when is_atom(mod) do true end   # variable call
  defp should_wrap(_other) do false end
  
  def wrap_call(line, left, right, args) do
    {
      {:., line, [{:__aliases__,[line: 0],[:Objectify]},:object_invoke]}, 
      line,
      [do_transform(left), do_transform(right), do_transform(args)]
    }
  end
  
  def object_invoke({:__objectified__, module, instance}, method, args) do
    object_invoke(module, method, [instance | args])
  end
  
  def object_invoke(m, f, a) do
    apply(m, f, a)
  end
  
  def wrap(module, instance), do: {:__objectified__, module, instance}
  def unwrap({:__objectified__, _, instance}), do: instance
  
    
  defmacro defmodule_o(name, [do: definition]) do
    quote do
      defmodule unquote(name) do
        Objectify.transform do
          def _new(value), do: Objectify.wrap(__MODULE__, value)
          
          unquote(definition)
        end
      end
    end
  end
end