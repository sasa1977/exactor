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
  
  # Had to exclude Kernel calls from objectification, because pattern matching on
  # records didn't work. Must investigate it further.
  defp do_transform({{:., _, [left, right]}, _, args}) when left != Kernel do
    quote do
      Objectify.object_invoke(
        unquote(do_transform(left)), 
        unquote(do_transform(right)), 
        unquote(do_transform(args))
      )
    end
  end
  
  defp do_transform(list) when is_list(list) do
    lc element inlist list do do_transform(element) end
  end
  
  defp do_transform(tuple) when is_tuple(tuple) do
    list_to_tuple(do_transform(tuple_to_list(tuple)))
  end
  
  defp do_transform(any) do any end
  
  def object_invoke({:object, module, instance}, method, args) do
    object_invoke(module, method, [instance | args])
  end
  
  def object_invoke(m, f, a) do
    apply(m, f, a)
  end
  
  def wrap(module, instance) do {:object, module, instance} end
end