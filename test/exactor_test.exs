require Objectify

defmodule ExActor.Test do
  use ExUnit.Case 
  
  defmodule Calculator do
    def new(value) do
      Objectify.wrap(ExActor.Test.Calculator, value)
    end
    
    def inc(value) do
      Objectify.wrap(ExActor.Test.Calculator, value + 1)
    end
    
    def get(value) do value end    
  end

  defrecord Tr, a: nil
  test "objectify" do 
    Objectify.transform do
      # plain old call
      assert List.last([1,2,3]) == 3
      
      # inline call
      assert Objectify.wrap(List, [1,2,3]).last === 3
      
      # variable call
      a = Objectify.wrap(List, [1,2,3])
      assert a.last === 3
      
      # chain call
      assert 2 == Calculator.new(0).inc.inc.get
      
      # In early versions I had problems with record pattern matching, so I test this as well, even
      # if it is not directly related to objectify
      Tr[a: 1] = Tr.new(a: 1)
    end
  end
  
  defmodule Functional do
    import ExActor.Functional
    
    actor Actor do
      defcast set(x), do: new_state(x)
      defcall get, state: state, do: state
      defcall reply_leave_state, do: 3
      defcast leave_state, do: 4
      defcall full_reply, do: reply(5,6)
    end
  end
  
  test "functional actor" do
    alias Functional.Actor, as: FunAct
    
    {:ok, actor} = FunAct.start(1)
    assert FunAct.get(actor) == 1
    
    FunAct.set(actor, 2)
    assert FunAct.get(actor) == 2
    
    assert FunAct.reply_leave_state(actor) == 3
    assert FunAct.get(actor) == 2
    
    FunAct.leave_state(actor)
    assert FunAct.get(actor) == 2
    
    assert FunAct.full_reply(actor) == 5
    assert FunAct.get(actor) == 6
  end
  
  test "starting" do
    alias Functional.Actor, as: FunAct
    
    {:ok, actor} = FunAct.start
    assert FunAct.get(actor) == nil
    
    {:ok, actor} = FunAct.start(1)
    assert FunAct.get(actor) == 1
    
    {:ok, actor} = FunAct.start(1, [])
    assert FunAct.get(actor) == 1
    
    {:ok, actor} = FunAct.start_link
    assert FunAct.get(actor) == nil
    
    {:ok, actor} = FunAct.start_link(1)
    assert FunAct.get(actor) == 1
    
    {:ok, actor} = FunAct.start_link(1, [])
    assert FunAct.get(actor) == 1
  end
  
  defmodule Objectified do
    import ExActor.Objectified
    
    actor Actor do
      defcast set(x), do: new_state(x)
      defcast inc(x), state: value, do: new_state(value + x)
      defcast dec(x), state: value, do: new_state(value - x)
      defcall get, state: value, do: value
    end
  end
  
  test "objectified actor" do
    Objectify.transform do
      {:ok, actor} = Objectified.Actor.start(0)
      assert actor.get == 0
      
      actor.set(4)
      assert actor.get == 4
      
      actor.inc(10)
      actor.dec(3)
      assert actor.get == 11
    end
  end
end