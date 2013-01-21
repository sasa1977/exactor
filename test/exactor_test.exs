require Objectify

defmodule ExActor.Test do
  use ExUnit.Case 
  import ExActor
  
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
  
  actor FunActor do
    defcast set(x), do: new_state(x)
    defcall get, state: state, do: state
    defcall reply_leave_state, do: 3
    defcast leave_state, do: 4
    defcall full_reply, do: reply(5,6)
  end
  
  test "functional actor" do    
    {:ok, actor} = FunActor.start(1)
    assert FunActor.get(actor) == 1
    
    FunActor.set(actor, 2)
    assert FunActor.get(actor) == 2
    
    assert FunActor.reply_leave_state(actor) == 3
    assert FunActor.get(actor) == 2
    
    FunActor.leave_state(actor)
    assert FunActor.get(actor) == 2
    
    assert FunActor.full_reply(actor) == 5
    assert FunActor.get(actor) == 6
  end
  
  test "functional starting" do
    {:ok, actor} = FunActor.start
    assert FunActor.get(actor) == nil
    
    {:ok, actor} = FunActor.start(1)
    assert FunActor.get(actor) == 1
    
    {:ok, actor} = FunActor.start(1, [])
    assert FunActor.get(actor) == 1
    
    {:ok, actor} = FunActor.start_link
    assert FunActor.get(actor) == nil
    
    {:ok, actor} = FunActor.start_link(1)
    assert FunActor.get(actor) == 1
    
    {:ok, actor} = FunActor.start_link(1, [])
    assert FunActor.get(actor) == 1
  end
  
  actor ObjActor do
    defcast set(x), do: new_state(x)
    defcast inc(x), state: value, do: new_state(value + x)
    defcast dec(x), state: value, do: new_state(value - x)
    defcall get, state: value, do: value
  end
  
  test "objectified actor" do
    Objectify.transform do
      {:ok, actor} = ObjActor.new(0)
      assert actor.get == 0
      
      actor.set(4)
      assert actor.get == 4
      
      actor.inc(10)
      actor.dec(3)
      assert actor.get == 11
    end
  end
  
  test "objectified starting" do
    Objectify.transform do
      assert elem(ObjActor.new, 1).get == nil
      assert elem(ObjActor.new(1), 1).get == 1
      assert elem(ObjActor.new(1, []), 1).get == 1
      
      assert elem(ObjActor.new_link, 1).get == nil
      assert elem(ObjActor.new_link(1), 1).get == 1
      assert elem(ObjActor.new_link(1, []), 1).get == 1
    end
  end
end