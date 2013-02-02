defmodule ExActor.Test do
  use ExUnit.Case 
  require ExActor
  
  defmodule FunActor do
    use ExActor
    
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
  
  
  ExActor.defactor ObjActor do
    def init(nil), do: initial_state(0)
    def init(other), do: initial_state(other)
    
    defcast set(x), do: new_state(x)
    defcast inc(x), state: value, do: new_state(value + x)
    defcast dec(x), state: value, do: new_state(value - x)
    defcall get, state: value, do: value
    defcall me, do: this
  end
  
  test "objectified actor" do
    {:ok, actor} = ObjActor.start
    
    assert is_pid(actor.pid)
    assert actor === ObjActor.actor(actor.pid)
    
    assert actor.get == 0
    
    actor.set(4)
    assert actor.get == 4
    
    assert actor.inc(10).dec(3).get == 11
    
    assert actor.me == actor
  end
  
  test "objectified starting" do
    assert elem(ObjActor.start, 1).get == 0
    assert elem(ObjActor.start(1), 1).get == 1
    assert elem(ObjActor.start(1, []), 1).get == 1
    
    assert elem(ObjActor.start_link, 1).get == 0
    assert elem(ObjActor.start_link(1), 1).get == 1
    assert elem(ObjActor.start_link(1, []), 1).get == 1
  end
end