require Objectify

defmodule ExActor.Test do
  use ExUnit.Case 

  test "objectify" do 
    Objectify.transform do
      assert List.last([1,2,3]) == 3
      assert Objectify.wrap(List, [1,2,3]).last === 3
    end
  end
  
  defmodule Functional do
    import ExActor.Functional
    
    actor Actor do
      defcast set(_, x) do new_state(x) end
      defcall get(state) do state end
      defcast leave_state(_) do 0 end
      defcall reply_leave_state(_) do 0 end
      defcall full_reply(_) do reply(1,2) end
    end
  end
  
  test "functional actor" do
    actor = Functional.Actor.start(1)
    assert Functional.Actor.get(actor) == 1
    
    Functional.Actor.set(actor, 2)
    assert Functional.Actor.get(actor) == 2
    
    Functional.Actor.cast(actor, {:set, 3})
    assert Functional.Actor.call(actor, {:get}) == 3
    
    Functional.Actor.leave_state(actor)
    assert Functional.Actor.get(actor) == 3
    
    assert Functional.Actor.reply_leave_state(actor) == 0
    assert Functional.Actor.get(actor) == 3
    
    assert Functional.Actor.full_reply(actor) == 1
    assert Functional.Actor.get(actor) == 2
  end
  
  
  
  defmodule Objectified do
    import ExActor.Objectified
    
    actor Actor do
      defcast set(_, x) do new_state(x) end
      defcast inc(value, x) do new_state(value + x) end
      defcast dec(value, x) do new_state(value - x) end
      defcall get(value) do value end
    end
  end
  
  test "objectified actor" do
    Objectify.transform do
      actor = Objectified.Actor.start(0)
      assert actor.get == 0
      
      actor.set(4)
      assert actor.get == 4
      
      assert actor.inc(10).dec(3).get == 11
    end
  end
  
  test "initialization" do
    Objectify.transform do
      assert Objectified.Actor.start.get == nil
      assert Objectified.Actor.start(1).get == 1
    end
  end
  
  
  
  defmodule Custom do
    import ExActor.Objectified
    
    actor Actor do
      defcast set(_, x) do new_state(x) end
      defcall get(state) do state end
      
      def handle_call(:test_call, _, state) do {:reply, :call_reply, state} end
      def handle_cast({:multiply, new_value}, state) do {:noreply, state * new_value} end
    end
  end
  
  test "custom actor" do
    Objectify.transform do
      actor = Custom.Actor.start(0)
      assert actor.call(:test_call) == :call_reply
      assert actor.get == 0
      
      assert actor.set(3).cast({:multiply, 5}).get == 15
    end
  end
  
  
  defmodule PatternMatching do
    import ExActor.Objectified
    
    actor Actor do
      defcast set(_, x) do new_state(x) end
      defcall get(state) do state end
      
      defcall test_pattern_arg(_, 1) do :one end
      def handle_call({:test_pattern_arg, 2}, _, state) do {:reply, :two, state} end
      defcall test_pattern_arg(_, any) do any end
      
      defcall test_pattern_state(1) do :one end
      impcall test_pattern_state(2) do :two end
      
      defcast test_pattern_arg2(_, 1) do new_state(:one) end
      defcast test_pattern_arg2(_, 2) do new_state(:two) end
      
      defcast test_pattern_state2(3) do new_state(:three) end
      impcast test_pattern_state2(4) do new_state(:four) end
    end
  end
  
  test "actor pattern matching" do
    Objectify.transform do
      actor = PatternMatching.Actor.start
      assert actor.test_pattern_arg(1) == :one
      assert actor.test_pattern_arg(2) == :two
      assert actor.test_pattern_arg(:any) == :any
      
      assert actor.set(1).test_pattern_state == :one
      assert actor.set(2).test_pattern_state == :two
      
      assert actor.test_pattern_arg2(1).get == :one
      assert actor.test_pattern_arg2(2).get == :two
      
      assert actor.set(3).test_pattern_state2.get == :three
      assert actor.set(4).test_pattern_state2.get == :four
    end
  end
end