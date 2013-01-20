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
      defcall get, state: state, do: {:reply, state, state}
    end
  end
  
  test "functional actor" do
    actor = Functional.Actor.start(1)
    assert Functional.Actor.get(actor) == 1
  end
  
  '''
  defmodule Functional do
    import ExActor.Functional
    
    actor Actor do
      defcast set(_, x), do: new_state(x)
      defcall get(state), do: state
      defcast leave_state(_), do: 0
      defcall reply_leave_state(_), do: 0
      defcall full_reply(_), do: reply(1,2)
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
      defcast set(_, x), do: new_state(x)
      defcast inc(value, x), do: new_state(value + x)
      defcast dec(value, x), do: new_state(value - x)
      defcall get(value), do: value
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
      defcast set(_, x), do: new_state(x)
      defcall get(state), do: state
      
      def handle_call(:test_call, _, state), do: {:reply, :call_reply, state}
      def handle_cast({:multiply, new_value}, state), do: {:noreply, state * new_value}
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
    
    defrecord TestRecord, a: nil
    actor Actor do
      defcast set(_, x), do: new_state(x)
      defcall get(state), do: state
      
      defcall test_pattern_arg(_, 1), do: :one
      def handle_call({:test_pattern_arg, 2}, _, state), do: {:reply, :two, state}
      defcall test_pattern_arg(_, any), do: any
      
      defcall test_pattern_state(1), do: :one
      impcall test_pattern_state(2), do: :two
      
      defcast test_pattern_arg2(_, 1), do: new_state(:one)
      defcast test_pattern_arg2(_, 2), do: new_state(:two)
      
      defcast test_pattern_state2(3), do: new_state(:three)
      impcast test_pattern_state2(4), do: new_state(:four)
      
      defcast test_pattern_state3(TestRecord[a: nil]), do: new_state(:anil)
      impcast test_pattern_state3(_), do: new_state(:a1)
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
      
      assert actor.set(PatternMatching.TestRecord.new(a: nil)).test_pattern_state3.get == :anil
      assert actor.set(PatternMatching.TestRecord.new(a: 1)).test_pattern_state3.get == :a1
    end
  end
  '''
end