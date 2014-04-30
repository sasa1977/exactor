# v.0.3.2
- adapted to Elixir v0.13.1 (backwards compatible with 0.13.0)

# v.0.3.1
- "dummy" empty version due to Hex limitations

# v.0.3.0
- removed implicit "smart" returns
- removed default `use ExActor`

# v0.2.1

- migrated to Elixir v0.13.0

# v0.2.0

This version introduces some significant changes. If you don't want to incorporate them, there is a tag for 0.1. Keep in mind that I won't maintain the 0.1, so your best option is to fork it and maintain it yourself.

On the plus side, the migration to the new version should not be very complicated (unless you're relying on tuple modules support). Here are quick pointers for migrating from old API to the new one:

## Predefines

The point of predefines is to make the decision about default implementations explicit. Earlier, you used `use ExActor` and the default implementation was implicit. Now you have to decide which implementation do you want, or you can easily make your own. If you want to keep the status quo, just use `use ExActor.GenServer`.

## Deprecated implicit "smart" returns

```elixir
definit do: some_state                  # deprecated
definit do: initial_state(some_state)

defcall op, do: response                # deprecated
defcall op, do: reply(response)

defcast op, do: :ok                     # deprecated
defcast op, do: noreply

definfo msg, do: :ok                    # deprecated
definfo msg, do: noreply
```

Beside these "special" responses, you can use standard `gen_server` responses. The reason for this change was again to make things more explicit, and aligned with standard `gen_server` approach.

## Dropped tuple modules support

There's no way around this - if you use ExActor tuple modules support, you need to change the code to classical functional approach, or remain on version 0.1. The tuple modules support was more a hack, to make some code in blog articles more OO-ish. In hindsight, this was a mistake that made the ExActor code more complicated, so I decided to drop it.