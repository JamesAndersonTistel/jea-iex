# jea-iex
I want to be able to filter out lines out output that I am not interested in seeing (warnings etc) when using elixir's iex command. 

If you are working on a live machine and want to do some context debugging with `iex --pry` it can get quite annoying if there are tons of prints happending. I could not find a way to filter them out inside iex (it might be there), so I made the `iex` filter tool.

# usage
`run-jea-iex`

when the `jea-iex` mode is on, you can hit F5 or call `jea-iex-cli-filter-on-toggle` to change the state from:
- `off` no filter just like plain `iex`
- `warnings_and_errors` just the warnings and errors
- `all_prints` or everything

it should work identically to `iex` with the exception that shell output that matches in the function `jea-iex-filter-line-process` will be removed. Any line that does not match will come out as before. You can evaluate the function C-x, C-e to change the behaviour live during dev. 

# acknowledgment
based on:
https://www.masteringemacs.org/article/comint-writing-command-interpreter
