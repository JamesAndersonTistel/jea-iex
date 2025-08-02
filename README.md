# jea-iex
I want to be able to filter out lines out output that I am not interested in seeing (warnings etc) when using elixir's iex command. This is helpful during development when some systems are down and the system is complaining.

# usage
`run-jea-iex`

it should work identically to `iex` with the exception that shell output that matches in the function `jea-iex-filter-line-process` will be removed. Any line that does not match will come out as before. You can evaluate the function C-x, C-e to change the behaviour live during dev. 

# acknowledgment
based on:
https://www.masteringemacs.org/article/comint-writing-command-interpreter
