{-
in ghci
`it` will print out the last return value

`:set +t` will print out the type of every last line
`:unset +t` will remove it

`:type` or `:t` to see type for one instance (variable)

`:kind` or `:k` to see kind of type (Num, etc)

`:info` of `:i` to see more info of most things

`:m` or `:module` with + to load specifed modules, - to subtract specified modules, omit + or - to fully replace all
loaded modules (or just :m to remove all modules)
`:m Control.Monad Data.Ratio`

`:load` or `:l` <filename> to load a file

`:reload` or `:r` to reload if you made a change

to start a multi-line function, etc, use
:{ to start on separate line
...code
:} to end on separate line


`:help`
`:?`
`:h`
all get help

`:! <shell command>` will execute a shell command inside ghci (ls, mkdir, etc...)

`it` most recently returned value (like `v` in iex)

`:browse` or `:bro` <module> to see the exported functions in a module

`:quit` or `:q` to quit ghci
-}
