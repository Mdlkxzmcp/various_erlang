# Compile
> c("<file-name>", [{outdir, "./build"}]).


# Distributed
erl -sname <name> -setcookie <cookie>
> net_adm:ping(<node>).
> rpc:call(<node>, <module>, <function>, [<arguments>]).


# Running tests
erl -noshell -sasl errlog_type error -pa out/ -eval 'eunit:test(<module-with-tests>,[{verbose,true}]),init:stop().'


# Which process uses the most memory:
lists:reverse(lists:keysort(2,[{P, erlang:process_info(P, heap_size)} || P <- erlang:processes()])).

# Which ets table uses the most memory:
lists:reverse(lists:keysort(2,[{T, ets:info(T, memory)} || T <- ets:all()])).



# Overall stuff about erl ($ man erl)

## erl -<flag-name> <arg-0> [--/-extra] <arg-1> <arg-2>
> init:get_argument(<flag-name>).
<arg-0>
> init:get_plain_arguments().
<arg-1> <arg-2>

erl -detached (-noinput) - start erl detached from console

erl -noinput (-noshell) - ensures no input scans

erl -noshell - starts erl process without a shell (useful for Unix piping)

erl -proto_dist `inet_tls` - dist over TLS/SSL (Using SSL for Erlang Distribution)

erl -hidden - hidden from nodes not in same global group

erl -remsh <node> - starts connected to node

erl -rsh <program> - start slave node on a remote host (slave(3erl).)

erl +P <number> - set limit on processes (default: 262144)

erl - man <module> - display the man for given module

## In erl (help().)
> b(). - bindings
> f/0/1. - forget binding(s)
> ih(). - help for module
> mm(). - modified modules
> il(). - interpreted modules
