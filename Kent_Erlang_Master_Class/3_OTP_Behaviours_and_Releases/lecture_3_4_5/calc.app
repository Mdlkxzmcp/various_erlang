{application, calc, [
    {description, "Calc application"},
    {vsn, "1.0.0"},
    {modules, [calc, calc_app, calc_sup, expr]},
    {registered, [calc_sup, calc]},
    {applications, [kernel, stdlib]},
    {env, [{env, [{a, 23}, {b, -12}]}]},
    {mod, {calc_app, []}},
    {maintainers, []},
    {licenses, ["Apache 2.0"]},
    {links, []}
]}.
