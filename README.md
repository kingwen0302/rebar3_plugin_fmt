fmt
=====

A rebar plugin, format erlang code

Use
---

Add the plugin to your rebar config:

    {plugins, [
        %% { fmt, ".*", {git, "http://git.mingilin.com:3000/kingwen0302/rebar3_plugin_fmt.git", {tag, "master"}}}
        { fmt, ".*", {git, "https://github.com/kingwen0302/rebar3_plugin_fmt.git", {tag, "master"}}}
    ]}.
    
    %% fmt config options
    {fmt_opts, [
        %% default match
        {"default", [
            %% erl_tidy:dir/2 Options expcept printer
            {backups, false},
            
            %% erl_prettypr:format Options 
            %% add {indent, space|tab}
            {printer, [{paper, 100}, {ribbon, 80}, {indent, space}]}
        ]},
        {"mycfg", [
            {backups, false},
            {printer, [{paper, 100}, {ribbon, 80}, {indent, space}]}
        ]}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 fmt
    $ rebar3 fmt default
    $ rebar3 fmt mycfg
