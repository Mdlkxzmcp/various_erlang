# Kent Erlang Master Class
The code here follows the lectures given in [the Erlang Master Class course from University of Kent](https://www.cs.kent.ac.uk/ErlangMasterClasses).

Most of the code is commented and has its types declared, although I have not checked with Dialyzer if they are indeed correct. It's more of an excercise for getting the hang of types in Erlang than anything else (type related).

As Erlang doesn't treat documentation with such high care as Elixir for example, I have tried to apply a bit of my own style when documenting after a bit of experimenting. Probably doesn't follow the exact standard, but (for now) I like it. It goes like this:
* `%%% Types` – module documentation including section separation.
* ```
%%
%% @doc function/arity
%%
%% Documentation for exported functions.
%%Might include an example of use:
%% For example,
%%  function(arguments). is
%%  result
```
* ```
%% @doc function/arity
%% Documentation for auxiliary functions.
```
* `% Code comment.`

Another thing I tried to do is to create space between sections. It works nicely, but by no means do I believe that it's the only way.
