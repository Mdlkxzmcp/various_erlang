# Designing for Scalability with Erlang/OTP.
Notes.

---
## Chapter 2:

### Macros
To see the effects of macros -> c([filename, ['P']).

This creates a filename.P file.


### Upgrading modules
Only two versions of the code are held in the system. So in the case of `-v(1)`, `-v(2)` and `-v(3)`, the `-v(1)` is purged with any processes still running it with an unconditional termination.

An upgrade is initialized when there is a fully qualified call to a function within the upgraded module – i.e. `B:loop()`. Otherwise, the process will run the old code.

There are multiple ways of loading the new code – `c(Module)` in the shell will compile and reload, likewise, `compile:file(Module)` is a functional way of doing this. Alternatively, code can be loaded explicitly in the shell with `l(Module)` or by a call to `code:load_file(Module)`.

`code:purge(Module)` explicitly purges an old version (without loading a new version). This terminates all processes running the old code before removing the code. Returns `true` if processes were terminated, `false` otherwise. `code:soft_purge(Module)` does the same, but only if no processes were running the code. Returns `true` if that is the case, `false` otherwise.


### ETS
Table is linked to the process that creates it, and deleted when that process terminates. `DETS` are available for `D`isk storage.

* Set – each key-value tuple can occur only once. `O(n)`

* Ordered set – same restrictions as with sets, but the tuples can be visited in order by key. `O(log n)`

* Bag – each key-value tuple combination can only occur once, but a key can appear multiple times. `O(n)`

* Duplicate bag – tuples can be duplicated. `O(n)`


### Distributed Erlang
The `net_kernel` module allows for fine-grained control of interconnections. In a combination with the `-hidden` flag, this creates a variety of uses, including operations and maintnance, as well as creating bridges between node clusters. They are still detectable with `> nodes(hidden)`.

---
