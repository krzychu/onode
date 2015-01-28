# ONode
Asynchronous web server written in OCaml.

## Scope
This project aimed to recreate a small subset of Nodejs functionality in OCaml for
educational purposes. It contains:

* Asynchronous IO scheduler
* Asynchronous IO streams
* Simple Http library

## How to read the code?
I recommend to look at examples first, and then move to `Scheduler` and `Async` modules. 
When you grasp what is going on in there there is no need to read rest of the modules, 
because they are doing mundane things using abstractions provided by those two. 

## Building
Fortunately, no external libraries besides OUnit are needed. Easiest way of installing this
package is to use opam:
```
opam install OUnit
```

In order to compile ONode you have to go to root directory of repository and issue following 
commands:
```
ocaml setup.ml -configure
ocaml setup.ml -build
```

## Executables
* `async_file_access.byte` - When started in repository root prints Joseph Conrad "Heart of Darkness" on the screen.
* `echo_server.byte` - Asynchronous echo server on port 12345.
* `http_server.byte` - Simple asynchronous http server on port 12345.
* `static_file_server.byte` - Serves static files from a directory where it was started. Try it in `test_page`.
* `test_runner.byte` - Runs several unit tests.
