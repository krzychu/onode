open Async;;
open AsyncStream;;

let handler addr istream ostream = 
    In.read_line 100 istream >>= fun ol ->
    match ol with
        | In.EOF -> 
                return ()
        | In.Just x -> 
                Out.write_all x ostream >>= fun () ->
                Out.write_all "\n" ostream 
;;

AsyncServer.start 12345 handler;
