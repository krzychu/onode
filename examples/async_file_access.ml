open AsyncStream ;;
open Async;;

let fd = Unix.openfile "ttt" [Unix.O_RDONLY; Unix.O_NONBLOCK] 0 ;;

let stream = In.create fd 1024 ;;

let cat = 
    In.read 4 stream >>= fun x -> 
    Printf.printf "--> %s\n" x ; return (flush stdout) >>= fun () ->
    In.read 4 stream >>= fun x -> 
    Printf.printf "--> %s\n" x ; return (flush stdout) >>= fun () ->
    return (Unix.close fd) >>= fun () ->
    shutdown ;;

let sch = Scheduler.create 0.1 ;;
run sch cat;
Scheduler.go sch;
