open AsyncStream ;;
open Async;;

let infd = Unix.openfile "examples/heart_of_darkness.txt" [Unix.O_RDONLY; Unix.O_NONBLOCK] 0 ;;
let outfd = Unix.openfile "copy.txt" [Unix.O_WRONLY; Unix.O_NONBLOCK] 0 ;;

let istream = In.create infd 1024 ;;
let ostream = Out.create outfd 1024 ;;

let line_length = 256

let cat =
    let rec aux line = match line with
        | Some line ->
                Printf.printf "%s\n" line; 
                flush stdout;
                Out.write_all "hehe: " ostream >>= fun () ->
                Out.write_all line ostream >>= fun () ->
                Out.write_all "\n" ostream >>= fun () ->
                In.read_line line_length istream >>= aux
        | None ->
                In.close istream >>= fun () -> 
                Out.close ostream >>= fun () ->
                shutdown
    in
    In.read_line line_length istream >>= aux 
;;

let sch = Scheduler.create 0.1 ;;
run sch cat;
Scheduler.go sch;
