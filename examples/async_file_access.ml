open AsyncStream ;;
open Async;;

let fd = Unix.openfile "examples/heart_of_darkness.txt" [Unix.O_RDONLY; Unix.O_NONBLOCK] 0 ;;

let stream = In.create fd 1024 ;;

let line_length = 256

let cat =
    let rec aux line =
        Printf.printf "%s\n" line; 
        flush stdout;
        In.read_line line_length stream >>= aux
    in
    In.read_line line_length stream >>= aux
;;

let sch = Scheduler.create 0.1 ;;
run sch cat;
Scheduler.go sch;
