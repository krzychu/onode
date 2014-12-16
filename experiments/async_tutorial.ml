open Async.Std;;
open Core.Std;;

let () = Reader.file_contents "test.md" >>> (fun x -> printf "%s\n" x; shutdown 0)
let () = never_returns (Scheduler.go ())
