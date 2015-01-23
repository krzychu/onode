open Async;;
open AsyncStream;;

type level = 
    | All
    | Debug
    | Error
    | Fatal
    | Info
    | Off
;;


let level_code l = match l with
    | All   -> 0 
    | Debug -> 1
    | Error -> 2 
    | Fatal  -> 3 
    | Info  -> 4
    | Off   -> 5
;;
 

type t = level -> string -> unit async ;;

let openfile path = Unix.openfile path [Unix.O_WRONLY; Unix.O_NONBLOCK; Unix.O_CREAT; Unix.O_TRUNC] 0o640 ;;

let create (fd : Unix.file_descr) (min_level : level) (prefix : string) : t =
    let ostream = Out.create fd 1024 in
    let min_code = level_code min_level in

    fun l s -> 
        if level_code l < min_code 
            then return ()
            else
                let msg = String.concat "" [prefix; " : "; s; "\n"] in
                Out.write_all msg ostream >>= fun () ->
                Out.flush ostream 
;;
