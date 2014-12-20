module Scheduler = struct
    type async_task = 
        | Read of (Unix.file_descr * (Unix.file_descr -> async_task))
        | Write of (Unix.file_descr * (Unix.file_descr -> async_task)) 
        | Done 

    type async_callback = Unix.file_descr -> async_task
    type callback_hash = (Unix.file_descr, async_callback) Hashtbl.t

    let pending_read : callback_hash = Hashtbl.create 0
    let pending_write : callback_hash = Hashtbl.create 0

    let schedule (t : async_task) = match t with
        | Done -> ()
        | Read (fd, callback) -> Hashtbl.add pending_read fd callback
        | Write (fd, callback) -> Hashtbl.add pending_write fd callback

    let timeout = 0.1

    (*
    let rec go () =
        let to_read = all_keys pending_read in
        let to_write = all_keys pending_write in

        let r, w, _ = Unix.select to_read to_write [] timeout in

        go () ;; *)
end

let pflush str = Printf.printf str; flush stdout;;

let addr = Unix.ADDR_INET (Unix.inet_addr_any, 12345) ;;
let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 ;;

Unix.setsockopt sock Unix.SO_REUSEADDR true;;
Unix.set_nonblock sock ;;

Unix.bind sock addr;;
Unix.listen sock 10;;

pflush "waiting for connection\n" ;;
let r, w, ex = Unix.select [sock] [] [] (-1.) ;; 

pflush "someone connected\n" ;;
let (client, addr) = Unix.accept sock ;;

