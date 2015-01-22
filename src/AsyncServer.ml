open Async;;
open AsyncStream;;

type handler = string -> In.t -> Out.t -> unit async ;;

let get_readable_addr sa = match sa with
    | Unix.ADDR_INET (iaddr, port) -> Unix.string_of_inet_addr iaddr, port
    | Unix.ADDR_UNIX str -> (str, 0)


let start (port : int) (h : handler) =
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt socket Unix.SO_REUSEADDR true;
    Unix.set_nonblock socket;

    let addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
    Unix.bind socket addr;
    Unix.listen socket 10;

    let sch = Scheduler.create 0.1 in
    
    let rec main_loop () = 
        await_read socket >>= fun fd -> 
        let client_sock, client_addr = Unix.accept fd in
        let istream = In.create client_sock 1024 in
        let ostream = Out.create client_sock 1024 in
        
        let addr, port = get_readable_addr client_addr in
        Printf.printf "connection from %s port %d\n" addr port;
        flush stdout; 
        
        let task = 
            h addr istream ostream >>= fun () -> 
            Out.close ostream 
        in

        run sch task;
        return () >>= main_loop
    in
    run sch (main_loop ());
    Printf.printf "listening on port %d\n" port;
    flush stdout; 
    Scheduler.go sch;
;;
