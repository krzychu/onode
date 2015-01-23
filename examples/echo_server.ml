open Async;;
open AsyncStream;;
open AsyncServer;;

let handler log addr istream ostream = 
    In.read_line 100 istream >>= fun ol ->
    match ol with
        | None -> 
                return ()
        | Some x -> 
                Out.write_all x ostream >>= fun () ->
                Out.write_all "\n" ostream >>= fun () ->
                log Log.Info x
;;

let log_fd = Log.openfile "log.txt" 

let start_info : AsyncServer.startinfo = {
    AsyncServer.port = 12345;
    log_fd = log_fd;
    log_level = Log.Debug;
    handler = handler; 
};;

AsyncServer.start start_info
