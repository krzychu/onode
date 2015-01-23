open Async;;
open AsyncStream;;
open Http;;


let http_handler log request write_response =
    log Log.Debug "I'm in handler, yay!"        >>= fun () ->
    let r = {
        Response.status = Response.OK;
        Response.header = [
            ("content-type", "text/plain")
        ];
    }
    in
    write_response r                            >>= fun ostream ->
    Out.write_all "all hail to ZOD\n" ostream 
;;

let log_fd = Log.openfile "log.txt" ;;

let async_handler = Http.async_handler http_handler ;;

let start_info = {
    AsyncServer.port = 12345;
    AsyncServer.log_fd = log_fd;
    AsyncServer.log_level = Log.Debug;
    AsyncServer.handler = async_handler;
} ;;

AsyncServer.start start_info;;
