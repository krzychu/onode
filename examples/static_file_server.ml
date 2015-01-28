open Async;;
open AsyncStream;;
open Http;;

let log_fd = Log.openfile "log.txt" ;;

let async_handler = Http.async_handler (StaticFileHandler.create () );;

let start_info = {
    AsyncServer.port = 12345;
    AsyncServer.log_fd = log_fd;
    AsyncServer.log_level = Log.Debug;
    AsyncServer.handler = async_handler;
} ;;

AsyncServer.start start_info;;
