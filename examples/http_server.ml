open Async;;


let http_handler request write_response =
    Printf.printf "kek";
    return ()
;;


let async_handler = Http.async_handler http_handler ;;

AsyncServer.start 12345 async_handler
