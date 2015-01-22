open Async;;
open AsyncStream;;

module Request = struct

    type request_method = 
        | Options
        | Get
        | Head
        | Post
        | Put
        | Delete
        | Trace
        | Connect
        | Custom of string
    ;;

    type header_field = 
        | Other of string
    ;;

    type t = {
        meth : request_method;
        header : (header_field, string) Hashtbl.t;
        istream : In.t;
        addr : string;
    } ;;

    let read (addr : string) (is : In.t) : t async = return ({ 
        meth = Get;
        header = Hashtbl.create 10;
        istream = is;
        addr = addr;
    }) ;;
end 


module Response = struct
    type status_code = 
        | OK
        | BadRequest
        | NotImplemented
        | InternalError
        | Number of int
    ;;

    type t = {
        code: status_code;
        header: (string, string) Hashtbl.t;
    } ;;

    let write response os = return ()
end

type handler = Request.t -> (Response.t -> Out.t async) -> unit async

let async_handler (h : handler) : AsyncServer.handler = fun addr is os -> 
    Request.read addr is >>= fun request -> 
    let sender response_header = 
        Response.write response_header os >>= fun () -> 
        return os
    in
    h request sender
;;
