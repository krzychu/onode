open Async;;
open AsyncStream;;

module Request = struct

    type request_method = 
        | Get
        | Custom of string
    ;;


    type t = {
        meth : request_method;
        header : (string, string) Hashtbl.t;
        istream : In.t;
        addr : string;
        url : string;
    } ;;


    let read_all_header_lines (is : In.t) : string list option async = 
        let rec aux acc = 
            In.read_line 4096 is >>? fun line ->
            match line with
                | "" -> return (Some (List.rev acc))
                | line -> aux (line :: acc)
        in
        aux []
    ;;


    let (>?) (x : 't option) (f : 't -> 'r option) : 'r option =
        match x with
            | None -> None
            | Some x -> f x
    ;; 


    let parse_request_method s = match String.uppercase s with
        | "GET" -> Get
        | s     -> Custom s
    ;;


    let request_line_rx = Str.regexp_case_fold "\\([a-z]+\\) +\\([^ ]+\\)"
    let parse_request_line line =
        let mg k = Str.matched_group k line in
        if Str.string_match request_line_rx line 0
            then Some (parse_request_method (mg 1), mg 2)
            else None
    ;;


    let request_header_line_rx = Str.regexp_case_fold "\\([-a-z]+\\) *: *\\(.*\\)"
    let parse_request_header_line line = 
        let mg k = Str.matched_group k line in
        if Str.string_match request_header_line_rx line 0
            then Some (String.lowercase (mg 1), mg 2)
            else None
    ;;


    let parse_request_header lines = 
        let acc = Hashtbl.create (List.length lines) in
        let rec aux xs = match xs with
            | [] -> Some acc
            | None :: _ -> None
            | (Some (u, v)) :: xs -> 
                    Hashtbl.add acc u v;
                    aux xs
        in
        lines 
        |> List.rev_map parse_request_header_line
        |> aux
    ;;


    let parse lines =
        match lines with
            | [] -> None
            | x :: xs -> 
                    parse_request_line x >? fun (meth, url) ->
                    parse_request_header xs >? fun header -> 
                    Some (meth, url, header)
    ;;


    let read (addr : string) (is : In.t) : t option async = 
        read_all_header_lines is >>? fun lines ->
        match parse lines with
            | None -> return None
            | Some (meth, url, header) -> return ( 
                Some { 
                    meth = meth;
                    header = header;
                    istream = is;
                    addr = addr;
                    url = url;
                }) 
    ;;
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

    let from_code code = {
        code = code;
        header = Hashtbl.create 0;
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
    match request with
        | None -> Response.write (Response.from_code Response.BadRequest) os
        | Some request -> h request sender
;;
