open Async;;
open AsyncStream;;

module Request = struct

    type request_method = string ;;

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


    let parse_request_method = String.uppercase ;;


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
    type status = 
        | OK
        | BadRequest
        | NotFound
        | Code of int
    ;;


    let status_to_int s = match s with
        | OK -> 200
        | BadRequest -> 400
        | NotFound -> 404
        | Code x -> x
    ;;


    let status_to_reason_phrase s = match s with
        | OK -> "Awesome"
        | BadRequest -> "WTF"
        | NotFound -> "..."
        | Code _ -> "look it up yourself"
    ;;


    type t = {
        status: status;
        header: (string * string) list; 
    } ;;


    let create status header = {
        status = status;
        header = header; 
    } ;;


    let from_status status = create status [] 

    
    let write_status_line status os =
        let code = status_to_int status in
        let phrase = status_to_reason_phrase status in
        let line = Printf.sprintf "HTTP/1.0 %d %s\r\n" code phrase in
        Out.write_all line os
    ;;


    let write_status_header_line p os =
        let k, v = p in
        let line = Printf.sprintf "%s : %s\r\n" k v in
        Out.write_all line os
    ;;


    let write_header header os = 
        sequence header (fun p -> write_status_header_line p os) >>= fun () ->
        Out.write_all "\r\n" os
    ;;


    let write response os = 
        write_status_line response.status os >>= fun () ->
        write_header response.header os
    ;;

end


type handler = Log.t -> Request.t -> (Response.t -> Out.t async) -> unit async


let async_handler (h : handler) : AsyncServer.handler = fun log addr is os -> 
    Request.read addr is >>= fun request -> 
    let sender response_header = 
        Response.write response_header os >>= fun () -> 
        return os
    in
    match request with
        | None -> 
                log Log.Debug "Bad Request" >>= fun () -> 
                Response.write (Response.from_status Response.BadRequest) os
        | Some r -> 
                log Log.Debug (Printf.sprintf "%s %s" r.Request.meth r.Request.url) >>= fun () ->
                h log r sender
;;
