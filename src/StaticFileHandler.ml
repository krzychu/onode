open Async;;
open AsyncStream;;
open Http;;


exception Unknown_content;;
let get_content_type extension = 
    match String.lowercase extension with
        | "html" -> "text/html"
        | "css"  -> "text/css"
        | "js"   -> "text/javascript"
        | "txt"  -> "text/plain"
        | "gif"  -> "image/gif"
        | "png"  -> "image/png"
        | "jpeg" -> "image/jpeg"
        | "jpg"  -> "image/jpeg"
        | _      -> raise Unknown_content
;;


let normalize parts = match parts with
    | []    -> ["index.html"]
    | parts -> parts
;;


let create () : Http.handler = fun log req write_response -> 
    let respond_with status =
        write_response (Response.from_status status)
    in

    let fail_with ex status = 
        log Log.Error (Printexc.to_string ex)   >>= fun () ->
        respond_with status                     >>= fun _  ->
        return None
    in

    let try_get_target req = 
        try
            let parts = Path.split req.Request.url in
            Path.ensure_safe parts;
            let normalized = normalize parts in
            let extension = Path.get_extension normalized in
            let content_type = get_content_type extension in
            let path = Path.join normalized in
            return (Some (content_type, path))
       with 
            | e -> fail_with e Response.NotFound
    in

    let try_open_fd path = 
        try
            let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_NONBLOCK] 0 in
            return (Some fd)
        with 
            | e -> fail_with e Response.NotFound
    in

    let try_read_all fd =
        let istream = In.create fd 1024 in
        In.read_all istream                     >>= fun content ->
        In.close istream                        >>= fun () ->
        return (Some content)
    in

    let try_respond content_type content =
        let content_length = string_of_int (Bytes.length content) in

        let headers = [
            ("Content-Type", content_type);
            ("Content-Length", content_length)
        ]
        in

        let response = Response.create Response.OK headers in
    
        write_response response                 >>= fun ostream -> 
        Out.write_all content ostream           >>= fun _ ->
        return (Some ())
    in


    let out =
        try_get_target req                      >>? fun (content_type, path) ->
        try_open_fd path                        >>? fun (fd) ->
        try_read_all fd                         >>? fun (content) ->
        try_respond content_type content            
    in

    out >>= fun _ -> return ()
;;
