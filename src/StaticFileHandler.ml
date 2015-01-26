open Async;;
open Http;;
open ErrorMonad;;


let get_content_type extension = 
    match String.lowercase extension with
        | "html" -> Some "text/html"
        | "css"  -> Some "text/css"
        | "js"   -> Some "text/javascript"
        | "txt"  -> Some "text/plain"
        | "gif"  -> Some "image/gif"
        | "png"  -> Some "image/png"
        | "jpeg" -> Some "image/jpeg"
        | "jpg"  -> Some "image/jpeg"
        | _      -> None 
;;


let normalize parts = match parts with
    | []    -> ["index.html"]
    | parts -> parts
;;

(*
let create (path : string) : Http.handler = fun log req write_response -> 
    let respond_with status = 
        write_response (Response.from_status status) >>= fun _ ->
        return ()
    in

    Path.split req.Request.url 
    |> fun parts -> if Path.is_safe parts then Some parts else

        let normalized = normalize parts in
        let extension = Path.get_extension normalized in
        let content_type = get_content_type extension in

        let fd = Unix.openfile [O_RDONLY]

        let headers = [
            ("Content-Type", content_type);
            ("Content-Length")
        ]
;;

*)
