open Async ;;

module In = struct
    (*
    let buffer_size = 4096

    type t = .t
   
    let read_more (is : t) : unit async = 
        await_read is.fd >>= fun fd -> 
        let buffer = Bytes.create buffer_size in 
        let num_read = Unix.read fd buf 0 buffer_size in

        return () ;;

    let read (requested_len : int) (is : t) : bytes async = 
        let out = Bytes.create requested_len in
        let rec aux offset = 
            let remaining = requested_len - offset in
            let copied = blit is out offset remaining in 
            if copied < remaining then
                read_more is >>= fun () -> aux (offset + copied) 
            else
                return out
        in 
        aux 0
        *)

end
