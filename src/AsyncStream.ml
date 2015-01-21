open Async ;;

module In = struct
    type t = {
        fd : Unix.file_descr;
        buffer : bytes;
        buflen : int ref;
    };;

    let create fd chunk_size = {
        fd = fd;
        buffer = Bytes.create (2 * chunk_size);
        buflen = ref 0;
    } ;;

    let chunk_size (is : t) : int = (Bytes.length is.buffer) / 2

    let blit (is : t) (dst : bytes) (dstoff : int) (len : int) : int =
        let src_len = !(is.buflen) in
        let to_blit = min src_len len in
        let src = is.buffer in
        Bytes.blit src 0 dst dstoff to_blit;
        Bytes.blit src to_blit src 0 (src_len - to_blit);
        to_blit ;;

    let read_more (is : t) : unit async = 
        await_read is.fd >>= fun fd -> 
        let bl = !(is.buflen) in
        let num_read = Unix.read fd is.buffer bl (chunk_size is) in
        is.buflen := bl + num_read;
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

end
