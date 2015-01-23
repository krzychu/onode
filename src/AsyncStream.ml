open Async ;;

module Out = struct
    type t = {
        fd: Unix.file_descr;
        flush_threshold: int;
        buffer: Buffer.t;
    };;

    let create fd flush_threshold = {
        fd = fd;
        flush_threshold = flush_threshold;
        buffer = Buffer.create (2 * flush_threshold)
    };;

    let rec send fd buf offset len = 
        if len = 0
            then return ()
            else
                await_write fd >>= fun fd ->
                let num_written = Unix.write fd buf offset len in
                send fd buf (offset + num_written) (len - num_written)
    ;;

    let flush (os : t) : unit async = 
        let len = Buffer.length os.buffer in
        let all = Buffer.to_bytes os.buffer in
        Buffer.reset os.buffer;
        send os.fd all 0 len
    ;;

    let close (os : t) : unit async = 
        flush os >>= fun () ->
        Unix.close os.fd;
        return ()
    ;;

    let write (d : bytes) (offset : int) (len : int) (os : t) : unit async = 
        Buffer.add_subbytes os.buffer d offset len; 
        if Buffer.length os.buffer > os.flush_threshold 
            then flush os
            else return ()
    ;;

    let write_all (d : bytes) (os : t) : unit async = 
        write d 0 (Bytes.length d) os
    ;;
end

module In = struct

    let return_bytes x = return (Some x)
    let return_buffer x = return_bytes (Buffer.to_bytes x)

    type t = {
        fd: Unix.file_descr;
        chunk_size: int;
        queue: ByteQueue.t;
    };;

    let create fd chunk_size = {
        fd = fd;
        chunk_size = chunk_size;
        queue = ByteQueue.create (2 * chunk_size);
    };;

    let close (os : t) : unit async = 
        Unix.close os.fd;
        return ()
    ;;

    let read_more (is : t) : int async = 
        await_read is.fd >>= fun fd -> 
        let chunk = Bytes.create is.chunk_size in 
        let num_read = Unix.read fd chunk 0 is.chunk_size in
        ByteQueue.push chunk 0 num_read is.queue;
        return num_read
    ;;

    let read_if_needed (size : int) (is : t) : int async = 
        if ByteQueue.length is.queue >= size
            then return size
            else read_more is
    ;;

    let read (requested_len : int) (is : t) : bytes option async = 
        let out = Buffer.create requested_len in
        let rec aux remaining = 
            if remaining = 0
                then return_buffer out
                else read_if_needed remaining is >>= fun ar ->
                    if ar = 0 then return None else
                        let n = min (ByteQueue.length is.queue) remaining in
                        Buffer.add_bytes out (ByteQueue.pop n is.queue);
                        aux (remaining - n)
        in 
        aux requested_len;
    ;;

    let read_all (is : t) : bytes async = 
        let buf = Buffer.create 0 in

        let copy_all () =
            let len = ByteQueue.length is.queue in
            Buffer.add_bytes buf (ByteQueue.pop len is.queue);
        in
        copy_all ();

        let rec aux () = 
            read_more is >>= fun cnt ->
            if cnt = 0 
                then 
                    return (Buffer.to_bytes buf)
                else 
                    return (copy_all ()) >>= fun () -> 
                    aux ();
        in
        aux ()
    ;;   

    let read_line (max_len : int) (is : t) : bytes option async = 
        let out = Buffer.create max_len in
        let rec aux prev =
            read_if_needed 1 is >>= fun k ->
            if k = 0 then return None else 
                match (prev, ByteQueue.pop_one is.queue) with
                    | None     , '\n' -> return_bytes "" 
                    | None     , curr -> aux (Some curr)
                    | Some '\r', '\n' -> return_buffer out
                    | Some prev, '\n' -> Buffer.add_char out prev; return_buffer out
                    | Some prev, curr -> Buffer.add_char out prev; aux (Some curr)
        in
        aux None
     ;; 
end
