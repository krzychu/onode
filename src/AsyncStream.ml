open Async ;;

module In = struct
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

    let read (requested_len : int) (is : t) : bytes async = 
        let out = Buffer.create requested_len in
        let rec aux remaining = 
            if remaining = 0
                then return (Buffer.to_bytes out)
                else read_if_needed remaining is >>= fun ar ->
                    let n = min (ByteQueue.length is.queue) remaining in
                    Buffer.add_bytes out (ByteQueue.pop n is.queue);
                    aux (remaining - n)
        in 
        aux requested_len;
    ;;

    exception End_of_stream;;

    let read_line (max_len : int) (is : t) : bytes async = 
        let out = Buffer.create max_len in
        let rec aux prev =
            read_if_needed 1 is >>= fun k ->
            if k = 0 then raise End_of_stream;
            match (prev, ByteQueue.pop_one is.queue) with
                | None     , '\n' -> return ""
                | None     , curr -> aux (Some curr)
                | Some '\r', '\n' -> return (Buffer.to_bytes out)
                | Some prev, '\n' -> Buffer.add_char out prev; return (Buffer.to_bytes out)
                | Some prev, curr -> Buffer.add_char out prev; aux (Some curr)
        in
        aux None
     ;; 
end
