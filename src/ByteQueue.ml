type t = {
    data: bytes;
    start: int ref;
    length: int ref;
} ;;

let create size = {
    data = Bytes.create size;
    start = ref 0;
    length = ref 0;
} ;;



let capacity queue = Bytes.length queue.data

let length queue = !(queue.length)

let space queue = (capacity queue) - (length queue)

let start_ptr queue = !(queue.start)

let move_start delta queue = 
    queue.start := ((start_ptr queue) + delta) mod (capacity queue);
    queue.length := (length queue) - delta;
;;

let stop_ptr queue = ((length queue) + (start_ptr queue)) mod (capacity queue)

exception Queue_overflow ;;

let intervals start len queue = 
    let cap = capacity queue in 
    let start = start_ptr queue in
    let stop = start + len in

    let p, q = 
        if stop < cap
            then (len, 0)
            else (cap - start, stop - cap)
    in
    (start, p, 0, q)


let push b offset len queue = 
    if space queue < len then raise Queue_overflow ;
    
    let stop = stop_ptr queue in
    let (o1, l1, o2, l2) = intervals stop len queue in

    if l1 > 0 then Bytes.blit b offset queue.data o1 l1;
    if l2 > 0 then Bytes.blit b (offset + l1) queue.data o2 l2;
    queue.length := (length queue) + len;
;;

exception Queue_underflow ;; 

let pop len queue =
    if length queue < len then raise Queue_underflow;

    let buf = Bytes.create len in
    let stop = stop_ptr queue in
    let (o1, l1, o2, l2) = intervals stop len queue in
    
    if l1 > 0 then Bytes.blit queue.data o1 buf 0 l1;
    if l2 > 0 then Bytes.blit queue.data o2 buf l1 l2;

    move_start len queue;
    buf
;;

let pop_one queue = 
    if length queue < 1 then raise Queue_underflow;
   
    let start = start_ptr queue in
    let ch = Bytes.get queue.data start in

    move_start 1 queue;
    ch
;;
