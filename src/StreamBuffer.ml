type t = {
    fd : Unix.file_descr;
    buffer : char Queue.t;
} ;;

let create fd = {
    fd = fd;
    buffer = Queue.create;
} ;;
