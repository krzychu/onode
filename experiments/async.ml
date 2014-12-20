module Scheduler = struct
    type file_descr = Unix.file_descr 
    type action = (file_descr -> unit)
    type event = Read | Write | Exception

    type action_list = (file_descr, action) Hashtbl.t
    type t = action_list * action_list * action_list

    let create () : t = (Hashtbl.create 0, Hashtbl.create 0, Hashtbl.create 0)

    let schedule (sch : t) (fd : file_descr) (ev : event) (f : action) =
        let (r, w, e) = sch in
        let tab = match ev with
            | Read -> r
            | Write -> w
            | Exception -> e
        in
        Hashtbl.add tab fd f ;;
end

module Async = struct
    type 't cont = 't -> unit
    type 't async = Scheduler.t -> 't cont -> unit

    let return (x : 't) : 't async = fun _ tcont -> tcont x

    let await_read (fd : Scheduler.file_descr) : Scheduler.file_descr async = fun sch fdcont ->
        Scheduler.schedule sch fd Scheduler.Read fdcont

    let await_write (fd : Scheduler.file_descr) : Scheduler.file_descr async = fun sch fdcont ->
        Scheduler.schedule sch fd Scheduler.Write fdcont

    let (>>=) (x : 't async) (f : 't -> 'r async) : 'r async = fun sch rcont -> 
        let tcont t = (f t) sch rcont in x sch tcont

    let run (sch : Scheduler.t) (t : unit async) = t sch (fun () -> ())
end
