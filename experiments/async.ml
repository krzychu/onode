(* develpment moved to src *)

module Scheduler = struct
    type file_descr = Unix.file_descr 

    type event = 
        | Read of file_descr
        | Write of file_descr
        | Exception of file_descr
        | Shutdown

    type handler = (file_descr -> unit)

    type t = {
        actions : (event, handler) Hashtbl.t;
        timeout : float;
    } ;;

    let create timeout : t = {
        actions = Hashtbl.create 0;
        timeout = timeout;
    } ;;

    let schedule (sch : t) (e : event) (h : handler) = 
        Hashtbl.add sch.actions e h ;;

    let shutdown_scheduled (sch : t) = 
        Hashtbl.mem sch.actions Shutdown ;;

    let split_io (sch : t) =
        let rec aux event _ (r, w, e) = match event with
            | Shutdown          -> (r, w, e)
            | Read c            -> ((c::r), w, e)
            | Write c           -> (r, (c::w), e)
            | Exception c       -> (r, w, (c::e))
        in
        Hashtbl.fold aux sch.actions ([], [], []) ;;

    let select (sch : t) = 
        let r, w, e = split_io sch in
        let ar, aw, ae = Unix.select r w e sch.timeout in
        let mkread x = Read x in
        let mkwrite x = Write x in
        let mkexc x = Exception x in
        [List.map mkread ar; List.map mkwrite aw; List.map mkexc ae] 
            |> List.concat

    let getfd e = match e with
        | Shutdown -> Unix.stderr
        | Read c -> c
        | Write c -> c
        | Exception c -> c
            
    let rec go (sch : t) =
        let ev = select sch in
        let prepare e = 
            let fd = getfd e in
            Hashtbl.find_all sch.actions e |> List.map (fun h -> (fd, h))
        in
        let to_fire = ev |> List.map prepare |> List.concat in
        let fire (fd, h) = h fd in
        let rec remove e = 
            if Hashtbl.mem sch.actions e then 
                Hashtbl.remove sch.actions e;
                remove e;
        in
        ev |> List.iter remove ;
        to_fire |> List.iter fire ;
        go sch ;;
end

module Async = struct
    type 't cont = 't -> unit
    type 't async = Scheduler.t -> 't cont -> unit

    let return (x : 't) : 't async = fun _ tcont -> tcont x

    let await_read (fd : Scheduler.file_descr) = fun sch fdcont ->
        Scheduler.schedule sch (Scheduler.Read fd) fdcont

    let await_write (fd : Scheduler.file_descr) = fun sch fdcont ->
        Scheduler.schedule sch (Scheduler.Write fd) fdcont

    let (>>=) (x : 't async) (f : 't -> 'r async) : 'r async = fun sch rcont -> 
        let tcont t = (f t) sch rcont in x sch tcont

    let run (sch : Scheduler.t) (t : unit async) = t sch (fun () -> ())
end

module AsyncStream = struct

end
