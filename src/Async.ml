type 't cont = 't -> unit
type 't async = Scheduler.t -> 't cont -> unit

let return (x : 't) : 't async = fun _ tcont -> tcont x

let await_read (fd : Scheduler.file_descr) = fun sch fdcont ->
    Scheduler.schedule sch (Scheduler.Read fd) fdcont

let await_write (fd : Scheduler.file_descr) = fun sch fdcont ->
    Scheduler.schedule sch (Scheduler.Write fd) fdcont

let (>>=) (x : 't async) (f : 't -> 'r async) : 'r async = fun sch rcont -> 
    let tcont t = (f t) sch rcont in x sch tcont

let (>>?) (x : 't option async) (f : 't -> 'r option async) : 'r option async = fun sch rcont -> 
    let tcont t = match t with
        | Some t -> (f t) sch rcont
        | None -> ()
    in x sch tcont

let shutdown : unit async = fun sch ucont ->
    Scheduler.schedule sch Scheduler.Shutdown (fun fd -> ucont ())

let run (sch : Scheduler.t) (t : unit async) = t sch (fun () -> ())
