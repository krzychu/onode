type 't cont = 't -> unit
type 't t = Scheduler.t -> 't cont -> unit

let return (x : 't) : 't t = fun _ tcont -> tcont x

let await_read (fd : Scheduler.file_descr) = fun sch fdcont ->
    Scheduler.schedule sch (Scheduler.Read fd) fdcont

let await_write (fd : Scheduler.file_descr) = fun sch fdcont ->
    Scheduler.schedule sch (Scheduler.Write fd) fdcont

let (>>=) (x : 't t) (f : 't -> 'r t) : 'r t = fun sch rcont -> 
    let tcont t = (f t) sch rcont in x sch tcont

let run (sch : Scheduler.t) (t : unit t ) = t sch (fun () -> ())
