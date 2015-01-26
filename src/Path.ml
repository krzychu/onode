let delimiter_rx = Str.regexp " */+ *" ;;
let split url = Str.split delimiter_rx url ;;


let is_safe parts = 
    parts 
    |> List.exists (fun x -> List.mem x ["~"; ".."]) 
    |> not
;;


exception Too_short;;
let last parts = 
    let i = List.length parts - 1 in
    if i >= 0 then List.nth parts i else raise Too_short
;;


exception No_extension;;
let dot_rx = Str.regexp_string "." ;;
let get_extension parts = 
    try 
        last parts 
        |> Str.split dot_rx 
        |> fun l -> if List.length l < 2 then raise No_extension else l
        |> last
    with
        | _ -> raise No_extension
;;


let join parts = String.concat "/" parts ;;
