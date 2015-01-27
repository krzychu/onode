open OUnit;;

let assert_exception (e : exn) (x : 't) (f : 't -> 'r) : unit = 
    try
        let _ = f x in
        ()
    with
        | x -> assert_equal x e
;;


let tests = "Path Tests" >::: [
    "Splits paths correctly" >:: (fun cxt -> 
        assert_equal ["a"; "b"; "c"] (Path.split "a   / b/c")
    );

    "Checks path safety" >:: (fun cxt ->
        assert_equal true  (Path.split "a/b/c" |> Path.is_safe);
        assert_equal false (Path.split "a/../c" |> Path.is_safe);
        assert_equal false (Path.split "../../c" |> Path.is_safe);
        assert_equal false (Path.split "c/~/c" |> Path.is_safe);
    );

    "Extracts file extension" >:: (fun cxt -> 
        assert_equal "jpg"              (Path.split "a/b/c.jpg" |> Path.get_extension);
        assert_exception Path.Too_short (Path.split "a/b/c") Path.get_extension;
        assert_exception Path.Too_short (Path.split "") Path.get_extension;
    );
] ;; 
