open OUnit2;;

let target x = 2 * x 

let suite = "suite" >::: [
    "test1" >:: (fun _ -> assert_equal 8 (target 4));
    "test2" >:: (fun _ -> assert_equal 2 (target 1))
    ] ;;

run_test_tt_main suite
