open OUnit;;

let dummy cxt = assert_equal 1 1 ;;

let tests = "Dummy" >::: [
    "dummy" >:: dummy 
];;
