open OUnit;;
open Http;;

let parses_request_line_correctly cxt = 
    assert_equal 
        (Some (Request.Custom "OPTIONS", "*")) 
        (Request.parse_request_line "OPTIONS * HTTP/1.1");

    assert_equal 
        (Some (Request.Get, "/")) 
        (Request.parse_request_line "GET / HTTP/1.0");
;;


let parses_request_header_line_correctly cxt =
    assert_equal (Some ("a", "b")) (Request.parse_request_header_line "a:  b");
    assert_equal (Some ("x-b", "&&")) (Request.parse_request_header_line "x-b  :&&");
    assert_equal None (Request.parse_request_header_line "x*b  :&&")
;;


let parses_request_header_correctly cxt = 
    let header = [
        "abc : 1";
        "x-Y-z  : 2";
        "j : 3";
    ]
    in 
    match Request.parse_request_header header with
        | None -> assert_equal 1 0;
        | Some h ->
                assert_equal "1" (Hashtbl.find h "abc");
                assert_equal "2" (Hashtbl.find h "x-y-z");
                assert_equal "3" (Hashtbl.find h "j")
;;


let tests = "Http Tests" >::: [
    "Parses request line correctly" >:: parses_request_line_correctly;
    "Parses request header line correctly" >:: parses_request_header_line_correctly;
    "Parses request header correctly" >:: parses_request_header_correctly;
] ;;
