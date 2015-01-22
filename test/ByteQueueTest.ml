open OUnit;;

let creation_test cxt = 
    let q = ByteQueue.create 10 in
    assert_equal 0 (ByteQueue.length q);
    assert_equal 10 (ByteQueue.capacity q);
    assert_equal 0 (ByteQueue.start_ptr q);
;;

let push_test cxt = 
    let q = ByteQueue.create 5 in
    ByteQueue.push "abc" 0 3 q ;

    assert_equal 5 (ByteQueue.capacity q);
    assert_equal 2 (ByteQueue.space q);
    assert_equal 3 (ByteQueue.length q);

    ByteQueue.push "de" 0 2 q ;

    assert_equal 5 (ByteQueue.capacity q);
    assert_equal 0 (ByteQueue.space q);
    assert_equal 5 (ByteQueue.length q);
;;

let push_pop_test cxt = 
    let q = ByteQueue.create 5 in
    ByteQueue.push "abcd" 0 4 q;

    assert_equal "ab" (ByteQueue.pop 2 q);
    assert_equal "cd" (ByteQueue.pop 2 q);
    assert_equal 0 (ByteQueue.length q);

    ByteQueue.push "abcd" 0 4 q;

    assert_equal "ab" (ByteQueue.pop 2 q);
    assert_equal "cd" (ByteQueue.pop 2 q);
    assert_equal 0 (ByteQueue.length q);
;;


let tests = "Byte Queue Tests" >::: [
    "Creation" >:: creation_test;
    "Push Test" >:: push_test; 
    "Push Pop Test" >:: push_pop_test;
];;
