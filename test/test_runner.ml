open OUnit;;

let _ = run_test_tt_main Dummy.tests
let _ = run_test_tt_main ByteQueueTest.tests
let _ = run_test_tt_main HttpTest.tests;
