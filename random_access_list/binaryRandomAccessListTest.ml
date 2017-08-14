open BinaryRandomAccessList;;
open RandomAccessListTestBase;;

module BinaryRandomAccessListTest =
  RandomAccessListTestBase.MakeTest(BinaryRandomAccessList);;

let () =
  BinaryRandomAccessListTest.run
;;
