open SkewBinaryRandomAccessList;;
open RandomAccessListTestBase;;

module SkewBinaryRandomAccessListTest =
  RandomAccessListTestBase.MakeTest(SkewBinaryRandomAccessList);;

let () =
  SkewBinaryRandomAccessListTest.run
;;
