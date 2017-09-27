open SequenceBinaryRandomAccessList;;
open RandomAccessListTestBase;;

module SequenceBinaryRandomAccessListTest =
  RandomAccessListTestBase.MakeTest(SequenceBinaryRandomAccessList);;

let () =
  SequenceBinaryRandomAccessListTest.run
;;
