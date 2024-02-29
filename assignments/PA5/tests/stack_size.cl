(*
   Tests whether the size of the stack is correctly computed, not corrupted by the presence of a method call.
   Note that a let construct is used to create local variables.
*)

class Main inherits IO {
   main() : SELF_TYPE {
      let i : Int in {
         out_int(foo(1));
         out_string("\n");
         -- expected: 6
      }
   };

   foo(i : Int) : Int {
      let j : Int <- 2 in
         bar(i + j)
   };

   bar(i : Int) : Int {
      let j : Int <- 3 in
         i + j
   };
};
