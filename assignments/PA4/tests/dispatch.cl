class A {
   methodA(a : Int, b : String) : Bool {
      false
   };
};

class B {
   methodB(a : Int, b : String) : Bool {
      false
   };
};

class Main {
   main() : Int { 0 };

   method(a : A, b : B)
      : Bool  -- should be Object
   { {
      a.methodA(
         0,
         0  -- should be string
      );
      (*
       * when the number of argument is wrong, conformance check is skipped
       *)
      a.
      methodA(
         0 + "",  -- error not caught due to wrong number of argument
         1,
         ""  -- extra argument
      );
      b.methodA(  -- no such method (it belong to A, this tests the method table)
         0, "");
      (unknownIdentifier).unknownMethod(
         1 + ""  -- error in undefined method is still caught
      );
   } };
};
