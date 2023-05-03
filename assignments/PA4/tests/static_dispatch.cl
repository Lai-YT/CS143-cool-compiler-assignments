class Base {
   method(a : Int, b : String) : String {
      "Base"
   };
};

class Derived inherits Base {
   method(a : Int, b : String) : String {
      "Derived"
   };
};

class Main {
   main() : Int { 0 };

   method(b : Base, d : Derived)
      : Bool  -- should be Object
   { {
      d@Derived.method(
         0,
         0  -- should be string
      );
      (*
       * when the number of argument is wrong, conformance check is skipped
       *)
      b@Base.
      method(
         0 + "",  -- error not caught due to wrong number of argument
         1,
         ""  -- extra argument
      );
      b@Derived  -- Base does not conform to Derived
      .method(
         1 + ""  -- error is still caught
      );
      (unknownIdentifier)
      @A  -- undefined class
      .unknownMethod(
         1 + ""  -- error in undefined method is still caught
      );
   } };
};
