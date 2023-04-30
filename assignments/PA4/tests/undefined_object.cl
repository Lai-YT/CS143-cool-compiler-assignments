class Main {
   main() : Int { 0 };
};

Class A {
   undefinedObject : String
      <- "is defined in class A but class B should not be able to see this";

   method() : Int { {
      undefinedObject;
      0;
   } };
};

Class B {
   method() : Int { {
      undefinedObject;
      0;
   } };
};
