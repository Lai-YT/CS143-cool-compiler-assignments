class Main {
   main() : Int { 1 };

   method1() : UnknownType {
      0
   };

   method2() : UnknownType {
      0
   };

   -- SELF_TYPE is valid
   method3() : SELF_TYPE {
      self
   };
};
