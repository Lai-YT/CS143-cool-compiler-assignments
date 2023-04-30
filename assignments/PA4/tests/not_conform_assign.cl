class Main {
   main() : Int { 0 };

   i : Int;

   -- the type of the assignment is the type of the expression on the right
   -- hand side
   method() : Int {
      i <- "should be an integer"
   };
};
