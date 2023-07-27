class A {
   type() : String { "A" };
};

class AA inherits A {
   type() : String { "AA" };
};

class Main {
   io : IO <- new IO;
   uninitialized : IO;
   aa : A <- new AA;

   main() : IO {{
      io.out_string("dispatch to other\n");
      io.out_string(aa.type());
      io.out_string("\n");
      uninitialized.out_string("dispatch on void, should abort\n");
   }};
};
