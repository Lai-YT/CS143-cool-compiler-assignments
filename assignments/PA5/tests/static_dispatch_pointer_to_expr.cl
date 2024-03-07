class Foo inherits Bar {
   a : Bar <- new Bar;

   -- The pointer to the a object should be passed to the increment method correctly.
   j : Int <- a@Bar.increment();
};

class Bar inherits IO {
   i : Int <- 1;

   increment() : Int {{
      out_int(i); out_string("\n");
      i <- i + 1;
      out_int(i); out_string("\n");
      i;
   }};
};

class Main {
   f : Foo <- new Foo;

   main() : String { "do nothing" };
};
