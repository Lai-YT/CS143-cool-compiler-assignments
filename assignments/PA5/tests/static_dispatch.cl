class Main inherits IO {
   uninitialized : IO;

   main() : IO {{
      self@IO.out_string("IO.out_string\n");
      uninitialized@IO.out_string("should abort");
   }};

   out_string(s: String) : SELF_TYPE {
      out_string("Main.".concat(s))
   };
};
