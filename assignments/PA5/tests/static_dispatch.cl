class Main inherits IO {
   main() : IO {
      self@IO.out_string("IO.out_string")
   };

   out_string(s: String) : SELF_TYPE {
      out_string("Main.".concat(s))
   };
};
