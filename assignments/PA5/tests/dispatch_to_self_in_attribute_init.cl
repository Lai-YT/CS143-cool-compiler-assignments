class Main inherits IO {
   i : Int <- one();

   one() : Int { 1 };

   main() : SELF_TYPE {
      out_string("doesn't crash!\n")
   };
};
