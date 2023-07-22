class Main inherits IO {

   main() : SELF_TYPE {{
      out_int(plus());
      out_int(sub());
      out_int(mul());
      out_int(divide());
   }};

   plus() : Int {
      3 + 1
   };

   sub() : Int {
      3 - 1
   };

   mul() : Int {
      3 * 1
   };

   divide() : Int {
      3 / 2
   };
};
