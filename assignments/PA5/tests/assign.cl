class Main inherits IO {
   i : Int <- 1;

   main() : SELF_TYPE {{
      out_int(i);
      i <- 2;
      out_int(i);
   }};
};
