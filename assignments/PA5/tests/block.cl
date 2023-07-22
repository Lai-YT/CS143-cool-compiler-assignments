class Main inherits IO {
   (*
      A simple block expression test that prints two integers in a row.
   *)
   main() : SELF_TYPE {{
      out_int(0);
      out_int(0);
   }};
};
