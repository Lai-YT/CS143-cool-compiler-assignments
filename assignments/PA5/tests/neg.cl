class Main inherits IO {
   x : Int <- 1;
   main() : SELF_TYPE {{
      out_int(~2147483647);
      out_string("\n");
      out_int(~0);
      out_string("\n");
      out_int(~x);
      out_string("\n");
      -- The attribute should remain unchanged after the negation.
      out_int(x);
      out_string("\n");
   }};
};
