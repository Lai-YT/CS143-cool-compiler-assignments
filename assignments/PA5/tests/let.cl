class Main inherits IO {
   main() : SELF_TYPE {{
      let i : Int in {
         i <- 10;
         out_int(i);
      };
      out_string("\n");

      let i : Int <- 10 in
         out_int(i);
      out_string("\n");

      let i : Int <- 2 in
         let s : String <- "\n" in {
            out_int(i);
            out_string(s);
         };

      let shadow : Int <- 1 in
         let shadow : Int <- 2 in
            out_int(shadow);
      out_string("\n");
   }};
};
