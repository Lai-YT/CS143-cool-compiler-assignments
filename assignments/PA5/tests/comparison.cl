class Main inherits IO {
   main() : SELF_TYPE {{
      out_string(
         if 1 < 2 then "true" else "false" fi
      );
      out_string("\n");

      out_string(
         if 1 <= 2 then "true" else "false" fi
      );
      out_string("\n");

      out_string(
         if 1 = 2 then "true" else "false" fi
      );
      out_string("\n");
   }};
};
