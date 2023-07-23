class Main inherits IO {
   main() : SELF_TYPE {{
      out_string(
         if not true then "true" else "false" fi
      );
      out_string("\n");

      out_string(
         if not false then "true" else "false" fi
      );
      out_string("\n");
   }};
};
