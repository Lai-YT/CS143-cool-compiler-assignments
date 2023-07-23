class Main inherits IO {
   predicate : Bool;

   main() : SELF_TYPE {{
      predicate <- true;
      out_string(
         if predicate then "true" else "false" fi
      );
      out_string("\n");

      predicate <- false;
      out_string(
         if predicate then "true" else "false" fi
      );
      out_string("\n");
   }};
};
