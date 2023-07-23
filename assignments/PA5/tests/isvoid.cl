class A {};

class Main inherits IO {
   uninitialized : A;
   initialized : A <- new A;

   main() : SELF_TYPE {{
      out_string("uninitialized ");
      out_string(
         if isvoid uninitialized then "is void" else "is not void" fi
      );
      out_string("\n");

      out_string("initialized ");
      out_string(
         if isvoid initialized then "is void" else "is not void" fi
      );
      out_string("\n");
   }};
};
