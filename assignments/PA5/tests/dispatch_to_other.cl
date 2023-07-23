class Main {
   io : IO <- new IO;
   uninitialized : IO;

   main() : IO {{
      io.out_string("dispatch to other\n");
      uninitialized.out_string("should abort");
   }};
};
