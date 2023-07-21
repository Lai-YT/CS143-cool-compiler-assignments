class Main inherits IO {
   io: IO <- new IO;

   main(): SELF_TYPE {{
      io.out_string("dispatch to other\n");
      dispatch_to_self();
   }};

   dispatch_to_self() : SELF_TYPE {
      out_string("dispatch to SELF_TYPE\n")
   };
};
