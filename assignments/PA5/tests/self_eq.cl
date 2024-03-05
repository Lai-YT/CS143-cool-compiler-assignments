class Main inherits IO {
   main() : SELF_TYPE {
      if self = self then
         out_string("self is self")
      else
         out_string("self is not self")
      fi
   };
};
