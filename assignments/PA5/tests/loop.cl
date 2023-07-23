class Main inherits IO {
   counter : Int <- 0;

   main() : Int {{
      while counter < 10 loop {
         out_int(counter);
         out_string("\n");
         counter <- counter + 1;
      }
      pool;
      counter;
   }};
};
