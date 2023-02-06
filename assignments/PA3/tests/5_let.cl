class Main inherits IO {
   i : Int <- 1;

   main() : Object { {
      out_int(
         (*
          * (1) If the reduction is made first, the second i refers to 2 and the
          *    entire expr evaluates to 1;
          * (2) if the shift is made first, the second i refers to 1 and the entire
          *    evaluates to 0.
          * The (2) is correct.
          *)
         let i : Int <- 2 in i (* first i *) - i (* second i *)
      );
      out_string("\nshould be 0\n");
   } };
};
