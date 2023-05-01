class Main {
   main() : Int { 0 };

   (*
    * SELF_TYPE <= Main,
    * possible downcast is an error
    *)

   m : Main;
   s : SELF_TYPE <- m;

   method() : SELF_TYPE {
      m
   };
};
