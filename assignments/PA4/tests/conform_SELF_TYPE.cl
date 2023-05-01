class Main {
   main() : Int { 0 };

   s : SELF_TYPE;
   m : Main;

   (*
    * SELF_TYPE <- SELF_TYPE
    *)
   s1 : SELF_TYPE <- s;

   currentClassConformsToSelfType() : SELF_TYPE {
      s
   };

   (*
    * current class <- SELF_TYPE
    *)
   m1 : Main <- s;

   selfTypeConformsToCurrentClass() : Main {
      m
   };
};
