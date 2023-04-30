class A {};

class B {};

class Main {
   a : A;
   b : B;

   main() : Bool { {
      (*
       * not same type (for Int, String, and Bool)
       *)
      1 = "1";
      true = 1;

      (*
       * other types can be compared freely
       *)
      a = b;

      false = "0";  -- end with an error to test recovery
   } };
};
