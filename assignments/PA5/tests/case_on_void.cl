class A {};

class Main inherits IO {
   uninitialized_a : A;

   main() : SELF_TYPE {
      case uninitialized_a of
         a : A => out_string("is A\n");
      esac
   };
};
