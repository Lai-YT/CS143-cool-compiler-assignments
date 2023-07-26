class A {
   type() : String { "A" };
};

class B {
   type() : String { "B" };
};


class AA inherits A {
   type() : String { "AA" };
};

class Main inherits IO {
   a : A <- new A;
   b : B <- new B;
   aa : A <- new AA;

   main() : SELF_TYPE {{
      case a of
         i : Int => out_string("Int");
         -- aa : AA => out_string("AA");
         a : A => out_string(a.type());
         -- b : B => out_string("B");
         o : Object => out_string("Object");
      esac;
      out_string("\n");

      case b of
         i : Int => out_string("Int");
         aa : AA => out_string("AA");
         a : A => out_string("A");
         b : B => out_string("B");
         o : Object => out_string("Object");
      esac;
      out_string("\n");

      case aa of
         i : Int => out_string("Int");
         aa : AA => out_string("AA");
         a : A => out_string("A");
         b : B => out_string("B");
         o : Object => out_string("Object");
      esac;
      out_string("\n");
   }};
};
