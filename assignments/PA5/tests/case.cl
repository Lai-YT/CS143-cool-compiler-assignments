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
         aa : AA => out_string(aa.type());
         a : A => out_string(a.type());
         b : B => out_string(b.type());
         o : Object => out_string("Object");
      esac;
      out_string("\n");

      case b of
         i : Int => out_string("Int");
         aa : AA => out_string(aa.type());
         a : A => out_string(a.type());
         b : B => out_string(b.type());
         o : Object => out_string("Object");
      esac;
      out_string("\n");

      -- will be matched by aa instead of a
      case aa of
         i : Int => out_string("Int");
         a : A => {
            out_string("matched type: A, real type: ");
            out_string(a.type());
         };
         b : B => out_string(b.type());
         aa : AA => {
            out_string("matched type: AA, real type: ");
            out_string(aa.type());
         };
         o : Object => out_string("Object");
      esac;
      out_string("\n");

      -- -- will be matched by o
      case true of
         i : Int => out_string("Int");
         aa : AA => out_string(aa.type());
         a : A => out_string(a.type());
         b : B => out_string(b.type());
         o : Object => out_string("Object");
      esac;
      out_string("\n");
   }};
};
