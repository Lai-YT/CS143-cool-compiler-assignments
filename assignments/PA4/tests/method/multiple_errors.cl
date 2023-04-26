class Base {
   method() : Int { 0 };
};

(*
 * `method` is
 * (1) different from original
 * (2) has self as formal
 * (3) the formal type is undefined
 * (4) formal is multiply defined
 * (5) method return type is undefined
 * Errors should be reported in the above order.
 *)

class Main inherits Base {
   method(formal : Int, self : Main, formal : UnknownType) : UnknownType {
      true
   };

   main() : Int { 0 };
};
