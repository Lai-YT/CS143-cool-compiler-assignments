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
 * The multiply defined error should come right after (1).
 *)

class Derived1 inherits Base {
   method(formal : Int, self : Derived1, formal : UnknownType) : UnknownType {
      true
   };

   method1() : UnknownType { true };
   method1() : UnknownType { true };
};

class Derived2 inherits Base {
   method(formal : Int, self : Derived2, formal : UnknownType) : UnknownType {
      true
   };

   method2() : UnknownType { true };
   method2() : UnknownType { true };
};
