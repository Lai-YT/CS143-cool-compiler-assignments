class Base {
   method() : Int { 0 };
};

(*
 * (1) different from original
 * (2) method is multiply defined
 * (3) the attribute type is undefined
 * (4) has self as formal
 * (5) the formal type is undefined
 * (6) formal is multiply defined
 * (7) method return type is undefined
 *
 * Errors should be reported in the above order.
 * (2) should come right after (1) over all classes,
 * then the "Class 'Main' not defined" error.
 * Other errors are reported class by class, method by method.
 *)

class Derived1 inherits Base {
   attr1: UnknownType;

   method(formal : Int, self : Derived1, formal : UnknownType) : UnknownType {
      true
   };

   method1() : UnknownType { true };
   method1() : UnknownType { true };
};

class Derived2 inherits Base {
   attr2: UnknownType;

   method(formal : Int, self : Derived2, formal : UnknownType) : UnknownType {
      true
   };

   method2() : UnknownType { true };
   method2() : UnknownType { true };
};
