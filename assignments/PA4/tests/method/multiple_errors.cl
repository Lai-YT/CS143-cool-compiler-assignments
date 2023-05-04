class Base {
   base_attr : Int;

   method() : Int { 0 };
};

(*
 * (1) inherited attr is redefined
 * (2) different from original
 * (3) method is multiply defined
 * (4) the attribute type is undefined
 * (5) has self as formal
 * (6) the formal type is undefined
 * (7) formal is multiply defined
 * (8) method return type is undefined
 *
 * Errors should be reported in the above order.
 * The order of (1) (2) depends on their order of occurrence.
 * (1) (2) is checks over all classes,
 * then the "Class 'Main' not defined" error.
 * Other errors are reported class by class, method by method.
 *)

class Derived1 inherits Base {
   attr1: UnknownType;

   method(formal : Int, self : Derived1, formal : UnknownType) : UnknownType {
      true
   };

   base_attr : Int;

   method1() : UnknownType { true };
   method1() : UnknownType { true };
};

class Derived2 inherits Base {
   base_attr : Int;

   attr2: UnknownType;

   method(formal : Int, self : Derived2, formal : UnknownType) : UnknownType {
      true
   };

   method2() : UnknownType { true };
   method2() : UnknownType { true };
};
