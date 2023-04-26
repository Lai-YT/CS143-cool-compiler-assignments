class A {
   method(a : Int, b : Bool) : String {
      ""
   };
};

class UnmatchedReturnType inherits A {
   method(a : Int, b : Bool) : Int {
      0
   };
};

class MissingFormal inherits A {
   method(a : Int) : String {
      ""
   };
};

class UnmatchedFormalType inherits A {
   method(shouldBeInt : Bool, b : Bool) : String {
      ""
   };
};

class IndirectMiddleClass inherits A {};

(*
 * test indirection
 *)

class IndirectUnmatchedFormalType inherits IndirectMiddleClass {
   method(shouldBeInt : Bool, b : Bool) : String {
      ""
   };
};

(*
 * Test multiple method errors.
 *
 * There are 3 kinds of errors:
 *  (1) unmatched number of formals
 *  (2) unmatched formal type
 *  (3) unmatched return type
 * We'll test their combinations.
 *
 * As we can see when facing multiple errors, only one of them is reported.
 * The precedence is (3) (1) (2).
 *)


-- formal type + return type => return type
class IndirectUnmatchedFormalAndReturnType inherits IndirectMiddleClass {
   method(shouldBeInt : Bool, b : Bool) : Int {
      0
   };
};


-- number of formal + formal type => number of formal
class IndirectMissingAndUnmatchedFormalType inherits IndirectMiddleClass {
   method(shouldBeInt : Bool) : String {
      ""
   };
};


-- number of formal + return type => return type
class IndirectMissingFormalAndUnmatchedReturnType inherits IndirectMiddleClass {
   method(shouldBeInt : Int) : Int {
      0
   };
};


-- number of formal + formal type + return type => return type
class IndirectMissingFormalAndUnmatchedFormalAndReturnType inherits IndirectMiddleClass {
   method(shouldBeInt : Bool) : Int {
      0
   };
};


class Main inherits A {
   main() : Int { 0 };
};
