-- two kinds of error messages should be shown

class Main inherits UndeclaredBase {
   main() : Int {
      0
   };
};

(*
 * cycles are checked only if no undeclared base,
 * so you won't see a circular inheritance error for the following
 *)
class A inherits B {};

class B inherits A {};
