class Base {};

class A inherits Base {};
class B inherits Base {};
class C inherits Base {};

class Main {
   main() : Int { 0 };

   a : A;
   b : B;
   c : C;
   returnJointType()
      : Int  -- should be base
   {
      case
         i -- unknown identifier
      of
         x : Int => a;
         y : Bool => b;
         z : String => c;
      esac
   };

   branchOfSameType() : Int {
      case 0 of
         x : Int => x;
         y : Int => y;
      esac
   };

   errorInBranch() : Object {
      case 0 of
         (* test the order of error messages
          * duplicate branch is reported immediately when such branch is checked
          *)
         z : String => z <- "z is defined";
         y : Int => unknownIdentifier;
         x : String => x <- 0 (* assign error should still be caught*);
         w : UnknownType => w <- "Str assign to Object is ok";
         v : Object => 0;
      esac
   };
};
