class GrandBase {};

class Base1 inherits GrandBase {};
class Base2 inherits GrandBase {};

class Main {
   main() : Int { 0 };

   b1 : Base1;
   b2 : Base2;

   method() : Base1 {  -- should return the joint type, which is GrandBase
      if
         0  -- should be Bool
      then
         b1
      else
         b2
      fi
   };
};
