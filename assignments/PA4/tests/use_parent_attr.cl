class Main {
   main() : Int { 0 };
};

class Base {
   parent_attr : Int;
};

class Derived1 inherits Base {
   method() : Int {
      -- using attribute of parent is valid
      parent_attr
   };
};

class Derived2 inherits Base {
   -- shadowing is an error
   parent_attr : String;

   method() : String {
      -- does not see the redefined attribute, the type is kept as Int
      parent_attr
   };
};
