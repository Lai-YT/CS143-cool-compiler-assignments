class Base {
   method() : SELF_TYPE {
      self
   };
};

class Derived inherits Base {
   method() : SELF_TYPE {
      self
   };
};

class Main {
   b : Base;
   d : Derived;
   main() : Int { {
      b.method();
      d.method();
      d@Base.method();
      0;
   } };
};
