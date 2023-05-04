class Base {
   parent_attr : Int;
};

-- two level redefinition + undefined type

class Derived inherits Base {
   parent_attr : Int;
};

class Main {
   parent_attr : UndefinedType;

   main() : Int { 0 };
};
