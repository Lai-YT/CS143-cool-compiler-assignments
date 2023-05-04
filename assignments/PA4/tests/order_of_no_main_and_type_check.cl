class Main {
   redefinedAttr : Int;
   redefinedAttr : Int;

   redefinedMethod() : Int { 0 };
   redefinedMethod() : Int { 0 };

   -- missing main

   t : Bool <- "should be boolean";

   attrWithUndefinedType : UndefinedType;

   methodWithDuplicateFormal(
      formal : Int, formal : Int
   ) : Int {
      formal <- ""
   };

   method(i : Int) : Int { {
      i <- "";
      self.methodWithDuplicateFormal(1 + "", 1);
   } };

   useRedefinedAttrIsOk() : Int {
      redefinedAttr + 1
   };

   useAttrWithUndefinedType() : Object { {
      attrWithUndefinedType + 1;
      attrWithUndefinedType;  -- unknown type is resolved as Object
   } };
};
