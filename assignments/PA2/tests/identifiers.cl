cLaSs Main {

   att_R1 : Int;
   _leading_underscore_is_an_error : Bool;
   2should_be_lexed_as_INT_CONST_2_and_OBJECTID : String;

   main() : Object {
      att_R1 <- 0
   };

   CapitalizedMethodNameShouldBeTypeId() : Int {
      0
   };

   self_type_should_be_object_id() : SELF_TYPE {
      self
   };

  tryKeywordFalse() : Bool {
      false
   };
};
