class A {
   noFormal() : Int { 1 };

   singleFormal(formal : Int) : Int { formal };

   multipleFormals(
      formal_1 : Int, formal_2 : String, formal_3 : Bool
   ) : Bool { formal_3 };
};


class B inherits A {

   i : Int;

   intConst() : Int {
      1
   };

   stringConst() : String {
      "string"
   };

   boolConst() : Bool {
      true
   };

   assign() : Int {
      i <- 1
   };

   staticDispatch() : Int {
      self@A.noFormal()
   };

   dispatchNoFormal() : Int {
      self.noFormal()
   };

   dispatchSingleFormal() : Int {
      self.singleFormal(1)
   };

   dispatchMultipleFormals() : Bool {
      self.multipleFormals(
         1,
         "1",
         true
      )
   };

   shorthandDispatch() : Int {
      noFormal()
   };

   condition() : Int {
      if true then 1 else 0 fi
   };

   loop_() : Object {
      while false loop 0 pool
   };

   new_() : Int {
      i <- new Int
   };

   isvoid_() : Bool {
      isvoid i
   };

   plus() : Int {
      1 + 2
   };

   sub() : Int {
      1 - 2
   };

   multiply() : Int {
      1 * 2
   };

   divide() : Int {
      2 / 1
   };

   negate() : Int {
      ~1
   };

   lessThan() : Bool {
      1 < 2
   };

   lessThanOrEqualTo() : Bool {
      1 <= 2
   };

   equalTo() : Bool {
      1 = 2
   };

   syntaxErrorCompOpTwiceInARow() : Bool {
      true = 1 < 2
   };

   compliment() : Bool {
      not true
   };

   doubleComplimentIsOk() : Bool {
      not not true
   };

   parenthesized() : Int {
      ( 1 )
   };

   id() : Int {
      i
   };

   block() : Int { {
      "single expr";
      {
         "multiple";
         "expr";
      };
      "all of which are semi-colon separated";
      0;
   } };

   singleLet() : Int {
      let x : Int in 0
   };

   singleLetWithInit() : Int {
      let x : Int <- 1 in x
   };

   commaSeparatedLets() : Int {
      let x : Int, y : String in 0
   };

   nestedLets() : Int {
      let x : Int in
         let y : String in 0
   };

   singleBranchCase() : Int {
      case i of
         a : Int => 0;
      esac
   };

   multipleBranchesCase() : Bool {
      case i of
         a : Int => true;
         o : Object => false;
      esac
   };

};
