(*
 * By putting each symbol in a single line,
 * we know which one the line number depends on.
 *)

class LineNumberOfLet {
   noAssign() : Int {
      let
      i
      :
      Int
      in
      i
   };

   assign() : Int {
      let
      i
      :
      Int
      <-
      1
      in
      i
   };

   nestedNoAssign() : Int {
      let
      i
      :
      Int
      in
         let
         j
         :
         Int
            in
            0
   };

   nestedAssign() : Int {
      let
      i
      :
      Int
      <-
      1
      in
         let
         j
         :
         Int
         <-
         2
         in
            0
   };

   commaNoAssign() : Int {
      let
      i
      :
      Int
      ,
         j
         :
         Int
         in
            0
   };

   commaNoAssign() : Int {
      let
      i
      :
      Int
      <-
      1
      ,
         j
         :
         Int
         <-
         2
         in
            0
   };
};

class LineNumberOfArithmeticOperators {
   plus() : Int {
      1
      +
      2
   };

   minus() : Int {
      2
      -
      1
   };

   multiply() : Int {
      1
      *
      2
   };

   divide() : Int {
      2
      /
      1
   };

   i : Int;

   assign() : Int {
      i
      <-
      1
   };

   negate() : Int {
      ~
      1
   };
};

class LineNumberOfBooleanOperators {
   compliment() : Bool {
      not
      true
   };

   lessThan() : Bool {
      1
      <
      2
   };

   lessThanOrEqualTo() : Bool {
      1
      <=
      2
   };

   equalTo() : Bool {
      1
      =
      2
   };
};
