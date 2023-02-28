(*
 * by putting each symbol in a single line,
 * we know which one the line number depends on
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
