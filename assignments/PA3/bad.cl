
(*
 *  execute "coolc bad.cl" to see the error messages that the coolc parser
 *  generates
 *
 *  execute "myparser bad.cl" to see the error messages that your parser
 *  generates
 *)

(* no error *)
class A {
};

(* error:  b is not a type identifier *)
Class b inherits A {
};

(* error:  a is not a type identifier *)
Class C inherits a {
};

(* error:  keyword inherits is misspelled *)
Class D inherts A {
};

(* error:  closing brace is missing *)
Class E inherits A {
;

(* error:  type is missing *)
class F {
   i <- 1;
   j : Int <- 2;
};

(* error:  invalid expression in block *)
class G {
   method() : Int { {
      "not an expression" ?;
      0;
   } };
};

(* error:  misusing LE as ASSIGN in let *)
class H {
   method() : Int {
      let s : String <= "misusing LE as ASSIGN" in 0
   };
};

(* error:  missing type in let binding + invalid expression *)
class I {
   method1() : Int {
      let i, j : Int <- 1 in j?
   };

   -- missing type in the second binding
   method2() : Int {
      let i : Int <- 1, j in i?
   };
};

(* error:  keyword inherits is misspelled
 * This error is a duplicate to check that the parser isn't terminated early.
 *)
Class J inherts A {
};
