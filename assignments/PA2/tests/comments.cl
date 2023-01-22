-- this is a one line comment

(* this is an one line spannable comment *)

(* this is an one line (* nested *) comment *)

(* this is an one line spannable comment with -- one line comment *)

(*
 * this is a multiple line comment
 *)

(*
 * this is a multiple line (*
 *  nested
 * *)
 * comment
 *)

-- this is a one line comment with (* spannable *) comment

(*
 * this is a multiple line comment with
 -- one line comment
 *)

(*
  this is a broken comment, should have EOF error
)
