class Stack {

   list : List <- new List;

   push(c : StackCommand) : Stack { {
      list <- list.cons(c);
      self;
   } };

   pop() : StackCommand {
      (let top : StackCommand <- list.head() in {
         list <- list.tail();
         top;
      })
   };

   evaluate() : Int {
      if not list.isNil() then
         list.head().evaluate()
      else 0 fi
   };

   display() : Object {
      (let curr : List <- list in
         while not curr.isNil() loop {
            curr.head().display();
            (new IO).out_string("\n");
            curr <- curr.tail();
         } pool
      )
   };

};


class StackCommand {

   stack : Stack;

   evaluate() : Int { { abort(); 0; } };

   display() : Object { { abort(); 0; } };

   set_receiver(s : Stack) : StackCommand { {
      stack <- s;
      self;
   } };

};


class IntCommand inherits StackCommand {

   val : Int;

   evaluate() : Int { val };

   display() : Object {
      (new IO).out_int(val)
   };

   init(i : Int) : IntCommand { {
      val <- i;
      self;
   } };

};


class PlusCommand inherits StackCommand {

   evaluate() : Int { {
      stack.pop(); -- pop the plus command
      (let augend : StackCommand <- stack.pop() in
      (let addend : StackCommand <- stack.pop() in
      (let val : Int <- augend.evaluate() + addend.evaluate() in {
         stack.push((new IntCommand).init(val));
         0;
      })));
   } };

   display() : Object {
      (new IO).out_string("+")
   };

};


class SwapCommand inherits StackCommand {
   evaluate() : Int { {
      stack.pop(); -- pop the swap command
      (let top : StackCommand <- stack.pop() in
      (let next : StackCommand <- stack.pop() in {
         stack.push(top).push(next);
         0;
      }));
   } };

   display() : Object {
      (new IO).out_string("s")
   };
};


class Main inherits IO {

   command : String;
   stack : Stack <- new Stack;

   main() : Object { {
      command <- prompt();
      while not command = "x" loop {
         if command = "d" then
            stack.display()
         else if command = "e" then
            stack.evaluate()
         else if command = "+" then
            stack.push((new PlusCommand).set_receiver(stack))
         else if command = "s" then
            stack.push((new SwapCommand).set_receiver(stack))
         else  -- is integer
            stack.push(
               (new IntCommand)
                  .init((new A2I).a2i(command))
                  .set_receiver(stack)
            )
         fi fi fi fi;
         command <- prompt();
      } pool;
   } };

   prompt() : String { {
      out_string(">");
      in_string();
   } };

};


(*
 * The following implementation of the List (& Cons) data structure
 * are based on [Cool root]/examples/list.cl with the underlaying data
 * type changed from Int to StackCommand.
 *)

class List {

   isNil() : Bool { true };

   head() : StackCommand { { abort(); new StackCommand; } };

   tail() : List { { abort(); self; } };

   cons(c : StackCommand) : List {
      (new Cons).init(c, self)
   };

};


class Cons inherits List {

   car : StackCommand;

   cdr : List;

   isNil() : Bool { false };

   head() : StackCommand { car };

   tail() : List { cdr };

   init(c : StackCommand, rest : List) : List { {
      car <- c;
      cdr <- rest;
      self;
   } };

};
