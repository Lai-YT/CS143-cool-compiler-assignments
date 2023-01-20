class Stack {

   node : StackNode;

   -- Sets "this" stack as the receiver of the command and pushes onto the stack.
   push(c : StackCommand) : Stack { {
      c.set_receiver(self);
      node <- (new StackNode).init(c, node);
      self;
   } };

   -- Removes the top-most node from the stack and returns the command of it.
   pop() : StackCommand {
      (let top : StackNode <- node in {
         node <- node.next();
         top.val();
      })
   };

   (**
    * Evaluates the command of the top-most node.
    *
    * It's safe to evaluate an empty stack.
    *)
   evaluate() : Int {
      if not isvoid node then node.val().evaluate() else 0 fi
   };

   -- Iterates through all of the nodes and displays them one by one.
   display() : Object {
      (let curr : StackNode <- node in
         while not isvoid curr loop {
            curr.val().display();
            (new IO).out_string("\n");
            curr <- curr.next();
         } pool
      )
   };

};


class StackCommand {

   stack : Stack;

   -- using abort() to simulate an abstract class: should override

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
            stack.push(new PlusCommand)
         else if command = "s" then
            stack.push(new SwapCommand)
         else  -- is integer
            stack.push(
               (new IntCommand)
                  .init((new A2I).a2i(command))
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


(**
 * Though it's named "StackNode", multiple StackNodes
 * together can effectively construct a singly linked list.
 *)
class StackNode {

   val : StackCommand;

   next : StackNode;

   val() : StackCommand { val };

   next() : StackNode { next };

   init(v : StackCommand, n : StackNode) : StackNode { {
      val <- v;
      next <- n;
      self;
   } };

};
