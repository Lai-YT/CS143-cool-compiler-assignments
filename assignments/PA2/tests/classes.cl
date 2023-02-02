class A {

   method1() : Int { 0 };

   method2(param1 : Object) : Int { {
      1;
      0;
   } };

   method3(param1 : Object, param2: Int) : Object {
      (new IO).out_int(0)
   };
};

class B {};

class inherits {};

class Main {

   main() : Object {
      (let i : Int <- 1 in
         if i = 1 then i else 0 fi
   ) };
};
