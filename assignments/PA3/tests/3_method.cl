-- Use simple single expressions since we're not testing expression here.

class Main {

   noFormal() : String { "0" };

   singleFormal(formal : Int) : Bool { true };

   multipleFormals(
      formal_1 : Int, formal_2 : String, formal_3 : Bool
   ) : Bool { true };

};
