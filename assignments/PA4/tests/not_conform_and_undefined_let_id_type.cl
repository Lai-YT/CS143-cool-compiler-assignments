class Main {
   main() : Int {
      -- no "does not conform" error if the identifier type is undefined
      let i : UnknownType <- 0 in
         i
   };
};
