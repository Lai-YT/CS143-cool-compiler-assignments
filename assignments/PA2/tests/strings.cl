"this is a string"

"this is a string with a null \0 character"

"\0"
""

"\n"
"\t"
"\r"
"\b"
"\f"
"\1"

"this is a string with \"escape\" quotes"

"this is a string that spans \
multiple lines"

"should be on line 19"

"unterminated string
constant"

"string of length 1024 is ok                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            1024 safe"
"string of length 1025 is too long                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       1025 boom"

class Main {

  string : String <- "this is a string\n";

  main() : Object {
    string <- "this is also a \
    string";
  };

};
