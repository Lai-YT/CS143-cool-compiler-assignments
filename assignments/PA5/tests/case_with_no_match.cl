class Main inherits IO {
   main() : SELF_TYPE {
      case 1 of
         boolean : Bool => out_string("is Bool\n");
         string : String => out_string("is String\n");
      esac
   };
};
