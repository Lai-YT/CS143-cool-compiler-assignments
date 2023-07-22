
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include <functional>
#include <vector>

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os)
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Pop the stack to a register. The stack shrinks towards larger addresses.
//
static void emit_pop(char *reg, ostream& str)
{
  // SP points to one above top, so adjust SP first.
  emit_addiu(SP, SP, WORD_SIZE, str);
  emit_load(reg, 0, SP, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_pop(ACC, s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

//
// Emits human-readable comments if the code gen debugging flag is set.
//
static void emit_comment(char *comment, ostream &s) {
  if (cgen_debug) {
    s << "# " << comment << endl;
  }
}

//
// Caller saved: ACC (temporaries, if any)
// Callee saved: RA, FP, SELF, SP
//

static const int NUM_OF_CALLEE_SAVED = 3;

static void emit_callee_saves(ostream &s) {
  emit_comment("Callee saves:", s);
  emit_push(RA, s);
  emit_push(FP, s);
  emit_push(SELF, s);
  emit_comment("End callee save", s);
}

static void emit_callee_restores(ostream &s) {
  emit_comment("Callee restores:", s);
  emit_pop(SELF, s);
  // NOTE: In the class example, FP is caller-saved but restored by the callee,
  // which looks awkward to me. I change it to fully callee-saved.
  emit_pop(FP, s);
  emit_pop(RA, s);
  emit_comment("End callee restore", s);
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/

      emit_disptable_ref(Str, s);  s << endl;                 // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD;

 /***** Add dispatch information for class Int ******/

      emit_disptable_ref(Int, s);  s << endl;             // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/

      emit_disptable_ref(Bool, s);  s << endl;            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}


//////////////////////////////////////////////////////////////////////////////
//
//  Overloads of list methods
//
//  We find them useful with modern C++ features.
//
//////////////////////////////////////////////////////////////////////////////

template <class T>
void list_map(std::function<void(T*)>f, List<T> *l)
{
  for (l; l != NULL; l = l->tl())
    f(l->hd());
}

//////////////////////////////////////////////////////////////////////////////
//
//  Utilities
//
//////////////////////////////////////////////////////////////////////////////

//
// Returns nullptr if f can't be safely converted into a method.
//
static method_class *as_method(const Feature f) {
  return dynamic_cast<method_class *>(f);
}

//
// Returns nullptr if f can't be safely converted into an attribute.
//
static attr_class *as_attribute(const Feature f) {
  return dynamic_cast<attr_class *>(f);
}

//
// Returns a vector which contains the nodes in nds, sorted in ascending order
// with class_tag.
//
static std::vector<CgenNode *> sort_list_with_class_tag(List<CgenNode> *nds) {
  auto nodes = std::vector<CgenNode *>{};
  list_map<CgenNode>([&nodes](CgenNode *nd) { nodes.push_back(nd); }, nds);
  std::sort(nodes.begin(), nodes.end(), [](CgenNode *a, CgenNode *b) {
      return a->get_class_tag() < b->get_class_tag();
  });
  return nodes;
}

//
// Object will be the first parent. nd itself is excluded.
//
static List<CgenNode>* get_parents(const CgenNode *nd) {
  List<CgenNode>* parents = NULL;
  while (nd->name != Object) {
    parents = new List(nd->get_parentnd(), parents);
    nd = nd->get_parentnd();
  }
  return parents;
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL
      << WORD << stringclasstag << endl;
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::code_prototype_objects() {
  // template argument cannot be easily deduced with lambda
  list_map<CgenNode>([this](CgenNode *nd) { nd->code_prototype_object(str); },
                     nds);
}

//
// At index (class_tag) ∗ 4 contains a pointer to a String object containing
// the name of the class associated.
//
void CgenClassTable::code_class_name_table() {
  // NOTE: the order of nodes in `nds` doesn't necessarily follows their
  // classtags, which means we need to sort them first.
  str << CLASSNAMETAB << LABEL;
  for (auto *nd : sort_list_with_class_tag(nds)) {
    str << WORD;
    stringtable.lookup_string(nd->name->get_string())->code_ref(str);
    str << endl;
  }
}

//
// At index (class_tag) * 8 contains a pointer to the prototype object and at
// index (class_tag) * 8 + 4 contains a pointer to the initialization method for
// that class.
//
void CgenClassTable::code_class_object_table() {
  // emit code
  // NOTE: The order of nodes in `nds` doesn't necessarily follows their
  // classtags, which means we need to sort them first.
  str << CLASSOBJTAB << LABEL;
  for (auto *nd : sort_list_with_class_tag(nds)) {
    str << WORD << nd->name << PROTOBJ_SUFFIX << endl;
    str << WORD << nd->name << CLASSINIT_SUFFIX << endl;
  }
}

//
// A dispatch table of class C labels all the methods it has in the form
// "C.method".
//
void CgenClassTable::code_dispatch_tables() {
  list_map<CgenNode>(
      [this](CgenNode *nd) {
          str << nd->name << DISPTAB_SUFFIX << LABEL;
          nd->code_dispatch_table(str);
      },
      nds);
}

void CgenClassTable::code_class_inits() {
  list_map<CgenNode>(
      [this](CgenNode *nd) {
          str << nd->name << CLASSINIT_SUFFIX << LABEL;
          nd->code_class_init(str, this);
      },
      nds);
}

void CgenClassTable::code_class_methods() {
  list_map<CgenNode>(
      [this](CgenNode *nd) {
          if (!nd->basic()) {
              nd->code_class_method(str, this);
          }
      },
      nds);
}

CgenClassTable::CgenClassTable(Classes classes, ostream &s)
    : nds(NULL), str(s), local_table(new SymbolTable<Symbol, int>) {
  //  NOTE: the reference compiler uses these three tags for basic class,
  //  which is the reason why I choose them
   stringclasstag = 5 /* Change to your String class tag here */;
   intclasstag =    3 /* Change to your Int class tag here */;
   boolclasstag =   4 /* Change to your Bool class tag here */;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();
   build_dispatch_layouts();
   build_attribute_layouts();

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

//
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object,
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

//
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO,
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer.
//
   install_class(
    new CgenNode(
     class_(Int,
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//
   install_class(
    new CgenNode(
      class_(Str,
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat,
				   single_Formals(formal(arg, Str)),
				   Str,
				   no_expr()))),
	    single_Features(method(substr,
				   append_Formals(single_Formals(formal(arg, Int)),
						  single_Formals(formal(arg2, Int))),
				   Str,
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

//
// Handles the method dispatch layout with overriden.
//
// Must be called after the relations are set up.
//
void CgenClassTable::build_dispatch_layouts() {
  // Since we didn't keep the root node, we need a linear search.
  // The root node will be Object.
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    if (l->hd()->name == Object) {
        l->hd()->build_dispatch_layout();
        break;
    }
  }
}

//
// Must be called after the relations are set up.
//
void CgenClassTable::build_attribute_layouts() {
  // Since we didn't keep the root node, we need a linear search.
  // The root node will be Object.
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    if (l->hd()->name == Object) {
        l->hd()->build_attribute_layout();
        break;
    }
  }
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

//
// Builds the method dispatch layout of the node and its children.
//
void CgenNode::build_dispatch_layout() {
  // The derived class first copies the dispatch layout from it parent,
  // override some methods by looking it up from the offsets and change the
  // latest implementor.
  dispatch_layout = get_parentnd()->dispatch_layout;
  dispatch_offsets = get_parentnd()->dispatch_offsets;
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    auto *method = as_method(features->nth(i));
    if (!method) {
      continue;
    }
    // This is an override, change the implementor
    if (dispatch_offsets.count(method->name)) {
      const int offset = dispatch_offsets.at(method->name);
      dispatch_layout.at(offset).first = this->name;
    } else {
      // New method, add into the layout and record the offset
      dispatch_layout.emplace_back(this->name, method);
      dispatch_offsets.emplace(method->name, dispatch_layout.size() - 1);
    }
  }

  // Build recursively on the tree struture in a breath-first way.
  list_map(std::function<void(CgenNode *)>{[](CgenNode *child) {
               child->build_dispatch_layout();
           }},  // an explicit function construct to resolve ambiguous overload
                // selection
           children);
}

//
// Builds the attribute layout of the node and its children.
//
void CgenNode::build_attribute_layout() {
  // The derived class simply copies and extends the layout of its parent since
  // attributes are not allowed to be overriden.
  attribute_layout = get_parentnd()->attribute_layout;
  attribute_offsets = get_parentnd()->attribute_offsets;

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    auto *attribute = as_attribute(features->nth(i));
    if (!attribute) {
      continue;
    }
    attribute_layout.emplace_back(this->name, attribute);
    attribute_offsets.emplace(attribute->name, attribute_layout.size() - 1);
  }

  // Build recursively on the tree struture in a breath-first way.
  list_map(std::function<void(CgenNode *)>{[](CgenNode *child) {
               child->build_attribute_layout();
           }},  // an explicit function construct to resolve ambiguous overload
                // selection
           children);
}

int CgenNode::get_attribute_offset(Symbol attribute) const {
  return attribute_offsets.at(attribute);
}

int CgenNode::get_method_offset(Symbol method) const {
  return dispatch_offsets.at(method);
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
  if (cgen_debug) cout << "coding prototype objects" << endl;
  code_prototype_objects();

  if (cgen_debug) cout << "coding class name table" << endl;
  code_class_name_table();

  if (cgen_debug) cout << "coding class object table" << endl;
  code_class_object_table();

  if (cgen_debug) cout << "coding dispatch tables" << endl;
  code_dispatch_tables();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

  if (cgen_debug) cout << "coding object initializers" << endl;
  code_class_inits();

  if (cgen_debug) cout << "coding class methods" << endl;
  code_class_methods();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{
   stringtable.add_string(name->get_string());          // Add class name to string table
   set_class_tag();
}

// 0 ~ 5 are reserved for basic classes:
// 0: Object
// 1: IO
// 2: Main
// 3: Int
// 4: Bool
// 5: String
ClassTag CgenNode::next_class_tag = 6;

void CgenNode::set_class_tag() {
   if (!basic()) {
    if (name == Main) {
      class_tag = 2;
    } else {
      class_tag = next_class_tag++;
    }
    return;
   }
   if (name == Object) {
    class_tag = 0;
   } else if (name == IO) {
    class_tag = 1;
   } else if (name == Int) {
    class_tag = 3;
   } else if (name == Bool) {
    class_tag = 4;
   } else if (name == Str) {
    class_tag = 5;
   } else {
    // Non-generating classes fall into here, e.g., No_class.
    // It's okay that we ignore them.
    if (cgen_debug) {
      cout << "\tignore non-generating class: " << name << endl;
    }
   }
}

void CgenNode::code_prototype_object(ostream &s) const {
   if (cgen_debug) {
    cout << '\t' << name << endl;
   }
   s << WORD << -1 << endl;  // For GC
   s << name << PROTOBJ_SUFFIX << LABEL;
   s << WORD << class_tag << endl;
   int number_of_attributes = 0;
   s << WORD << (DEFAULT_OBJFIELDS + attribute_layout.size()) << endl;
   s << WORD;  emit_disptable_ref(name, s); s << endl;
   code_attributes(s);
}

void CgenNode::code_attributes(ostream &s) const {
  for (auto [_, attribute] : attribute_layout) {
    s << WORD;
    Symbol type = attribute->type_decl;
    if (type == Int) {
      inttable.lookup_string("0")->code_ref(s);
    } else if (type == Bool) {
      falsebool.code_ref(s);
    } else if (type == Str) {
      stringtable.lookup_string("")->code_ref(s);
    } else {
      s << '0' << "\t# void";
    }
    s << endl;
  }
}

void CgenNode::code_dispatch_table(ostream &s) const {
  if (cgen_debug) {
    cout << '\t' << name << endl;
  }
  for (auto [implementor, method] : dispatch_layout) {
    s << WORD;
    emit_method_ref(implementor, method->name, s);
    s << endl;
  }
}

void CgenNode::code_class_method(ostream &s, CgenClassTableP env) const {
  for (auto [implementor, method] : dispatch_layout) {
    if (implementor == name) {
      emit_method_ref(name, method->name, s);  s << LABEL;
      env->local_table->enterscope();
      env->self_object = name;
      method->code(s, env);
      env->local_table->exitscope();
    }
  }
}

//
// Emit initialization code of the parents first, then self.
//
void CgenNode::code_class_init(ostream &s, CgenClassTableP env) const {
  // From the perspective of called by child.
  emit_callee_saves(s);

  // So that we know FP + 4 is our first argument, FP + 8 is the second one, and
  // NOTE: The frame pointer points to the top, not bottom of the frame.
  // so on (although there's no argument in this case).
  emit_comment("Set new frame pointer", s);
  emit_addiu(FP, SP, WORD_SIZE * NUM_OF_CALLEE_SAVED, s);

  //
  // On entry:
  // |   ...    | <- FP
  // |   ...    |
  //      .
  //      .
  //      .
  // | OLD_SELF |
  // |  OLD_FP  |
  //   (no arg)
  // |   ...    | <- SP
  // ACC: SELF
  //
  // SELF is a special argument, which is always passed through ACC.
  // Other arguments are passed with the stack, but in the init method, there's
  // no any other arguments.
  //

  // XXX: not yet get the rule of passing SELF

  // Get our SELF pointer from ACC:
  emit_move(SELF, ACC, s);

  // It might look a bit redundant to move SELF around, but that's because we
  // got nothing else to do before calling the init method of parent.

  // We should push all arguments (exclude SELF) on to the stack, but there's no
  // arguments in this case.

  emit_comment("Pass SELF through ACC", s);
  emit_move(ACC, SELF, s);

  // Now it's time to call the init method of the parent.
  if (parent != No_class) {
    s << JAL;  emit_init_ref(parent, s);  s << endl;
  }

  // Restore caller saved:
  // No temporaries.

  // Emit code for our initialization:
  for (auto [implementor, attribute] : attribute_layout) {
    if (implementor != name) {
      // Those not implemented by the current class are initialized by the
      // parents,
      continue;
    }
    // We don't have to init attribute that doesn't have an init expression
    // since it's already set with default value in the prototype.
    if (!attribute->init->is_no_expr()) {
      attribute->init->code(s, env);
      emit_store(ACC, DEFAULT_OBJFIELDS + get_attribute_offset(attribute->name),
                 SELF, s);
    }
  }

  emit_callee_restores(s);

  // We should pop all arguments to restore SP, but there's no arguments in this
  // case.

  // The initialized object is returned in ACC.
  emit_move(ACC, SELF, s);

  emit_return(s);
}

void method_class::code(ostream &s, CgenClassTableP env) const {
  //
  // Caller saved: ACC (temporaries, if any)
  // Callee saved: RA, FP, SELF, SP
  //

  emit_callee_saves(s);

  // FP + 4 is our first argument, FP + 8 is the second one, and so on.
  emit_comment("Set new frame pointer", s);
  emit_addiu(FP, SP, 3 * WORD_SIZE, s);

  // SELF is passed through ACC.
  emit_move(SELF, ACC, s);

  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    // Offset 0 is where the return address is, can't be a local.
    env->local_table->addid(formals->nth(i)->get_name(), new int(i + 1));
  }

  emit_comment("Execute the expressions inside the method", s);
  expr->code(s, env);
  emit_comment("End method expression", s);

  // Note that the return value is in ACC.

  emit_callee_restores(s);

  emit_comment("Pops all arguments to restore SP", s);
  for (int i = 0; i < formals->len(); i++) {
    emit_pop(T1, s);
  }
  emit_comment("End argument pop", s);

  // return
  emit_return(s);
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s, CgenClassTableP env) {
  emit_comment("Start assign", s);

  emit_comment("Evaluate the right hand side", s);
  expr->code(s, env);
  emit_comment("End right hand side", s);

  emit_comment("Locate the left hand side", s);
  if (int *local_offset = env->local_table->lookup(name)) {
    emit_comment("Is local variable", s);
    emit_partial_load_address(T1, s);  emit_protobj_ref(env->self_object, s);  s << endl;
    // emit_load(T1, *local_offset, T1, s);
    emit_addiu(T1, T1, WORD_SIZE * (*local_offset), s);
  } else {
    // We can assume that the variable does exist since we've done the
    // semantic check.
    emit_comment("Is attribute", s);
    CgenNode *class_node = env->lookup(env->self_object);
    int attribute_offset = class_node->get_attribute_offset(name);
    // emit_load(T1, DEFAULT_OBJFIELDS + attribute_offset, SELF, s);
    emit_addiu(T1, SELF, WORD_SIZE * (DEFAULT_OBJFIELDS + attribute_offset), s);
  }
  emit_comment("End left hand side", s);
  // Now T1 contains the address of the lhs, ACC contains the value of the rhs.
  emit_store(ACC, 0, T1, s);
  emit_comment("End assign", s);
}

void static_dispatch_class::code(ostream &s, CgenClassTableP env) {
}

void dispatch_class::code(ostream &s, CgenClassTableP env) {
  // Evaluate and push the actuals.
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s, env);  // the result value is always placed in ACC
    emit_push(ACC, s);
  }

  // Evaluate the object in dispatch.
  expr->code(s, env);

  Symbol classname = expr->get_type();
  // Pass SELF through ACC.
  if (classname == SELF_TYPE) {
    emit_move(ACC, SELF, s);
    classname = env->self_object;
    if (cgen_debug) {
      cout << "\tDispatching to SELF_TYPE, resolved as " << classname << endl;
    }
  } else {
    emit_partial_load_address(ACC, s);  emit_protobj_ref(classname, s);  s << endl;
    if (cgen_debug) {
      cout << "\tDispatching to " << classname << endl;
    }
  }

  // Locate the method.
  emit_load(T1, DISPTABLE_OFFSET, ACC /* self of expr */, s);
  const int offset = env->lookup(classname)->get_method_offset(name);
  emit_load(T1, offset, T1, s);
  if (cgen_debug) {
    cout << "\tMethod " << name << " at offset " << offset << endl;
  }

  // Call the method.
  emit_jalr(T1, s);
}

void cond_class::code(ostream &s, CgenClassTableP env) {
}

void loop_class::code(ostream &s, CgenClassTableP env) {
}

void typcase_class::code(ostream &s, CgenClassTableP env) {
}

void block_class::code(ostream &s, CgenClassTableP env) {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->code(s, env);
  }
}

void let_class::code(ostream &s, CgenClassTableP env) {
}

using EmitArithmeticFp = void (*)(char *, char *, char *, ostream &);

static void code_arithmetic(Expression e1, Expression e2,
                            EmitArithmeticFp emit_arithmetic, ostream &s,
                            CgenClassTableP env) {
  emit_comment("Evaluate left operand", s);
  e1->code(s, env);
  emit_comment("Save left operand", s);
  emit_push(ACC, s);
  emit_comment("Evaluate right operand", s);
  e2->code(s, env);
  emit_comment("Save right operand", s);
  emit_push(ACC, s);
  // ACC now contains the pointer to a the right operand,
  // we'll copy one from it as the result Object.
  emit_comment("Create result object", s);
  s << JAL;
  emit_method_ref(Object, ::copy, s);
  s << endl;
  emit_comment("Restore right operand", s);
  emit_pop(T2, s);
  emit_comment("Restore left operand", s);
  emit_pop(T1, s);
  emit_comment("Get int value of right operand", s);
  emit_load(T2, 3, T2, s);
  emit_comment("Get int value of left operand", s);
  emit_load(T1, 3, T1, s);
  emit_comment("Place the result into the left operand, just to save register",
               s);
  emit_arithmetic(T1, T1, T2, s);
  emit_store(T1, 3, ACC, s);
}

void plus_class::code(ostream &s, CgenClassTableP env) {
  code_arithmetic(e1, e2, emit_add, s, env);
}

void sub_class::code(ostream &s, CgenClassTableP env) {
  code_arithmetic(e1, e2, emit_sub, s, env);
}

void mul_class::code(ostream &s, CgenClassTableP env) {
  code_arithmetic(e1, e2, emit_mul, s, env);
}

void divide_class::code(ostream &s, CgenClassTableP env) {
  code_arithmetic(e1, e2, emit_div, s, env);
}

void neg_class::code(ostream &s, CgenClassTableP env) {
}

void lt_class::code(ostream &s, CgenClassTableP env) {
}

void eq_class::code(ostream &s, CgenClassTableP env) {
}

void leq_class::code(ostream &s, CgenClassTableP env) {
}

void comp_class::code(ostream &s, CgenClassTableP env) {
}

void int_const_class::code(ostream &s, CgenClassTableP env)
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream &s, CgenClassTableP env)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream &s, CgenClassTableP env)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, CgenClassTableP env) {
  if (cgen_debug) {
    cout << "\tNewing object " << type_name << "..." << endl;
  }
  // 1. load the address of the prototype object
  // 2. make a copy
  // 3. init such object
  emit_partial_load_address(ACC, s);  emit_protobj_ref(type_name, s);  s << endl;
  s << JAL;  emit_method_ref(Object, ::copy, s);  s << endl;
  s << JAL;  emit_init_ref(type_name, s);  s << endl;

}

void isvoid_class::code(ostream &s, CgenClassTableP env) {
}

void no_expr_class::code(ostream &s, CgenClassTableP env) {
}

void object_class::code(ostream &s, CgenClassTableP env) {
  emit_comment("Locate object expression", s);
  // Could be a local or an attribute, or even self.
  if (name == self) {
    emit_comment("Is self", s);
    emit_move(ACC, SELF, s);
  } else if (int *local_offset = env->local_table->lookup(name)) {
    emit_comment("Is local variable", s);
    emit_partial_load_address(T1, s);  emit_protobj_ref(env->self_object, s);  s << endl;
    emit_load(ACC, *local_offset, T1, s);
  } else {
    // We can assume that the variable does exist since we've done the
    // semantic check.
    emit_comment("Is attribute", s);
    CgenNode *class_node = env->lookup(env->self_object);
    int attribute_offset = class_node->get_attribute_offset(name);
    emit_load(ACC, DEFAULT_OBJFIELDS + attribute_offset, SELF, s);
  }
  emit_comment("End object expression", s);
}
