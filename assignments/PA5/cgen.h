#include <assert.h>
#include <stdio.h>
#include <unordered_map>
#include <utility>
#include <vector>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class LocalTable;

typedef int ClassTag;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_prototype_objects();
   void code_class_name_table();
   void code_class_object_table();
   void code_dispatch_tables();
   void code_class_inits();
   void code_class_methods();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);

   void build_dispatch_layouts();
   void build_attribute_layouts();
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();

   // The CgenClassTable itself is an environment that maps the class attributes
   // & methods to their location (offset), though not directly.
   // We need one more mapping to map the local variables to their location.

   // Note that arguments are also in the category of locals, and their offsets
   // are negative, while others are always positive.
   LocalTable *local_table;

   // Records the class which the current SELF_TYPE holds.
   Symbol self_object;
};

class LocalTable : public SymbolTable<Symbol, int /* offset */> {
   using Base = SymbolTable<Symbol, int>;
   using Base::SymbolTable;

private:
 // Every time a new method is entered, a new offset (-1 * number of callee
 // saved) is pushed. Keep counting if a let scope is entered since it doesn't
 // use a new frame.
 List<int> *next_local_offsets = nullptr;
 int get_next_local_offset();

public:
   // Note that if is method scope, don't call enterscope.
   void enter_method_scope();
   // Note that if is method scope, don't call exitscope.
   void exit_method_scope();
   void enterscope();
   void exitscope();
   void addid(Symbol id);
   // Especially for parameters, since they have positive offsets.
   void addid(Symbol id, int offset);
};

class CgenNode : public class__class {
private:
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   static ClassTag next_class_tag;
   ClassTag class_tag = -1;
   void set_class_tag();

   //
   // For methods to be overridable while keeping the offset of such method as
   // same as its parent, we use a vector to represent the layout of the methods
   // and a map to record the offset of methods for fast looking ups.
   //

   // All methods of the class in the order of their offsets.
   std::vector<std::pair<Symbol /* implementor */, method_class *>>
       dispatch_layout;
   std::unordered_map<Symbol /* method name */, int /* offset */>
       dispatch_offsets;

   // All attributes of the class in the order of their offsets.
   std::vector<std::pair<Symbol /* implementor*/, attr_class *>>
       attribute_layout;
   std::unordered_map<Symbol /* attribute name */, int /* offset */>
       attribute_offsets;

  public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() const { return parentnd; }
   int basic() { return (basic_status == Basic); }

   void build_dispatch_layout();
   void build_attribute_layout();
   int get_attribute_offset(Symbol attribute) const;
   int get_method_offset(Symbol method) const;

   ClassTag get_class_tag() const { return class_tag; }
   void code_prototype_object(ostream&) const;
   void code_attributes(ostream&) const;
   void code_dispatch_table(ostream&) const;
   void code_class_init(ostream&, CgenClassTableP) const;
   void code_class_method(ostream&, CgenClassTableP) const;
};

class BoolConst
{
 private:
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};
