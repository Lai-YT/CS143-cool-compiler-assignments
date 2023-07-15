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
};


class CgenNode : public class__class {
private:
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   static int next_classtag;
   int classtag = -1;
   void set_classtag();

   //
   // For methods to be overridable while keeping the offset of such method as
   // same as its parent, we use a vector to represent the layout of the methods
   // and a map to record the offset of methods for fast looking ups.
   //

   // All methods of the class in the order of their offsets.
   std::vector<std::pair<Symbol /* implementor*/, method_class *>>
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

   int get_classtag() const { return classtag; }
   void code_prototype_object(ostream&) const;
   void code_attributes(ostream&) const;
   void code_dispatch_table(ostream&) const;
   void code_class_init(ostream&) const;
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
