#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;


// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable : public std::unordered_map<Symbol, Class_> {
private:
  int semant_errors;
  std::unordered_set<Symbol> basic_classes{};
  std::unordered_set<Symbol> final_classes{};
  void install_basic_classes();
  void InstallClasses(Classes);
  ostream& error_stream;
  void CheckNoInheritingFromBasic();
  void CheckDeclaration();
  void CheckCircularInheritance();

public:
  ClassTable(Classes);
  void AddClass(Class_ c);
  bool HasClass(Symbol name) const;
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif
