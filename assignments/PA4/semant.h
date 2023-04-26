#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <map>
#include <unordered_set>
#include <vector>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

typedef method_class *Method;


// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable : public std::map<Symbol, Class_> {
private:
  int semant_errors;
  std::unordered_set<Symbol> basic_classes{};
  bool IsBasic(Symbol name) const;
  std::unordered_set<Symbol> final_classes{};
  bool IsFinal(Symbol name) const;
  void install_basic_classes();
  void InstallClasses(Classes);
  ostream& error_stream;
  void ShowRedefinitionError(Class_ c);
  void ShowInheritanceFromFinalError(Class_ c);
  void ShowUndeclaredBaseClassError(Class_ c);
  void ShowCircularInheritanceError(Class_ c);

  // Up until Object.
  std::vector<Class_> GetParents(const Class_) const;


  /*
   * these checks has to be called in a certain order
   */

  void CheckNoInheritanceFromFinal();
  void CheckNoUndeclaredBaseClass();
  void CheckNoCircularInheritance();
  void CheckHasMainClass();
  void CheckHasMainMethod();

  void CheckNoUndefinedReturnType();

  void CheckNoMismatchRedefinedMethod();
  /// @brief Checks whether the method as identical signature with the original
  /// method. Shows error message if not.
  /// @param method the redefined method to check
  /// @param pmethod the original method in parent
  /// @param filename where method locates. Used in error message.
  void CheckNoMismatch(Method method, Method pmethod, Symbol filename);
  void CheckReturnType(Method method, Method pmethod, Symbol filename);
  void CheckNumberOfFormals(Method method, Method pmethod, Symbol filename);
  void CheckFormalTypes(Method method, Method pmethod, Symbol filename);

 public:
  ClassTable(Classes);
  // Shows semantic error if Redefinition occurs.
  void AddClass(Class_ c);
  bool HasClass(Symbol name) const;
  int errors() { return semant_errors; }
  /// @brief Does the class-related checks.
  /// The checks are
  /// (1) no inheritance on final basic classes,
  /// (2) no inheritance on undeclared classes,
  /// (3) no circular inheritance,
  /// (4) has a Main class with a main method.
  /// These four checks are done in order, and errors on the former checks
  /// disable the latter checks (since they are meaningless under such errors).
  void CheckClasses();
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};

bool IsMethod(const Feature f);
std::vector<Method> GetMethods(const Class_);

#endif
