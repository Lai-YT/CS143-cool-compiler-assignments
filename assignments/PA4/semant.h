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
  ostream& error_stream;

  /*
   * Common helpers.
   */

  std::unordered_set<Symbol> basic_classes{};
  bool IsBasic(Symbol name) const;
  std::unordered_set<Symbol> final_classes{};
  bool IsFinal(Symbol name) const;
  bool HasClass(Symbol name) const;
  // Up until Object.
  std::vector<Class_> GetParents(const Class_) const;

  /*
   * Checks done in the first pass.
   */

  void install_basic_classes();
  void InstallClasses(Classes);
  // Shows semantic error if Redefinition occurs.
  void AddClass(Class_ c);
  void ShowRedefinitionError(Class_ c);

  /*
   * Checks related to class.
   */

  void CheckNoInheritanceFromFinal();
  void ShowInheritanceFromFinalError(Class_ c);
  void CheckNoUndeclaredBaseClass();
  void ShowUndeclaredBaseClassError(Class_ c);
  void CheckNoCircularInheritance();
  void ShowCircularInheritanceError(Class_ c);
  void CheckHasMainClassAndMainMethod();

  /*
   * Checks related to method.
   */

  void CheckNoRedefinedAttr(Class_ c);
  /// @brief Checks whether the method as identical signature with the original
  /// method. Shows error message if not.
  /// @param method the redefined method to check
  /// @param pmethod the original method in parent
  /// @param filename where method locates. Used in error message.
  void CheckNoMismatch(Method method, Method pmethod, Symbol filename);
  void CheckReturnType(Method method, Method pmethod, Symbol filename);
  void CheckNumberOfFormals(Method method, Method pmethod, Symbol filename);
  void CheckFormalTypes(Method method, Method pmethod, Symbol filename);
  void CheckNoUndefinedAttrType(Class_ c);
  void CheckNoFormalNamedSelf(const Method method, const Symbol filename);
  void CheckNoUndefinedFormalType(const Method method, const Symbol filename);
  void CheckNoRedefinedFormal(const Method method, const Symbol filename);
  void CheckNoUndefinedReturnType(const Method method, const Symbol filename);

  Classes classes;

 public:
  ClassTable(Classes);
  /// @brief Does the class-related checks.
  /// The checks are
  /// (1) no inheritance on final basic classes,
  /// (2) no inheritance on undeclared classes,
  /// (3) no circular inheritance,
  /// (4) has a Main class with a main method.
  /// These four checks are done in order, and errors on the former checks
  /// disable the latter checks (since they are meaningless under such errors).
  void CheckClasses();
  /// @brief Does the method-related checks.
  /// The checks are
  /// (1) no multiply defined attribute
  /// (2) no difference from original
  /// (3) no multiply defined method is a single class
  /// (4) has Main class and main method
  /// (5) no undefined attribute type
  /// (6) no formal named self
  /// (7) no undefined formal type
  /// (8) no multiply defined formal
  /// (9) no undefined return type
  /// Keep checking even though their are previous errors.
  /// @note (1) is checked first along with (2) and (3) over all classes, then
  /// the remain checks are done method by method.
  void CheckMethods();
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};

bool IsMethod(const Feature f);
std::vector<Method> GetMethods(const Class_);

#endif
