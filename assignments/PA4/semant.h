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

/// @brief The class & method checks make sure the inheritance graph is
/// well-formed, but detail type checks are not performed.
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
  /// @brief Almost all of the checks only have to be performed on user-defined
  /// classes (non-basic). This helper method serves such purpose.
  std::vector<Class_> GetUserDefinedClasses() const;

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

 public:
  ClassTable(Classes);
  /// @brief Does the class-related checks.
  /// The checks are
  /// (1) no inheritance on final basic classes,
  /// (2) no inheritance on undeclared classes,
  /// (3) no circular inheritance,
  /// These checks are done in order, and errors on the former checks disable
  /// the latter checks (since they are meaningless under such errors).
  void CheckClasses();
  /// @brief Does the method-related checks.
  /// The checks are
  /// (1) no multiply defined attribute
  /// (2) no difference from original
  /// (3) no multiply defined method is a single class
  /// Keep checking even though their are previous errors.
  void CheckMethods();
  void CheckHasMainClassAndMainMethod();
  /// @returns From c (excluded) to Object, i.e., if c is already Object, an
  /// empty vector is returned.
  /// @note Goes into infinite loop if contains circular inheritance.
  std::vector<Class_> GetParents(const Class_ c) const;
  bool HasClass(Symbol name) const;
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};

bool IsMethod(const Feature f);
std::vector<Method> GetMethods(const Class_);

class Visitor {
  /*
   * Define the visit method for all nodes.
   * They do nothing by default.
   */
 public:
  virtual void VisitProgram(program_class *){};
  virtual void VisitClass(class__class *){};
  virtual void VisitAttr(attr_class *){};
  virtual void VisitMethod(method_class *){};

  /*
   * Expressions
   */

  virtual void VisitBranch(branch_class *){};
  virtual void VisitAssign(assign_class *){};
  virtual void VisitCond(cond_class *){};
  virtual void VisitStaticDispatch(static_dispatch_class *){};
  virtual void VisitDispatch(dispatch_class *){};
  virtual void VisitLoop(loop_class *){};
  virtual void VisitTypcase(typcase_class *){};
  virtual void VisitBlock(block_class *){};
  virtual void VisitLet(let_class *){};
  virtual void VisitNeg(neg_class *){};
  virtual void VisitComp(comp_class *){};
  virtual void VisitInt(int_const_class *){};
  virtual void VisitBool(bool_const_class *){};
  virtual void VisitString(string_const_class *){};
  virtual void VisitNew(new__class *){};
  virtual void VisitIsvoid(isvoid_class *){};
  virtual void VisitObject(object_class *){};
  virtual void VisitNoExpr(no_expr_class *){};

  /*
   * Arithmetic expressions
   */

  virtual void VisitPlus(plus_class *){};
  virtual void VisitSub(sub_class *){};
  virtual void VisitMul(mul_class *){};
  virtual void VisitDivide(divide_class *){};

  /*
   * Comparison expressions
   */

  virtual void VisitEq(eq_class *){};
  virtual void VisitLt(lt_class *){};
  virtual void VisitLeq(leq_class *){};

  virtual ~Visitor() = default;
};

#endif
