#include <algorithm>
#include <assert.h>
#include <ostream>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <symtab.h>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include "cool-tree.h"
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
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
static void initialize_constants(void) {
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

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr) {
    install_basic_classes();

    // Add user-defined classes.
    // Class redefinitions are detected early in this stage.
    InstallClasses(classes);
}

void ClassTable::CheckClasses() {
    CheckNoInheritanceFromFinal();
    CheckNoUndeclaredBaseClass();
    if (semant_errors) {
        // the graph is broken so no further circular check
        return;
    }
    CheckNoCircularInheritance();
    if (semant_errors) {
        // the graph is ill-formed so no further check
        return;
    }
}

/// @brief For dispatch check.
static std::unordered_map<Symbol /* class name */,
                          std::unordered_map<Symbol /* method name */, Method>>
    method_table;

static std::unordered_map<
    Symbol /* class name */,
    std::unordered_map<Symbol /* attr name */, attr_class *>>
    attr_table;

void ClassTable::CheckFeatures() {
    for (const auto [name, clss] : *this) {
        CheckNoRedefinedAttr(clss);
        std::unordered_map<Symbol, Method> defined_methods{};
        std::unordered_map<Symbol, attr_class *> defined_attrs{};
        const Features features = clss->GetFeatures();
        for (int i = features->first(); features->more(i);
             i = features->next(i)) {
            const Feature feature = features->nth(i);
            if (const auto method = dynamic_cast<Method>(feature)) {
                for (const auto parent : GetParents(clss)) {
                    for (const Method pmethod : GetMethods(parent)) {
                        if (pmethod->GetName() == method->GetName()) {
                            CheckNoMismatch(method, pmethod,
                                            clss->get_filename());
                            break;
                        }
                    }
                }
                // Check no multiply defined method in a single class.
                // Multiply defined methods are not added into the method table.
                if (defined_methods.find(method->GetName())
                    != defined_methods.cend()) {
                    semant_error(clss->get_filename(), method)
                        << "Method " << method->GetName()
                        << " is multiply defined.\n";
                    continue;
                }
                defined_methods.insert({method->GetName(), method});
            } else if (const auto attr = dynamic_cast<attr_class *>(feature)) {
                for (auto parent : GetParents(clss)) {
                    auto parent_attrs = GetAttrs(parent);
                    // Check whether there's an attribute with the same name in
                    // parents. Redefined inherited attributes are not added
                    // into the attribute table.
                    if (auto itr = std::find_if(
                            parent_attrs.begin(), parent_attrs.end(),
                            [attr](attr_class *pattr) {
                                return pattr->GetName() == attr->GetName();
                            });
                        itr != parent_attrs.cend()) {
                        semant_error(clss->get_filename(), attr)
                            << "Attribute " << attr->GetName()
                            << " is an attribute of an inherited class.\n";
                        break;
                    }
                    defined_attrs.insert({attr->GetName(), attr});
                }
            }
        }
        method_table[name] = defined_methods;
        attr_table[name] = defined_attrs;
    }
}

void ClassTable::InstallClasses(Classes classes) {
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        AddClass(classes->nth(i));
    }
}

void ClassTable::AddClass(Class_ c) {
    const Symbol name = c->GetName();
    if (HasClass(name) || name == SELF_TYPE) {
        ShowRedefinitionError(c);
    } else {
        emplace(name, c);
    }
}

void ClassTable::ShowRedefinitionError(Class_ c) {
    const Symbol name = c->GetName();
    if (IsBasic(name)) {
        semant_error(c) << "Redefinition of basic class " << name << ".\n";
    } else {
        semant_error(c) << "Class " << name << " was previously defined.\n";
    }
}

bool ClassTable::HasClass(Symbol name) const {
    return find(name) != cend();
}

bool ClassTable::IsBasic(Symbol name) const {
    return basic_classes.find(name) != basic_classes.cend();
}

bool ClassTable::IsFinal(Symbol name) const {
    return final_classes.find(name) != final_classes.cend();
}

void ClassTable::install_basic_classes() {
    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    Class_ Object_class =
	class_(Object,
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);
    AddClass(Object_class);
    basic_classes.insert(Object_class->GetName());

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
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
	       filename);
    AddClass(IO_class);
    basic_classes.insert(IO_class->GetName());

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
	class_(Int,
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);
    AddClass(Int_class);
    basic_classes.insert(Int_class->GetName());
    final_classes.insert(Int_class->GetName());

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
    AddClass(Bool_class);
    basic_classes.insert(Bool_class->GetName());
    final_classes.insert(Bool_class->GetName());

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class =
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
	       filename);
    AddClass(Str_class);
    basic_classes.insert(Str_class->GetName());
    final_classes.insert(Str_class->GetName());

    basic_classes.insert(SELF_TYPE);
    final_classes.insert(SELF_TYPE);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream &ClassTable::semant_error(Class_ c) {
    return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t) {
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream &ClassTable::semant_error() {
    semant_errors++;
    return error_stream;
}

/*
 * Errors should be reported in descending order by the line number.
 */
void ClassTable::CheckNoInheritanceFromFinal() {
    auto user_defined_classes = GetUserDefinedClasses();
    for (auto iter = user_defined_classes.rbegin();
         iter != user_defined_classes.rend(); iter++) {
        Class_ clss = *iter;
        if (IsFinal(clss->GetParentName())) {
            ShowInheritanceFromFinalError(clss);
        }
    }
}

void ClassTable::ShowInheritanceFromFinalError(Class_ c) {
    semant_error(c) << "Class " << c->GetName() << " cannot inherit class "
                    << c->GetParentName() << ".\n";
}

/*
 * Errors should be reported in descending order by the line number.
 */
void ClassTable::CheckNoUndeclaredBaseClass() {
    auto user_defined_classes = GetUserDefinedClasses();
    for (auto iter = user_defined_classes.rbegin();
         iter != user_defined_classes.rend(); iter++) {
        Class_ clss = *iter;
        const Symbol pname = clss->GetParentName();
        if (!HasClass(pname) && pname != No_class && pname != SELF_TYPE) {
            ShowUndeclaredBaseClassError(clss);
        }
    }
}

void ClassTable::ShowUndeclaredBaseClassError(Class_ c) {
    semant_error(c) << "Class " << c->GetName()
                    << " inherits from an undefined class "
                    << c->GetParentName() << ".\n";
}

/*
 * Errors should be reported in descending order by the line number.
 * Throws Error if there's any circular inheritance.
 */
void ClassTable::CheckNoCircularInheritance() {
    auto user_defined_classes = GetUserDefinedClasses();
    for (auto iter = user_defined_classes.rbegin();
         iter != user_defined_classes.rend(); iter++) {
        Class_ clss = *iter;
        for (Symbol pname = clss->GetParentName(); pname != No_class;
             pname = at(pname)->GetParentName()) {
            if (pname == clss->GetName()) {
                ShowCircularInheritanceError(clss);
                break;
            }
        }
    }
}

void ClassTable::ShowCircularInheritanceError(Class_ c) {
    semant_error(c) << "Class " << c->GetName() << ", or an ancestor of "
                    << c->GetName() << ", is involved in an inheritance cycle."
                    << '\n';
}

void ClassTable::CheckHasMainClassAndMainMethod() {
    if (HasClass(Main)) {
        const Class_ main = at(Main);
        for (const Method method : GetMethods(main)) {
            if (method->GetName()->equal_string("main", 4)) {
                return;
            }
        }
        semant_error(main) << "No 'main' method in class " << Main << ".\n";
    } else {
        semant_error() << "Class " << Main << " is not defined." << '\n';
    }
}

/*
 * When facing multiple errors on a single method, only one of them is reported.
 * The precedence is (1) return type (2) number of formal (3) formal type".
 */
void ClassTable::CheckNoMismatch(Method method, Method pmethod,
                                 Symbol filename) {
    const int errors_before_check = errors();
    CheckReturnType(method, pmethod, filename);
    if (errors() == errors_before_check) {
        CheckNumberOfFormals(method, pmethod, filename);
    }
    if (errors() == errors_before_check) {
        CheckFormalTypes(method, pmethod, filename);
    }
}

void ClassTable::CheckReturnType(Method method, Method pmethod,
                                 Symbol filename) {
    if (method->GetReturnType() != pmethod->GetReturnType()) {
        semant_error(filename, method)
            << "In redefined method " << method->GetName() << ", return type "
            << method->GetReturnType()
            << " is different from original return type "
            << pmethod->GetReturnType() << ".\n";
    }
}

void ClassTable::CheckNumberOfFormals(Method method, Method pmethod,
                                      Symbol filename) {
    const Formals formals = method->GetFormals();
    const Formals pformals = pmethod->GetFormals();
    if (formals->len() != pformals->len()) {
        semant_error(filename, method)
            << "Incompatible number of formal parameters in redefined method "
            << method->GetName() << ".\n";
    }
}

void ClassTable::CheckFormalTypes(Method method, Method pmethod,
                                  Symbol filename) {
    const Formals formals = method->GetFormals();
    const Formals pformals = pmethod->GetFormals();
    for (int i = 0; formals->more(i) && pformals->more(i); i++) {
        const Symbol formal_type = formals->nth(i)->GetDeclType();
        const Symbol pformal_type = pformals->nth(i)->GetDeclType();
        if (formal_type != pformal_type) {
            semant_error(filename, method)
                << "In redefined method " << method->GetName()
                << ", parameter type " << formal_type
                << " is different from original type " << pformal_type << '\n';
        }
    }
}

std::vector<Method> GetMethods(const Class_ clss) {
    std::vector<Method> methods;
    const Features fs = clss->GetFeatures();
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
        const Feature feature = fs->nth(i);
        if (IsMethod(feature)) {
            methods.push_back(dynamic_cast<Method>(feature));
        }
    }
    return methods;
}

std::vector<attr_class *> GetAttrs(const Class_ clss) {
    std::vector<attr_class *> attrs{};
    const Features fs = clss->GetFeatures();
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
        const Feature feature = fs->nth(i);
        if (auto attr = dynamic_cast<attr_class *>(feature)) {
            attrs.push_back(attr);
        }
    }
    return attrs;
}

void ClassTable::CheckNoRedefinedAttr(Class_ c) {
    std::unordered_set<Symbol> defined_attrs{};
    Features features = c->GetFeatures();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        if (auto attr = dynamic_cast<attr_class *>(features->nth(i))) {
            if (defined_attrs.find(attr->GetName()) != defined_attrs.cend()) {
                semant_error(c->get_filename(), attr)
                    << "Attribute " << attr->GetName()
                    << " is multiply defined in class.\n";
            } else {
                defined_attrs.insert(attr->GetName());
            }
        }
    }
}

// The method of class nodes are defined here to avoid circular including.

void program_class::Accept(Visitor *visitor) {
    visitor->VisitProgram(this);
}

void class__class::Accept(Visitor *visitor) {
    visitor->VisitClass(this);
}

void method_class::Accept(Visitor *visitor) {
    visitor->VisitMethod(this);
}

void attr_class::Accept(Visitor *visitor) {
    visitor->VisitAttr(this);
}

void branch_class::Accept(Visitor *visitor) {
    visitor->VisitBranch(this);
}

void assign_class::Accept(Visitor *visitor) {
    visitor->VisitAssign(this);
}

void cond_class::Accept(Visitor *visitor) {
    visitor->VisitCond(this);
}

void static_dispatch_class::Accept(Visitor *visitor) {
    visitor->VisitStaticDispatch(this);
}

void dispatch_class::Accept(Visitor *visitor) {
    visitor->VisitDispatch(this);
}

void loop_class::Accept(Visitor *visitor) {
    visitor->VisitLoop(this);
}

void typcase_class::Accept(Visitor *visitor) {
    visitor->VisitTypcase(this);
}

void block_class::Accept(Visitor *visitor) {
    visitor->VisitBlock(this);
}

void let_class::Accept(Visitor *visitor) {
    visitor->VisitLet(this);
}

void neg_class::Accept(Visitor *visitor) {
    visitor->VisitNeg(this);
}

void comp_class::Accept(Visitor *visitor) {
    visitor->VisitComp(this);
}

void int_const_class::Accept(Visitor *visitor) {
    visitor->VisitInt(this);
}

void bool_const_class::Accept(Visitor *visitor) {
    visitor->VisitBool(this);
}

void string_const_class::Accept(Visitor *visitor) {
    visitor->VisitString(this);
}

void new__class::Accept(Visitor *visitor) {
    visitor->VisitNew(this);
}

void object_class::Accept(Visitor *visitor) {
    visitor->VisitObject(this);
}

void plus_class::Accept(Visitor *visitor) {
    visitor->VisitPlus(this);
}

void sub_class::Accept(Visitor *visitor) {
    visitor->VisitSub(this);
}

void mul_class::Accept(Visitor *visitor) {
    visitor->VisitMul(this);
}

void divide_class::Accept(Visitor *visitor) {
    visitor->VisitDivide(this);
}

void eq_class::Accept(Visitor *visitor) {
    visitor->VisitEq(this);
}

void leq_class::Accept(Visitor *visitor) {
    visitor->VisitLeq(this);
}

void lt_class::Accept(Visitor *visitor) {
    visitor->VisitLt(this);
}

void isvoid_class::Accept(Visitor *visitor) {
    visitor->VisitIsvoid(this);
}

void no_expr_class::Accept(Visitor *visitor) {
    visitor->VisitNoExpr(this);
}

/// @brief Checks whether the type of expressions conform to the definitions,
/// and sets the inferred types to the class tree.
class TypeCheckVisitor : public Visitor {
   public:
    /// @param table The class table of the tree to be type checked.
    /// @note Type checking on a ill-formed class tree causes unexpected errors.
    TypeCheckVisitor(ClassTable *table) : table_{table} {}

    void VisitProgram(program_class *program) {
        Classes classes = program->GetClasses();
        for (int i = 0; classes->more(i); i = classes->next(i)) {
            curr_clss_ = classes->nth(i);
            curr_clss_->Accept(this);
        }
    }

    void VisitClass(class__class *clss) override {
        obj_env.enterscope();
        // add attributes defined in this class
        for (const auto attr : GetAttrs(clss)) {
            obj_env.addid(attr->GetName(), new Symbol(attr->GetDeclType()));
        }
        // add attributes inherited from parent
        for (const auto parent : table_->GetParents(clss)) {
            for (const auto [attr_name, attr] : attr_table.at(parent->GetName())) {
                obj_env.addid(attr_name, new Symbol(attr->GetDeclType()));
            }
        }
        Features features = clss->GetFeatures();
        for (int i = features->first(); features->more(i);
             i = features->next(i)) {
            features->nth(i)->Accept(this);
        }
        obj_env.exitscope();
    }

    void VisitMethod(method_class *method) override {
        CheckNoFormalNamedSelf_(method);
        CheckNoUndefinedFormalType_(method);
        CheckNoRedefinedFormal_(method);
        CheckNoUndefinedReturnType_(method);
        obj_env.enterscope();
        // extend with self
        obj_env.addid(self, new Symbol(SELF_TYPE));
        // extend with formals
        Formals formals = method->GetFormals();
        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
            obj_env.addid(formals->nth(i)->GetName(),
                          new Symbol(formals->nth(i)->GetDeclType()));
        }
        method->GetExpr()->Accept(this);
        if (!Conform_(method->GetExpr()->get_type(), method->GetReturnType())) {
            table_->semant_error(curr_clss_->get_filename(), method)
                << "Inferred return type " << method->GetExpr()->get_type()
                << " of method " << method->GetName()
                << " does not conform to declared return type "
                << method->GetReturnType() << ".\n";
        }
        obj_env.exitscope();
    }

    void VisitAssign(assign_class *assign) override {
        assign->GetExpr()->Accept(this);
        Symbol *id_type = obj_env.lookup(assign->GetName());
        if (!id_type) {
            table_->semant_error(curr_clss_->get_filename(), assign)
                << "Assignment to undeclared variable " << assign->GetName()
                << ".\n";
        } else {
            if (!Conform_(assign->GetExpr()->get_type(), *id_type)) {
                table_->semant_error(curr_clss_->get_filename(), assign)
                    << "Type " << assign->GetExpr()->get_type()
                    << " of assigned expression does not conform to declared "
                       "type "
                    << *id_type << " of identifier " << assign->GetName()
                    << ".\n";
            }
        }
        // the type of the assignment is the type of the expression on the right
        // hand side, no matter what happens on the left hand side.
        assign->set_type(assign->GetExpr()->get_type());
    }

    void VisitAttr(attr_class *attr) override {
        obj_env.enterscope();
        // self is bounded in the initialization
        obj_env.addid(self, new Symbol(SELF_TYPE));
        attr->GetInit()->Accept(this);
        obj_env.exitscope();
        if (!table_->HasClass(attr->GetDeclType())
            && attr->GetDeclType() != SELF_TYPE) {
            table_->semant_error(curr_clss_->get_filename(), attr)
                << "Class " << attr->GetDeclType() << " of attribute "
                << attr->GetName() << " is undefined.\n";
        }
        if (!Conform_(attr->GetInit()->get_type(), attr->GetDeclType())) {
            table_->semant_error(curr_clss_->get_filename(), attr)
                << "Inferred type " << attr->GetInit()->get_type()
                << " of initialization of attribute " << attr->GetName()
                << " does not conform to declared type " << attr->GetDeclType()
                << ".\n";
        }
    }

    void VisitBlock(block_class *block) override {
        Expressions exprs = block->GetExpressions();
        for (int i = exprs->first(); exprs->more(i); i = exprs->next(i)) {
            exprs->nth(i)->Accept(this);
        }
        block->set_type(
            exprs->nth(exprs->len() - 1 /* last expr*/)->get_type());
    }

    void VisitLet(let_class *let) override {
        /*
         * init is type checked in an environment without a new definition
         */
        let->GetInit()->Accept(this);
        bool is_undefined_type = false;
        if (!table_->HasClass(ResolveSelfType_(let->GetIdDeclType()))) {
            is_undefined_type = true;
            table_->semant_error(curr_clss_->get_filename(), let)
                << "Class " << let->GetIdDeclType()
                << " of let-bound identifier " << let->GetIdName()
                << " is undefined.\n";
        } else if (Symbol init_type = let->GetInit()->get_type();
                   init_type != No_type && init_type != let->GetIdDeclType()) {
            /*
             * no "does not conform" error if the identifier type is undefined
             */
            table_->semant_error(curr_clss_->get_filename(), let)
                << "Inferred type " << init_type << " of initialization of "
                << let->GetIdName()
                << " does not conform to identifier's declared type "
                << let->GetIdDeclType() << ".\n";
        }
        /*
         * body is type checked in the environment extended
         */
        obj_env.enterscope();
        obj_env.addid(
            let->GetIdName(),
            new Symbol(
                is_undefined_type
                    ? No_type  // recovery: using No_type, which is a sub-type
                               // of any type, to avoid cascading errors
                    : let->GetIdDeclType()));
        let->GetBody()->Accept(this);
        obj_env.exitscope();
        let->set_type(let->GetBody()->get_type());
    }

    void VisitCond(cond_class *cond) override {
        cond->GetPredicate()->Accept(this);
        if (cond->GetPredicate()->get_type() != Bool) {
            table_->semant_error(curr_clss_->get_filename(), cond)
                << "Predicate of 'if' does not have type Bool.\n";
        }
        cond->GetThenExpr()->Accept(this);
        cond->GetElseExpr()->Accept(this);
        cond->set_type(JoinType_(cond->GetThenExpr()->get_type(),
                                 cond->GetElseExpr()->get_type()));
    }

    void VisitLoop(loop_class *loop) override {
        loop->GetPredicate()->Accept(this);
        if (loop->GetPredicate()->get_type() != Bool) {
            table_->semant_error(curr_clss_->get_filename(), loop)
                << "Loop condition does not have type Bool.\n";
        }
        loop->GetBody()->Accept(this);
        // the type of the entire loop is always Object since it may not be
        // evaluated even once when the predicate is false at the first check
        loop->set_type(Object);
    }

    void VisitTypcase(typcase_class *typcase) override {
        typcase->GetExpr()->Accept(this);
        Cases cases = typcase->GetCases();
        std::unordered_set<Symbol> branch_types{};
        std::unordered_set<Symbol> expr_types{};
        for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
            Case_class *case_ = cases->nth(i);
            bool is_undefined_type = false;
            if (branch_types.find(case_->GetDeclType())
                != branch_types.cend()) {
                table_->semant_error(curr_clss_->get_filename(), case_)
                    << "Duplicate branch " << case_->GetDeclType()
                    << " in case statement.\n";
            } else if (!table_->HasClass(case_->GetDeclType())) {
                table_->semant_error(curr_clss_->get_filename(), case_)
                    << "Class " << case_->GetDeclType()
                    << " of case branch is undefined.\n";
                is_undefined_type = true;
            } else {
                branch_types.insert(case_->GetDeclType());
            }
            obj_env.enterscope();
            obj_env.addid(
                case_->GetName(),
                new Symbol(
                    is_undefined_type
                        ? Object  // recovery: simply allow cascading errors
                        : case_->GetDeclType()));
            case_->GetExpr()->Accept(this);
            expr_types.insert(case_->GetExpr()->get_type());
            obj_env.exitscope();
        }
        typcase->set_type(JoinType_(expr_types));
    }

    void VisitBranch(branch_class *branch) override {
        // empty
        /*
         * XXX: Since typcase has to know whether the class is defined, i.e.,
         * they share additional information, I can't let the branch does the
         * check.
         */
    }

    void VisitDispatch(dispatch_class *dispatch) override {
        dispatch->GetExpr()->Accept(this);
        /*
         * Actual expressions are always checked.
         * If the method is undefined, check that the number of arguments
         * matched and the actual types conform to the formal types.
         */
        CheckActuals_(dispatch->GetActuals());
        Symbol class_name = dispatch->GetExpr()->get_type();
        auto method = GetMethod_(class_name, dispatch->GetName());
        if (!method) {
            ShowUndefinedMethodError_(dispatch);
            // recovery: simply allow cascading errors
            dispatch->set_type(Object);
        } else {  // do formal conformance check
            if (dispatch->GetActuals()->len() != method->GetFormals()->len()) {
                ShowWrongNumberOfArgumentsError_(dispatch);
            } else {
                ConformActualsToFormals_(dispatch, method);
            }
            dispatch->set_type(method->GetReturnType() == SELF_TYPE
                                   ? class_name
                                   : method->GetReturnType());
        }
    }

    void VisitStaticDispatch(static_dispatch_class *static_dispatch) override {
        static_dispatch->GetExpr()->Accept(this);
        CheckActuals_(static_dispatch->GetActuals());
        bool cannot_resolve_dispatch_method = false;
        Symbol expr_type = static_dispatch->GetExpr()->get_type();
        if (!table_->HasClass(static_dispatch->GetTypeName())) {
            cannot_resolve_dispatch_method = true;
            ShowUndefinedDispatchError_(static_dispatch);
        } else if (!Conform_(expr_type, static_dispatch->GetTypeName())) {
            cannot_resolve_dispatch_method = true;
            ShowExprTypeNotConformError_(expr_type, static_dispatch);
        } else if (!GetMethod_(static_dispatch->GetTypeName(),
                              static_dispatch->GetName())) {
            cannot_resolve_dispatch_method = true;
            ShowUndefinedMethodError_(static_dispatch);
        }
        if (cannot_resolve_dispatch_method) {
            // recovery: simply allow cascading errors
            static_dispatch->set_type(Object);
        } else {  // do formal conformance check
            const Method method = GetMethod_(static_dispatch->GetTypeName(),
                                            static_dispatch->GetName());
            if (static_dispatch->GetActuals()->len()
                != method->GetFormals()->len()) {
                ShowWrongNumberOfArgumentsError_(static_dispatch);
            } else {
                ConformActualsToFormals_(static_dispatch, method);
            }
            static_dispatch->set_type(method->GetReturnType() == SELF_TYPE
                                          ? expr_type
                                          : method->GetReturnType());
        }
    }

    void VisitPlus(plus_class *plus) override {
        CheckArithmeticHasIntArgs_(plus);
    }

    void VisitSub(sub_class *sub) override {
        CheckArithmeticHasIntArgs_(sub);
    }

    void VisitMul(mul_class *mul) override {
        CheckArithmeticHasIntArgs_(mul);
    }

    void VisitDivide(divide_class *divide) override {
        CheckArithmeticHasIntArgs_(divide);
    }

    void VisitComp(comp_class *comp) override {
        comp->GetExpr()->Accept(this);
        if (comp->GetExpr()->get_type() != Bool) {
            table_->semant_error(curr_clss_->get_filename(), comp)
                << "Argument of 'not' has type " << comp->GetExpr()->get_type()
                << " instead of Bool.\n";
        }
        // recovery: we know that a Bool is desired
        comp->set_type(Bool);
    }

    void VisitNeg(neg_class *neg) override {
        neg->GetExpr()->Accept(this);
        if (neg->GetExpr()->get_type() != Int) {
            table_->semant_error(curr_clss_->get_filename(), neg)
                << "Argument of '~' has type " << neg->GetExpr()->get_type()
                << " instead of Int.\n";
        }
        // recovery: we know that an Int is desired
        neg->set_type(Int);
    }

    void VisitLeq(leq_class *leq) override {
        leq->GetLeftOp()->Accept(this);
        leq->GetRightOp()->Accept(this);
        const Symbol left_op_type = leq->GetLeftOp()->get_type();
        const Symbol right_op_type = leq->GetRightOp()->get_type();
        if (left_op_type != Int || right_op_type != Int) {
            table_->semant_error(curr_clss_->get_filename(), leq)
                << "non-Int arguments: " << left_op_type
                << " <= " << right_op_type << "\n";
        }
        // recovery: we know that a Bool is desired
        leq->set_type(Bool);
    }

    void VisitLt(lt_class *lt) override {
        lt->GetLeftOp()->Accept(this);
        lt->GetRightOp()->Accept(this);
        const Symbol left_op_type = lt->GetLeftOp()->get_type();
        const Symbol right_op_type = lt->GetRightOp()->get_type();
        if (left_op_type != Int || right_op_type != Int) {
            table_->semant_error(curr_clss_->get_filename(), lt)
                << "non-Int arguments: " << left_op_type << " < "
                << right_op_type << "\n";
        }
        // recovery: we know that a Bool is desired
        lt->set_type(Bool);
    }

    void VisitEq(eq_class *eq) override {
        eq->GetLhsExpr()->Accept(this);
        eq->GetRhsExpr()->Accept(this);
        // Any types may be freely compared except Int, String and Bool, which
        // may only be compared with objects of the same type.
        Symbol lhs_type = eq->GetLhsExpr()->get_type();
        Symbol rhs_type = eq->GetRhsExpr()->get_type();
        if ((lhs_type == Int || lhs_type == Str || lhs_type == Bool)
            && lhs_type != rhs_type) {
            table_->semant_error(curr_clss_->get_filename(), eq)
                << "Illegal comparison with a basic type.\n";
        }
        // recovery: we know that a Bool is desired
        eq->set_type(Bool);
    }

    void VisitNew(new__class *new_) override {
        if (!table_->HasClass(ResolveSelfType_(new_->GetName()))) {
            table_->semant_error(curr_clss_->get_filename(), new_)
                << "'new' used with undefined class " << new_->GetName()
                << ".\n";
            // recovery: simply allow cascading errors
            new_->set_type(Object);
        } else {
            new_->set_type(new_->GetName());
        }
    }

    void VisitInt(int_const_class *int_const) override {
        int_const->set_type(Int);
    }

    void VisitBool(bool_const_class *bool_const) override {
        bool_const->set_type(Bool);
    }

    void VisitString(string_const_class *string_const) override {
        string_const->set_type(Str);
    }

    void VisitIsvoid(isvoid_class *isvoid) override {
        isvoid->GetExpr()->Accept(this);
        // recovery: we know that a Bool is desired
        isvoid->set_type(Bool);
    }

    void VisitObject(object_class *object) override {
        Symbol *object_type = obj_env.lookup(object->GetName());
        if (!object_type) {
            table_->semant_error(curr_clss_->get_filename(), object)
                << "Undeclared identifier " << object->GetName() << ".\n";
            // recovery: simply allow cascading errors
            object->set_type(Object);
        } else {
            object->set_type(*object_type);
        }
    }

    void VisitNoExpr(no_expr_class *no_expr) override {
        no_expr->set_type(No_type);
    }

   private:
    ClassTable *table_;

    /// @brief Scoping.
    SymbolTable<Symbol /* name */, Symbol /* type */> obj_env;

    /// @brief For error message and SELF_TYPE resolution
    Class_ curr_clss_ = nullptr;

    /// @return The name of the current class if type is SELF_TYPE, as-is
    /// otherwise.
    Symbol ResolveSelfType_(Symbol type) const {
        if (type == SELF_TYPE) {
            return curr_clss_->GetName();
        }
        return type;
    }

    /// @return true if t_prime conforms to t.
    /// @note Undefined types are resolved as Object.
    bool Conform_(Symbol t_prime, Symbol t) const {
        // No_type is a sub-type of any type
        if (t_prime == No_type) {
            return true;
        }
        if (!table_->HasClass(t_prime) && t_prime != SELF_TYPE) {
            t_prime = Object;
        }
        if (!table_->HasClass(t) && t != SELF_TYPE) {
            t = Object;
        }
        if (t_prime == t) {
            return true;
        }
        if (t == SELF_TYPE) {
            // (1) t_prime is current class
            //      SELF_TYPE <= current class, possible downcast
            // (2) t_prime is other type
            //      (can't be SELF_TYPE since it's already checked)
            return false;
        }
        if (t_prime == SELF_TYPE) {
            t_prime = ResolveSelfType_(t_prime);
            if (t_prime == t) {
                return true;
            }
        }

        // t_prime should have t as one of its parents
        for (auto const parent : table_->GetParents(table_->at(t_prime))) {
            if (parent->GetName() == t) {
                return true;
            }
        }
        return false;
    }

    /// @return The lowest common ancestor of types.
    Symbol JoinType_(const std::unordered_set<Symbol> &types) const {
        // repeatedly join with the next type and update the ancestor
        Symbol ancestor = *types.begin();
        auto type_itr = ++types.begin();
        while (type_itr != types.cend()) {
            ancestor = JoinType_(ancestor, *type_itr);
            ++type_itr;
        }
        return ancestor;
    }

    /// @return The lowest common ancestor of t1 and t2.
    Symbol JoinType_(Symbol t1, Symbol t2) const {
        // Conformance check handles t1 == t2 or one is the parent of another.
        if (Conform_(t1, t2)) {
            return t2;
        }
        if (Conform_(t2, t1)) {
            return t1;
        }
        t1 = ResolveSelfType_(t1);
        t2 = ResolveSelfType_(t2);
        // Find after which class their parents diverged.
        const std::vector<Class_> p1 = table_->GetParents(table_->at(t1));
        const std::vector<Class_> p2 = table_->GetParents(table_->at(t2));
        Symbol ancestor = Object;
        for (auto itr1 = p1.rbegin(), itr2 = p2.rbegin();
             itr1 != p1.crend() && itr2 != p2.crend(); ++itr1, ++itr2) {
            if ((*itr1)->GetName() != (*itr2)->GetName()) {
                break;
            }
            ancestor = (*itr1)->GetName();
        }
        return ancestor;
    }

    /*
     * We can't use polymorphism here because arithmetic classes doesn't
     * share another base class between Expression_class.
     * A workaround is to use template methods.
     */

    template <typename Arithmetic>
    void CheckArithmeticHasIntArgs_(Arithmetic *arith) {
        const char operator_ = GetArithmeticOperator_(arith);
        arith->GetLeftOp()->Accept(this);
        arith->GetRightOp()->Accept(this);
        const Symbol left_op_type = arith->GetLeftOp()->get_type();
        const Symbol right_op_type = arith->GetRightOp()->get_type();
        if (left_op_type != Int || right_op_type != Int) {
            table_->semant_error(curr_clss_->get_filename(), arith)
                << "non-Int arguments: " << left_op_type << ' ' << operator_
                << ' ' << right_op_type << '\n';
        }
        // recovery: we know that an Int is desired
        arith->set_type(Int);
    }

    template <typename Arithmetic>
    char GetArithmeticOperator_(Arithmetic *arith) const {
        if (std::is_same<Arithmetic, plus_class>::value) {
            return '+';
        }
        if (std::is_same<Arithmetic, sub_class>::value) {
            return '-';
        }
        if (std::is_same<Arithmetic, mul_class>::value) {
            return '*';
        }
        if (std::is_same<Arithmetic, divide_class>::value) {
            return '/';
        }
        assert(false);
    }

    /*
     * Begin extracted functions for dispatch / static dispatch
     */

    void CheckActuals_(const Expressions actuals) {
        for (int i = actuals->first(); actuals->more(i); i = actuals->next(i)) {
            actuals->nth(i)->Accept(this);
        }
    }

    template <typename Dispatch>
    void ConformActualsToFormals_(Dispatch *dispatch, const Method method) {
        const Formals formals = method->GetFormals();
        const Expressions actuals = dispatch->GetActuals();
        for (int i = formals->first(), j = actuals->first();
             formals->more(i) && actuals->more(j);
             i = formals->next(i), j = actuals->next(j)) {
            const Symbol formal_type = formals->nth(i)->GetDeclType();
            const Symbol actual_type = actuals->nth(j)->get_type();
            if (!Conform_(actual_type, formal_type)) {
                table_->semant_error(curr_clss_->get_filename(), dispatch)
                    << "In call of method " << method->GetName() << ", type "
                    << actual_type << " of parameter "
                    << formals->nth(i)->GetName()
                    << " does not conform to declared type " << formal_type
                    << ".\n";
            }
        }
    }

    template <typename Dispatch>
    void ShowUndefinedMethodError_(Dispatch *dispatch) {
        table_->semant_error(curr_clss_->get_filename(), dispatch)
            << "Dispatch to undefined method " << dispatch->GetName() << ".\n";
    }

    void ShowWrongNumberOfArgumentsError_(dispatch_class *dispatch) {
        table_->semant_error(curr_clss_->get_filename(), dispatch)
            << "Method " << dispatch->GetName()
            << " called with wrong number of arguments.\n";
    }

    void ShowWrongNumberOfArgumentsError_(
        static_dispatch_class *static_dispatch) {
        // NOTE: surprisingly, the message if slightly different from dispatch
        table_->semant_error(curr_clss_->get_filename(), static_dispatch)
            << "Method " << static_dispatch->GetName()
            << " invoked with wrong number of arguments.\n";
    }

    void ShowUndefinedDispatchError_(static_dispatch_class *static_dispatch) {
        table_->semant_error(curr_clss_->get_filename(), static_dispatch)
            << "Static dispatch to undefined class "
            << static_dispatch->GetTypeName() << ".\n";
    }

    void ShowExprTypeNotConformError_(const Symbol expr_type,
                                      static_dispatch_class *static_dispatch) {
        table_->semant_error(curr_clss_->get_filename(), static_dispatch)
            << "Expression type " << expr_type
            << " does not conform to declared static dispatch type "
            << static_dispatch->GetTypeName() << ".\n";
    }

    /// @param class_name The class which the method belongs to.
    /// @param method_name
    /// @return The method defined in the class or inherited from parent, or
    /// nullptr is not found.
    Method GetMethod_(Symbol class_name, Symbol method_name) {
        class_name = ResolveSelfType_(class_name);
        /*
         * Look up in the the class, if not found, look up in the parents.
         */
        if (const auto class_methods = method_table.at(class_name);
            class_methods.count(method_name) != 0) {
            return class_methods.at(method_name);
        }
        for (const Class_ parent : table_->GetParents(table_->at(class_name))) {
            if (const auto parent_methods = method_table.at(parent->GetName());
                parent_methods.count(method_name) != 0) {
                return parent_methods.at(method_name);
            }
        }
        return nullptr;
    }

    /*
     * End extracted functions for dispatch / static dispatch
     */

    /*
     * Begin extracted function for method
     */

    void CheckNoFormalNamedSelf_(const Method method) {
        const Formals formals = method->GetFormals();
        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
            const Formal formal = formals->nth(i);
            if (formal->GetName()->equal_string("self", 4)) {
                table_->semant_error(curr_clss_->get_filename(), formal)
                    << "'self' cannot be the name of a formal parameter.\n";
            }
        }
    }

    void CheckNoUndefinedFormalType_(const Method method) {
        const Formals formals = method->GetFormals();
        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
            const Formal formal = formals->nth(i);
            if (formal->GetDeclType() == SELF_TYPE) {
                table_->semant_error(curr_clss_->get_filename(), formal)
                    << "Formal parameter formal cannot have type SELF_TYPE.\n";
            } else if (!table_->HasClass(formal->GetDeclType())) {
                table_->semant_error(curr_clss_->get_filename(), formal)
                    << "Class " << formal->GetDeclType()
                    << " of formal parameter " << formal->GetName()
                    << " is undefined.\n";
            }
        }
    }

    void CheckNoRedefinedFormal_(const Method method) {
        const Formals formals = method->GetFormals();
        std::unordered_set<Symbol> defined_formals{};
        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
            const Formal formal = formals->nth(i);
            if (defined_formals.find(formal->GetName())
                != defined_formals.cend()) {
                table_->semant_error(curr_clss_->get_filename(), formal)
                    << "Formal parameter " << formal->GetName()
                    << " is multiply defined.\n";
            }
            defined_formals.insert(formal->GetName());
        }
    }

    void CheckNoUndefinedReturnType_(const Method method) {
        const Symbol return_type = method->GetReturnType();
        if (!table_->HasClass(return_type) && return_type != SELF_TYPE) {
            table_->semant_error(curr_clss_->get_filename(), method)
                << "Undefined return type " << return_type << " in method "
                << method->GetName() << ".\n";
        }
    }

    /*
     * End extracted functions for method
     */
};

std::vector<Class_> ClassTable::GetParents(const Class_ clss) const {
    std::vector<Class_> parents;
    Symbol parent = clss->GetParentName();
    while (parent != No_class) {  // emphasize the condition, while instead for
        parents.push_back(at(parent));
        parent = at(parent)->GetParentName();
    }
    return parents;
}

std::vector<Class_> ClassTable::GetUserDefinedClasses() const {
    std::vector<Class_> classes{};
    for (auto [name, clss] : *this) {
        if (!IsBasic(name)) {
            classes.push_back(clss);
        }
    }
    return classes;
}

bool IsMethod(const Feature f) {
    return dynamic_cast<Method>(f);
}

static void CompilationHaltedWithErrors() {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant() {
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */
    classtable->CheckClasses();
    if (classtable->errors()) {
        CompilationHaltedWithErrors();
    }

    classtable->CheckFeatures();
    classtable->CheckHasMainClassAndMainMethod();

    TypeCheckVisitor visitor{classtable};
    this->Accept(&visitor);
    if (classtable->errors()) {
        CompilationHaltedWithErrors();
    }

    delete classtable;
}
