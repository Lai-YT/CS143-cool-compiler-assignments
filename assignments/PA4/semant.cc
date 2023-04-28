#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <symtab.h>
#include <unordered_set>
#include <vector>
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

ClassTable::ClassTable(Classes classes)
    : semant_errors(0), error_stream(cerr), classes(classes) {
    install_basic_classes();

    // first pass: collect classes
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
    CheckHasMainClass();
    if (semant_errors) {
        return;
    }
    CheckHasMainMethod();
}

void ClassTable::CheckMethods() {
    MethodRedefinitionChecker{this}.CheckNoMismatchRedefinedMethod();
    for (int i = 0; classes->more(i); i = classes->next(i)) {
        const Class_ clss = classes->nth(i);
        for (const Method method : GetMethods(clss)) {
            CheckNoFormalNamedSelf(method, clss->get_filename());
            CheckNoUndefinedFormalType(method, clss->get_filename());
            CheckNoRedefinedFormal(method, clss->get_filename());
            CheckNoUndefinedReturnType(method, clss->get_filename());
        }
    }
}

void ClassTable::CheckNoFormalNamedSelf(const Method method, const Symbol filename) {
    const Formals formals = method->GetFormals();
    for (int i = 0; formals->more(i); i = formals->next(i)) {
        const Formal formal = formals->nth(i);
        if (formal->GetName()->equal_string("self", 4)) {
            semant_error(filename, method)
                << "'self' cannot be the name of a formal parameter.\n";
        }
    }
}

void ClassTable::CheckNoUndefinedFormalType(const Method method, const Symbol filename) {
    const Formals formals = method->GetFormals();
    for (int i = 0; formals->more(i); i = formals->next(i)) {
        const Formal formal = formals->nth(i);
        if (!HasClass(formal->GetDeclType())) {
            semant_error(filename, method)
                << "Class " << formal->GetDeclType() << " of formal parameter "
                << formal->GetName() << " is undefined.\n";
        }
    }
}

void ClassTable::CheckNoRedefinedFormal(const Method method, const Symbol filename) {
    const Formals formals = method->GetFormals();
    std::unordered_set<Symbol> defined_formals{};
    for (int i = 0; formals->more(i); i = formals->next(i)) {
        const Formal formal = formals->nth(i);
        if (defined_formals.find(formal->GetName()) != defined_formals.cend()) {
            semant_error(filename, method)
                << "Formal parameter " << formal->GetName()
                << " is multiply defined.\n";
        } else {
            defined_formals.insert(formal->GetName());
        }
    }
}

void ClassTable::CheckNoUndefinedReturnType(const Method method, const Symbol filename) {
    const Symbol return_type = method->GetReturnType();
    if (!HasClass(return_type) && return_type != SELF_TYPE) {
        semant_error(filename, method)
            << "Undefined return type " << return_type << " in method "
            << method->GetName() << ".\n";
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
    for (auto iter = rbegin(); iter != rend(); iter++) {
        const auto clss = iter->second;
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
    for (auto iter = rbegin(); iter != rend(); iter++) {
        const auto clss = iter->second;
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
    for (auto iter = rbegin(); iter != rend(); iter++) {
        const auto [name, clss] = *iter;
        for (Symbol pname = clss->GetParentName(); pname != No_class;
             pname = at(pname)->GetParentName()) {
            if (pname == name) {
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

void ClassTable::CheckHasMainClass() {
    if (!HasClass(Main)) {
        semant_error() << "Class " << Main << " is not defined." << '\n';
    }
}

void ClassTable::CheckHasMainMethod() {
    const Class_ main = at(Main);
    for (const Method method : GetMethods(main)) {
        if (method->GetName()->equal_string("main", 4)) {
            return;
        }
    }
    semant_error(main) << "No 'main' method in class " << Main << ".\n";
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

void ClassTable::CheckNoUndeclaredIdentifier() {
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ clss = classes->nth(i);
        Symbol name = clss->GetName();
        auto features = clss->GetFeatures();
        SymbolTable<Symbol, Class_> environment;
        environment.enterscope();
        // add the attributes of the current class
        for (int j = features->first(); features->more(j);
             j = features->next(j)) {
            if (auto attr = dynamic_cast<attr_class *>(features->nth(j))) {
                if (environment.probe(attr->GetName())) {
                    semant_error(clss->get_filename(), attr)
                        << "Attribute " << attr->GetName()
                        << " is multiply defined in class.\n";
                } else {
                    environment.addid(attr->GetName(),
                                      &at(attr->GetDeclType()));
                }
            }
        }
        // check each methods
        for (const auto method : GetMethods(clss)) {
            environment.enterscope();
            Formals formals = method->GetFormals();
            for (int i = formals->first(); formals->more(i);
                 i = formals->next(i)) {
                auto formal = formals->nth(i);
                if (environment.probe(formal->GetName())) {
                    semant_error(clss->get_filename(), method)
                        << "Formal parameter " << formal->GetName()
                        << " is multiply defined.\n";
                } else {
                    environment.addid(formal->GetName(),
                                      &at(formal->GetDeclType()));
                }
            }
            environment.exitscope();
        }
        environment.exitscope();
    }
}

/*
 * Note that this function goes into infinite loop if contains circular
 * inheritance.
 */
std::vector<Class_> ClassTable::GetParents(const Class_ clss) const {
    std::vector<Class_> parents;
    Symbol parent = clss->GetParentName();
    while (parent != No_class) {  // emphasize the condition, while instead for
        parents.push_back(at(parent));
        parent = at(parent)->GetParentName();
    }
    return parents;
}

bool IsMethod(const Feature f) {
    return dynamic_cast<Method>(f);
}

MethodRedefinitionChecker::MethodRedefinitionChecker(ClassTable *table)
    : table_{table} {}

/**
 * @note Errors should be reported in ascending order by the line number.
 */
void MethodRedefinitionChecker::CheckNoMismatchRedefinedMethod() {
    for (const auto [_, clss] : *table_) {
        filename_ = clss->get_filename();
        for (const Method method : GetMethods(clss)) {
            method_ = method;
            bool has_found_ancestor_method = false;
            for (Symbol parent = clss->GetParentName();
                 !has_found_ancestor_method && parent != No_class;
                 parent = table_->at(parent)->GetParentName()) {
                for (const Method pmethod : GetMethods(table_->at(parent))) {
                    if (pmethod->GetName() == method->GetName()) {
                        pmethod_ = pmethod;
                        has_found_ancestor_method = true;
                        CheckNoMismatch_();
                        break;
                    }
                }
            }
        }
    }
}

/*
 * When facing multiple errors on a single method, only one of them is reported.
 * The precedence is (1) return type (2) number of formal (3) formal type".
 */
void MethodRedefinitionChecker::CheckNoMismatch_() {
    const int errors_before_check = table_->errors();
    CheckReturnType_();
    if (table_->errors() == errors_before_check) {
        CheckNumberOfFormals_();
    }
    if (table_->errors() == errors_before_check) {
        CheckFormalTypes_();
    }
}

void MethodRedefinitionChecker::CheckReturnType_() {
    if (method_->GetReturnType() != pmethod_->GetReturnType()) {
        table_->semant_error(filename_, method_)
            << "In redefined method " << method_->GetName() << ", return type "
            << method_->GetReturnType()
            << " is different from original return type "
            << pmethod_->GetReturnType() << ".\n";
    }
}

void MethodRedefinitionChecker::CheckNumberOfFormals_() {
    const Formals formals = method_->GetFormals();
    const Formals pformals = pmethod_->GetFormals();
    if (formals->len() != pformals->len()) {
        table_->semant_error(filename_, method_)
            << "Incompatible number of formal parameters in redefined method "
            << method_->GetName() << ".\n";
    }
}

void MethodRedefinitionChecker::CheckFormalTypes_() {
    const Formals formals = method_->GetFormals();
    const Formals pformals = pmethod_->GetFormals();
    for (int i = 0; formals->more(i) && pformals->more(i); i++) {
        const Symbol formal_type = formals->nth(i)->GetDeclType();
        const Symbol pformal_type = pformals->nth(i)->GetDeclType();
        if (formal_type != pformal_type) {
            table_->semant_error(filename_, method_)
                << "In redefined method " << method_->GetName()
                << ", parameter type " << formal_type
                << " is different from original type " << pformal_type << '\n';
        }
    }
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
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    classtable->CheckMethods();
    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}
