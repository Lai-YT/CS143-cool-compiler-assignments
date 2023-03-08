#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
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

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr) {
    install_basic_classes();

    // first pass: collect classes
    InstallClasses(classes);

    // second pass(es): check inheritance
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
    CheckNoUndefinedReturnType();
    CheckRedefinedMethodMatchAncestor();
}

void ClassTable::InstallClasses(Classes classes) {
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        AddClass(classes->nth(i));
    }
}

void ClassTable::AddClass(Class_ c) {
    Symbol name = c->GetName();
    if (HasClass(name) || name == SELF_TYPE) {
        if (IsBasic(name)) {
            semant_error(c) << "Redefinition of basic class " << name << ".\n";
        } else {
            semant_error(c) << "Class "<< name << " was previously defined." << '\n';
        }
        return;
    }
    emplace(name, c);
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

void ClassTable::CheckNoInheritanceFromFinal() {
    for (auto [name, clss] : *this) {
        Symbol pname = clss->GetParentName();
        if (IsFinal(pname)) {
            semant_error(clss) << "Class " << name << " cannot inherit class "
                               << pname << ".\n";
        }
    }
}

void ClassTable::CheckNoUndeclaredBaseClass() {
    for (auto [name, clss] : *this) {
        Symbol pname = clss->GetParentName();
        if (!HasClass(pname) && pname != No_class && pname != SELF_TYPE) {
            semant_error(clss)
                << "Class " << name << " inherits from an undefined class "
                << pname << ".\n";
        }
    }
}

void ClassTable::CheckNoCircularInheritance() {
    for (auto [name, clss] : *this) {
        Symbol pname = clss->GetParentName();
        while (pname != No_class) {
            if (pname == name) {
                semant_error(clss)
                    << "Class " << name << ", or an ancestor of " << name
                    << ", is involved in an inheritance cycle." << '\n';
                break;
            }
            assert(HasClass(pname));
            pname = at(pname)->GetParentName();
        }
    }
}


void ClassTable::CheckHasMainClass() {
    if (!HasClass(Main)) {
        semant_error() << "Class " << Main << " is not defined." << '\n';
    }
}

void ClassTable::CheckHasMainMethod() {
    assert(HasClass(Main));

    Class_ main = at(Main);
    Features features = main->GetFeatures();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature f = features->nth(i);
        if (IsMethod(f) && f->GetName()->equal_string("main", 4)) {
            return;
        }
    }
    semant_error(main) << "No 'main' method in class " << Main << ".\n";
}

void ClassTable::CheckNoUndefinedReturnType() {
    for (const auto [_, clss] : *this) {
        const Features fs = clss->GetFeatures();
        for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
            const Feature feature = fs->nth(i);
            if (!IsMethod(feature)) {
                continue;
            }
            const method_class *method = dynamic_cast<method_class *>(feature);
            const Symbol return_type = method->GetReturnType();
            if (!HasClass(return_type) && return_type != SELF_TYPE) {
                semant_error(clss->get_filename(), feature)
                    << "Undefined return type " << return_type << " in method "
                    << method->GetName() << ".\n";
            }
        }
    }
}

void ClassTable::CheckRedefinedMethodMatchAncestor() {
    for (const auto [_, clss] : *this) {
        for (const Method method : GetMethods(clss)) {
            const Formals formals = method->GetFormals();
            bool has_found_ancestor_method = false;
            for (Symbol parent = clss->GetParentName();
                 parent != No_class && !has_found_ancestor_method;
                 parent = at(parent)->GetParentName()) {
                for (const Method pmethod : GetMethods(at(parent))) {
                    if (pmethod->GetName() != method->GetName()) {
                        continue;
                    }
                    has_found_ancestor_method = true;
                    const Formals pformals = pmethod->GetFormals();
                    if (formals->len() != pformals->len()) {
                        semant_error(clss->get_filename(), method)
                            << "Incompatible number of formal parameters in "
                               "redefined method "
                            << method->GetName() << ".\n";
                    }
                    for (int i = formals->first(), j = pformals->first();
                         formals->more(i) && pformals->more(j);
                         i = formals->next(i), j = pformals->next(j)) {
                        const Symbol formal_type =
                            formals->nth(i)->GetDeclType();
                        const Symbol pformal_type =
                            pformals->nth(j)->GetDeclType();
                        if (formal_type != pformal_type) {
                            semant_error(clss->get_filename(), method)
                                << "In redefined method " << method->GetName()
                                << ", parameter type " << formal_type
                                << " is different from original type "
                                << pformal_type << '\n';
                        }
                    }
                    if (method->GetReturnType() != pmethod->GetReturnType()) {
                        semant_error(clss->get_filename(), method)
                            << "In redefined method " << method->GetName()
                            << ", return type " << method->GetReturnType()
                            << " is different from original return type "
                            << pmethod->GetReturnType() << ".\n";
                    }
                }
            }
        }
    }
}

std::vector<Method> ClassTable::GetMethods(Class_ clss) {
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

bool IsMethod(Feature f) {
    return dynamic_cast<method_class *>(f);
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

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}
