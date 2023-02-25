

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
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



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr)
{
    /* first pass: get name of the declared classes */
    install_basic_classes();
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        class__class *curr_class = dynamic_cast<class__class *>(classes->nth(i));
        add_class(curr_class->get_name(), curr_class->get_parent());
    }

    /* second pass: check no inheriting on undeclared class */
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        class__class *curr_class = dynamic_cast<class__class *>(classes->nth(i));
        assert(curr_class);
        if (!has_class(curr_class->get_parent()))
        {
            semant_error(curr_class) << "Class " << curr_class->get_name()
                                     << " inherits from an undefined class "
                                     << curr_class->get_parent() << ".\n";
        }
    }
}

void ClassTable::add_class(Symbol name, Symbol parent)
{
    if (!graph.has_edge(name, parent))
    {
        graph.add_edge(name, parent);
    }
}

int ClassTable::has_class(Symbol name) const
{
    return graph.has_vertex(name);
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

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object,
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);
    add_class(No_class);
    add_class(Object, No_class);

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
    add_class(IO, Object);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
	class_(Int,
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

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

ostream& ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(),c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}

int equal_symbol(Symbol a, Symbol b)
{
    return a->equal_string(b->get_string(), b->get_len());
}

int InheritanceGraph::find_vertex_pos(Symbol vertex) const
{
    int pos = 0;
    for (List<Symbol> *l = vertices; l && l->hd(); l = l->tl(), pos++)
    {
        if (equal_symbol(*l->hd(), vertex))
        {
            break;
        }
    }
    return pos;
}

List<Symbol> **InheritanceGraph::find_edges(int src_pos) const
{
    assert(src_pos < num_of_vertices);
    List<List<Symbol> *> *l = adjacency_lists;
    for (int pos = 0; pos < src_pos; l = l->tl(), pos++)
    {
        ;
    }
    return l->hd();
}

List<Symbol> **InheritanceGraph::find_edges(Symbol vertex) const
{
    int pos = find_vertex_pos(vertex);
    return find_edges(pos);
}

void InheritanceGraph::add_edge(Symbol src, Symbol dest)
{
    if (!has_vertex(src))
    {
        add_vertex(src);
    }
    List<Symbol> **adjacency_list_of_src = find_edges(src);
    *adjacency_list_of_src =
            new List<Symbol>(new Symbol(dest), *adjacency_list_of_src);
}

int InheritanceGraph::has_edge(Symbol src, Symbol dest) const
{
    if (!has_vertex(src))
    {
        return FALSE;
    }

    for (List<Symbol> *edges = *find_edges(src);
            edges /* NULL if not having any edges */ && edges->hd();
            edges = edges->tl())
    {
        if (equal_symbol(*edges->hd(), dest))
        {
            return TRUE;
        }
    }
    return FALSE;
}

void InheritanceGraph::add_vertex(Symbol vertex)
{
    vertices = new List<Symbol>(new Symbol(vertex), vertices);
    num_of_vertices++;

    List<Symbol> *empty_adjacency_list = new List<Symbol>(NULL);
    adjacency_lists = new List<List<Symbol> *>(
        new List<Symbol> *(
            empty_adjacency_list),  // takes yet one more layer of pointer
        adjacency_lists);
}

int InheritanceGraph::has_vertex(Symbol vertex) const
{
    for (List<Symbol> *l = vertices; l && l->hd(); l = l->tl())
    {
        if (equal_symbol(*l->hd(), vertex))
        {
            return TRUE;
        }
    }
    return FALSE;
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
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}
