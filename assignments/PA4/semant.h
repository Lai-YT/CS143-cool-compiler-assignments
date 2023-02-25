#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// Returns whether the string representation of the symbols are equal.
int equal_symbol(Symbol a, Symbol b);

// Build with adjacency list.
class InheritanceGraph {
private:
  // Though vertices and and adjacency_lists are linked lists, they are mapped
  // through relative positions (index).
  List<Symbol> *vertices = NULL;
  List<List<Symbol> *> *adjacency_lists = NULL;
  int num_of_vertices = 0;

  // Equals to "num_of_vertices" if the vertex doesn't exist.
  int find_vertex_pos(Symbol vertex) const;
  List<Symbol> **find_edges(int src_pos) const;
  List<Symbol> **find_edges(Symbol vertex) const;

public:
  // Adds a new edge from src to dest.
  // If src isn't already in the graph, it is added to the graph.
  // The caller is responsible for duplicating.
  void add_edge(Symbol src, Symbol dest);
  int has_edge(Symbol src, Symbol dest) const;

  // Adds a new vertex.
  // The caller is responsible for duplicating.
  void add_vertex(Symbol vertex);
  int has_vertex(Symbol vertex) const;
};

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  InheritanceGraph graph;

public:
  ClassTable(Classes);
  void add_class(Symbol name);
  void add_class(Symbol name, Symbol parent);
  int has_class(Symbol name) const;
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif
