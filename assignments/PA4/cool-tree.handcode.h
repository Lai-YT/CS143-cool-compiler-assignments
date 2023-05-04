//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

class Visitor;

#define Program_EXTRAS                  \
    virtual void semant() = 0;          \
    virtual void Accept(Visitor *) = 0; \
    virtual void dump_with_types(ostream &, int) = 0;

#define program_EXTRAS                              \
    void semant();                                  \
    Classes GetClasses() const { return classes; }; \
    void Accept(Visitor *) override;                \
    void dump_with_types(ostream &, int);

#define Class__EXTRAS                         \
    virtual Symbol get_filename() = 0;        \
    virtual Symbol GetParentName() const = 0; \
    virtual Symbol GetName() const = 0;       \
    virtual Features GetFeatures() const = 0; \
    virtual void Accept(Visitor *) = 0;       \
    virtual void dump_with_types(ostream &, int) = 0;

#define class__EXTRAS                                 \
    Symbol get_filename() { return filename; }        \
    Symbol GetParentName() const { return parent; }   \
    Symbol GetName() const { return name; }           \
    Features GetFeatures() const { return features; } \
    void Accept(Visitor *) override;                  \
    void dump_with_types(ostream &, int);

#define Feature_EXTRAS                  \
    virtual Symbol GetName() const = 0; \
    virtual void Accept(Visitor *) = 0; \
    virtual void dump_with_types(ostream &, int) = 0;

#define Feature_SHARED_EXTRAS               \
    Symbol GetName() const { return name; } \
    void Accept(Visitor *) override;        \
    void dump_with_types(ostream &, int);

#define method_EXTRAS                                    \
    Symbol GetReturnType() const { return return_type; } \
		Expression GetExpr() const { return expr; }          \
    Formals GetFormals() const { return formals; }

#define attr_EXTRAS \
    Symbol GetDeclType() const { return type_decl; } \
		Expression GetInit() const { return init; }

#define Formal_EXTRAS                       \
    virtual Symbol GetName() const = 0;     \
    virtual Symbol GetDeclType() const = 0; \
    virtual void dump_with_types(ostream &, int) = 0;

#define formal_EXTRAS                                \
    Symbol GetName() const { return name; }          \
    Symbol GetDeclType() const { return type_decl; } \
    void dump_with_types(ostream &, int);

#define assign_EXTRAS                       \
    Symbol GetName() const { return name; } \
    Expression GetExpr() const { return expr; }

#define typcase_EXTRAS                          \
    Expression GetExpr() const { return expr; } \
    Cases GetCases() const { return cases; };

#define Case_EXTRAS                         \
    virtual void Accept(Visitor *) = 0;     \
    virtual Symbol GetName() const = 0;     \
    virtual Symbol GetDeclType() const = 0; \
    virtual Expression GetExpr() const = 0; \
    virtual void dump_with_types(ostream &, int) = 0;

#define branch_EXTRAS                                \
    void Accept(Visitor *) override;                 \
    Symbol GetName() const { return name; }          \
    Symbol GetDeclType() const { return type_decl; } \
    Expression GetExpr() const { return expr; }      \
    void dump_with_types(ostream &, int);

#define dispatch_EXTRAS                         \
    Expression GetExpr() const { return expr; } \
    Symbol GetName() const { return name; }     \
    Expressions GetActuals() const { return actual; }

#define static_dispatch_EXTRAS                       \
    Expression GetExpr() const { return expr; }      \
    Symbol GetTypeName() const { return type_name; } \
    Symbol GetName() const { return name; }          \
    Expressions GetActuals() const { return actual; }

#define comp_EXTRAS \
    Expression GetExpr() const { return e1; };

#define Expression_EXTRAS                             \
    Symbol type;                                      \
    Symbol get_type() { return type; }                \
    Expression set_type(Symbol s) {                   \
        type = s;                                     \
        return this;                                  \
    }                                                 \
    Expression_class() { type = (Symbol)NULL; }       \
    void dump_type(ostream &, int);                   \
    virtual void dump_with_types(ostream &, int) = 0; \
    virtual void Accept(Visitor *) = 0;

#define Expression_SHARED_EXTRAS     \
    void Accept(Visitor *) override; \
    void dump_with_types(ostream &, int);

#define let_EXTRAS                                     \
    Symbol GetIdName() const { return identifier; }    \
    Symbol GetIdDeclType() const { return type_decl; } \
    Expression GetInit() const { return init; }        \
    Expression GetBody() const { return body; }

#define cond_EXTRAS                                     \
    Expression GetPredicate() const { return pred; }    \
    Expression GetThenExpr() const { return then_exp; } \
    Expression GetElseExpr() const { return else_exp; }

#define loop_EXTRAS                                  \
    Expression GetPredicate() const { return pred; } \
    Expression GetBody() const { return body; }

#define plus_EXTRAS                             \
    Expression GetLeftOp() const { return e1; } \
    Expression GetRightOp() const { return e2; }

#define sub_EXTRAS                              \
    Expression GetLeftOp() const { return e1; } \
    Expression GetRightOp() const { return e2; }

#define mul_EXTRAS                              \
    Expression GetLeftOp() const { return e1; } \
    Expression GetRightOp() const { return e2; }

#define divide_EXTRAS                           \
    Expression GetLeftOp() const { return e1; } \
    Expression GetRightOp() const { return e2; }

#define neg_EXTRAS \
    Expression GetExpr() const { return e1; }

#define leq_EXTRAS                              \
    Expression GetLeftOp() const { return e1; } \
    Expression GetRightOp() const { return e2; }

#define lt_EXTRAS                               \
    Expression GetLeftOp() const { return e1; } \
    Expression GetRightOp() const { return e2; }

#define eq_EXTRAS                                \
    Expression GetLhsExpr() const { return e1; } \
    Expression GetRhsExpr() const { return e2; }

#define new__EXTRAS \
    Symbol GetName() const { return type_name; }

#define block_EXTRAS \
    Expressions GetExpressions() const { return body; }

#define isvoid_EXTRAS \
    Expression GetExpr() const { return e1; }

#define object_EXTRAS \
    Symbol GetName() const { return name; }

#endif
