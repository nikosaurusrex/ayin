#ifndef COPIER_H
#define COPIER_H

#include "ast.h"
#include "common.h"

struct Compiler;

/*
* The copier is used to copy AST Nodes
* this is needed for polymorphing template functions
*/
struct Copier {
    Compiler *compiler;
    Scope *current_scope;

    Copier(Compiler *compiler);

   	AFunction *copy_function(AFunction *old); 
    Expression *copy(Expression *ast);
    TypeInfo *copy_type(TypeInfo *old);

	Scope *push_scope(Scope *old);
	void pop_scope();
};

#endif
