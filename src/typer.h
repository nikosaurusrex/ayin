#ifndef TYPER_H
#define TYPER_H

#include "ast.h"
#include "common.h"

struct Compiler;
struct Typer {
	Compiler *compiler;	

	AFunction *current_function = 0;
	Scope *current_loop_body;

	Typer(Compiler *compiler);

	void type_check_scope(Scope *scope);

	void type_check_statement(Expression *stmt);

	void type_check_variable_declaration(Declaration *decl);

	void resolve_function_type(AFunction *function);
	void type_check_function(AFunction *function);

	void infer_type(Expression *expression);
	TypeInfo *resolve_type_info(TypeInfo *type_info);
    void resolve_type_force(TypeInfo **type_info);

	bool compare_arguments(Identifier *call, Array<Expression *> *args, Array<TypeInfo *> *par_types, bool varags);

	Expression *check_expression_type_and_cast(Expression *expression, TypeInfo *other_type);

	Cast *make_cast(Expression *expression, TypeInfo *target_type);

	AFunction *get_polymorph_function(Call *call, AFunction *template_function);
	AFunction *make_polymorph_function(AFunction *template_function, Array<Expression *> *arguments);
	bool can_fill_polymorph(TypeInfo *par_type, TypeInfo *arg_type); 

	bool types_match(TypeInfo *t1, TypeInfo *t2);

	s32 get_pointer_level(TypeInfo *type_info);
	bool type_points_to_void(TypeInfo *type_info);

	String mangle_name(AFunction *decl);
	void mangle_type(StringBuilder *builder, TypeInfo *type);

	Expression *make_compare_zero(Expression *target);

	Expression *find_declaration_in_scope(Scope *scope, Identifier *id);

	Literal *make_integer_literal(s64 value, TypeInfo *type_info, Ast *source_loc=0);
	Identifier *make_identifier(Atom *name);
	Member *make_member(Expression *aggregate_expression, Atom *field);
	Index *make_index(Expression *array, Expression *index);
};

#endif
