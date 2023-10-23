 #include "compiler.h"
#include "copier.h"
#include "ast.h"

#define COPY_NEW(t) new t(); \
					_new->location = old->location; \
					_new->type_info = copy_type(old->type_info)

#define COPY_F(name) (_new->name = old->name)
#define COPY_C(name) (_new->name = (decltype(_new->name))copy(old->name))
#define COPY_ARRAY_TYPE(name) for (auto it : old->name) { _new->name.add((decltype(it))copy_type(it)); }
#define COPY_ARRAY(name) for (auto it : old->name) { _new->name.add((decltype(it))copy(it)); }
#define COPY_TYPE(name) (_new->name = copy_type(old->name))

Copier::Copier(Compiler *compiler) {
	this->compiler = compiler;
	this->current_scope = compiler->global_scope;
}

Expression *Copier::copy(Expression *ast) {
	if (!ast) return 0;

	switch (ast->type) {
		case AST_SCOPE: {
			auto old = static_cast<Scope *>(ast);
			auto _new = push_scope(old);

			pop_scope();

			return _new;
		}
		case AST_DECLARATION: {
			auto old = static_cast<Declaration *>(ast);
			for (auto edecl_expr : current_scope->declarations) {
				Declaration *edecl = (Declaration *) edecl_expr;
				if (edecl->type != AST_DECLARATION) {
					continue;
				}

				if (old->identifier->atom == edecl->identifier->atom) {
					return edecl;
				}
			}

			auto _new = COPY_NEW(Declaration);

			COPY_C(identifier);
			COPY_C(initializer);
			COPY_F(flags);

			return _new;
		}
		case AST_FUNCTION: {
			auto old = static_cast<AFunction *>(ast);

			return copy_function(old);
		}
		case AST_STRUCT: {
			auto old = static_cast<Struct *>(ast);
			auto _new = COPY_NEW(Struct);

			COPY_C(identifier);
			COPY_ARRAY(members);

			return _new;
		}
		case AST_ENUM: {
			auto old = static_cast<Enum *>(ast);
			auto _new = COPY_NEW(Enum);

			COPY_C(identifier);

			return _new;
		}
		case AST_TYPE_ALIAS: {
			auto old = static_cast<TypeAlias *>(ast);
			auto _new = COPY_NEW(TypeAlias);

			COPY_C(identifier);

			return _new;
		}
		case AST_IDENTIFIER: {
			auto old = static_cast<Identifier *>(ast);
			auto _new = COPY_NEW(Identifier);

			COPY_F(atom);
			_new->scope = current_scope;

			return _new;
		}
		case AST_LITERAL: {
			auto old = static_cast<Literal *>(ast);
			auto _new = COPY_NEW(Literal);

			COPY_F(literal_type);
			COPY_F(int_value);
			COPY_F(float_value);
			COPY_F(string_value);

			return _new;
		}
		case AST_CAST: {
			auto old = static_cast<Cast *>(ast);
			auto _new = COPY_NEW(Cast);

			COPY_C(expression);
			COPY_TYPE(target_type);

			return _new;
		}
		case AST_RETURN: {
			auto old = static_cast<Return *>(ast);
			auto _new = COPY_NEW(Return);

			COPY_C(return_value);

			return _new;
		}
		case AST_CALL: {
			auto old = static_cast<Call *>(ast);
			auto _new = COPY_NEW(Call);

			COPY_F(by_function_pointer);
			COPY_C(identifier);
			COPY_ARRAY(arguments);

			Expression *decl = find_declaration_by_id(_new->identifier);

			if (!decl) {
				compiler->report_error(_new->identifier, "Symbol not defined");
				return _new;
			}

			if (decl->type != AST_FUNCTION) {
				compiler->report_error(_new->identifier, "Symbol is not a function");
				return _new;
			}

			AFunction *function = static_cast<AFunction *>(decl);
			_new->resolved_function = function;

			return _new;
		}
		case AST_BINARY: {
			auto old = static_cast<Binary *>(ast);
			auto _new = COPY_NEW(Binary);

			COPY_F(op);
			COPY_C(lhs);
			COPY_C(rhs);

			return _new;
		}
		case AST_UNARY: {
			auto old = static_cast<Unary *>(ast);
			auto _new = COPY_NEW(Unary);

			COPY_F(op);
			COPY_C(target);
			COPY_F(is_pre);

			return _new;
		}
		case AST_SIZEOF: {
			auto old = static_cast<Sizeof *>(ast);
			auto _new = COPY_NEW(Sizeof);

			COPY_TYPE(target_type);

			return _new;
		}
		case AST_IF: {
			auto old = static_cast<If *>(ast);
			auto _new = COPY_NEW(If);

			COPY_C(condition);
			COPY_C(then_statement);
			COPY_C(else_statement);

			return _new;
		}
		case AST_WHILE: {
			auto old = static_cast<While *>(ast);
			auto _new = COPY_NEW(While);

			COPY_C(condition);
			COPY_C(statement);

			return _new;
		}
		case AST_INDEX: {
			auto old = static_cast<Index *>(ast);
			auto _new = COPY_NEW(Index);

			COPY_C(expression);
			COPY_C(index);

			return _new;
		}
		case AST_MEMBER: {
			auto old = static_cast<Member *>(ast);
			auto _new = COPY_NEW(Member);

			COPY_C(left);
			COPY_C(field);
			COPY_F(field_index);

			return _new;
		}
		case AST_CONTINUE: {
			auto old = static_cast<Continue *>(ast);
			auto _new = COPY_NEW(Continue);

			return _new;
		}
		case AST_BREAK: {
			auto old = static_cast<Break *>(ast);
			auto _new = COPY_NEW(Break);

			return _new;
		}
		case AST_DIRECTIVE: {
			auto old = static_cast<Directive *>(ast);
			auto _new = COPY_NEW(Directive);

			COPY_F(directive_type);
			COPY_F(file);

			return _new;
		}
		case AST_FOR: {
			auto old = static_cast<For *>(ast);
			auto _new = COPY_NEW(For);

			COPY_C(iterator_expr);
			COPY_C(upper_range_expr);
			COPY_C(index_declaration);
			COPY_C(value_declaration);
			COPY_C(body);

			return _new;
		}
		case AST_DEFER: {
			auto old = static_cast<ADefer *>(ast);
			auto _new = COPY_NEW(ADefer);

			COPY_C(target);

			return _new;
		}
	}

	assert(0);
	return 0;
}

AFunction *Copier::copy_function(AFunction *old) {
	Scope *old_current_scope = current_scope;
	current_scope = compiler->global_scope;

	AFunction *_new = COPY_NEW(AFunction);

	COPY_C(identifier);

	if (old->template_scope)
		_new->template_scope = push_scope(old->template_scope);

	if (old->parameter_scope)
		_new->parameter_scope = push_scope(old->parameter_scope);

	COPY_TYPE(return_type);
	_new->block_scope = push_scope(old->block_scope);

	COPY_F(linkage_name);
	COPY_F(flags);

	/* TODO: check if i have to copy polymorphed_overloads */

	if (old->template_scope)
		pop_scope();

	if (old->parameter_scope)
		pop_scope();

	pop_scope();

	current_scope = old_current_scope;

	return _new;
}

TypeInfo *Copier::copy_type(TypeInfo *old) {
	if (!old) return 0;

	TypeInfo *_new = new TypeInfo();

	COPY_F(type);
	COPY_TYPE(element_type);
	COPY_C(unresolved_name);
	COPY_F(array_size);
	COPY_F(struct_decl);
	COPY_ARRAY_TYPE(struct_members);
	COPY_ARRAY_TYPE(parameters);
	COPY_TYPE(return_type);
	COPY_F(is_signed);
	COPY_F(is_dynamic);
	COPY_F(auto_cast);
	COPY_F(size);
	
	/* @Todo: copy enum type */

	return _new;
}

Scope *Copier::push_scope(Scope *old) {
	Scope *_new = new Scope();
	_new->parent = current_scope;
	current_scope = _new;

	for (auto decl : old->declarations) {
		_new->declarations.add(copy(decl));
	}

	for (auto expr : old->statements) {
		_new->statements.add(copy(expr));
	}
	
	for (auto expr : old->defers) {
		_new->defers.add((ADefer *) copy(expr));
	}

	return _new;
}

void Copier::pop_scope() {
	current_scope = current_scope->parent;
}
