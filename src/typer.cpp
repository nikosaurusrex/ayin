#include "compiler.h"
#include "typer.h"
#include "ast.h"
#include "common.h"

static bool expr_is_targatable(Expression *expression);
static void copy_location_info(Ast *left, Ast *right);

Typer::Typer(Compiler *compiler) {
	this->compiler = compiler;
}

void Typer::type_check_scope(Scope *scope) {
	for (auto stmt : scope->statements) {
		type_check_statement(stmt);
	}

	for (int i = scope->defers.length - 1; i >= 0; --i) {
		type_check_statement(scope->defers[i]);
	}
}

void Typer::type_check_statement(Expression *stmt) {
	if (!stmt) return;

	switch (stmt->type) {
		case AST_DECLARATION: {
			auto var_decl = static_cast<Declaration *>(stmt);
			type_check_variable_declaration(var_decl);
		} break;
		case AST_TYPE_ALIAS: {
			break;
		}
		case AST_ENUM: {
			break;
		}
		case AST_FUNCTION: {
			auto fun = static_cast<AFunction *>(stmt);

			if ((fun->flags & FUNCTION_FOREIGN) && (fun->flags & FUNCTION_TEMPLATE)) {
				compiler->report_error(fun, "Function cannot be external and be a template function at the same time");
				break;
			}

			if ((fun->flags & FUNCTION_VARARG) && !(fun->flags & FUNCTION_FOREIGN)) {
				compiler->report_error(fun, "Only external functions can be marked vararg");
				break;
			}

			if (fun->flags & FUNCTION_TEMPLATE)
				break;

			type_check_function(fun);
		} break;
		case AST_STRUCT: {
			auto strct = static_cast<Struct *>(stmt);

			int i = 0;
			for (auto decl : strct->members) {
				type_check_variable_declaration(decl);
				strct->type_info->struct_members[i] = decl->type_info;
				i++;
			}
		} break;
		case AST_RETURN: {
			Return *ret = static_cast<Return *>(stmt);
			ret->function = current_function;

			if (ret->return_value) {
				infer_type(ret->return_value);
				ret->return_value = check_expression_type_and_cast(ret->return_value, current_function->return_type);

				if (!types_match(ret->return_value->type_info, current_function->return_type)) {
					String ret_val_ty_str = type_to_string(ret->return_value->type_info);
					String ret_ty_str = type_to_string(current_function->return_type);

					compiler->report_error(ret, "Type of return value (%.*s) and return type of function (%.*s) do not match", ret_val_ty_str.length, ret_val_ty_str.data, ret_ty_str.length, ret_ty_str.data);
				}

			} else if (current_function->return_type->type != TYPE_VOID_TYPE) {
				compiler->report_error(ret, "Tried to return no value from non-void function");
			}
			break;
		}
		case AST_DEFER: {
			Defer *defer = (Defer *) stmt;

			type_check_statement(defer->target);

			break;
		};
		case AST_SCOPE: {
			Scope *scope = static_cast<Scope *>(stmt);

			type_check_scope(scope);

			break;
		}
		case AST_IF: {
			If *_if = static_cast<If *>(stmt);

			infer_type(_if->condition);
			if (!is_bool(_if->condition->type_info)) {
				_if->condition = make_compare_zero(_if->condition);
			}

			type_check_statement(_if->then_statement);
			type_check_statement(_if->else_statement);

			break;
		}
		case AST_WHILE: {
			While *_while = static_cast<While *>(stmt);
			current_loop_body = _while->statement;

			infer_type(_while->condition);
			if (!is_bool(_while->condition->type_info)) {
				_while->condition = make_compare_zero(_while->condition);
			}

			type_check_statement(_while->statement);

			break;
		}
		case AST_FOR: {
			For *_for = static_cast<For *>(stmt);
			current_loop_body = _for->body;

			infer_type(_for->iterator_expr);

			if (_for->upper_range_expr) {
				infer_type(_for->upper_range_expr);

				auto init_type = _for->iterator_expr->type_info;
				auto upper_type = _for->upper_range_expr->type_info;

				if (!is_int(init_type) || !is_int(upper_type)) {
					compiler->report_error(_for, "'..' operator may only be surrouned by integer expression.\n");
					return;
				}

				if (_for->value_declaration) {
					compiler->report_error(_for->value_declaration, "Value iteration not available for range");
					return;
				}

				_for->iterator_expr = check_expression_type_and_cast(_for->iterator_expr, upper_type);
				init_type = _for->iterator_expr->type_info;
				_for->index_declaration->type_info = init_type;

				/* @Note: do we want to set initializer here?*/
				_for->index_declaration->initializer = _for->iterator_expr;

				if (!types_match(init_type, upper_type)) {
					compiler->report_error(_for, "'for' lower-range and upper-range types do not match!\n");
				}
			} else {
				/* Not range-based for */

				if (!_for->value_declaration) {
					_for->value_declaration = _for->index_declaration;
					_for->index_declaration = new Declaration();
					_for->index_declaration->identifier = make_identifier(compiler->atom_it_index);
					_for->index_declaration->identifier->scope = _for->value_declaration->identifier->scope;
					_for->value_declaration->identifier->scope->declarations.add(_for->index_declaration);

					copy_location_info(_for->index_declaration, _for->value_declaration);
				}

				if (!is_array(_for->iterator_expr->type_info)) {
					compiler->report_error(_for->iterator_expr, "Type of expression in 'for' condition is not iterable. Must be an integer range or an array");
					return;
				}
				
				auto count_expr = make_member(_for->iterator_expr, compiler->atom_length);
				infer_type(count_expr);

				assert(_for->upper_range_expr == 0);
				_for->upper_range_expr = count_expr;

				auto zero = make_integer_literal(0, count_expr->type_info);

				_for->index_declaration->type_info = zero->type_info;
				_for->index_declaration->initializer = zero;

				auto indexed = make_index(_for->iterator_expr, _for->index_declaration->identifier);
				infer_type(indexed);
				_for->value_declaration->type_info = indexed->type_info;
				_for->value_declaration->initializer = indexed;
			}

			type_check_statement(_for->body);

			break;
		}
		case AST_CONTINUE: {
			Continue *_continue = (Continue *) stmt;

			_continue->continue_to = current_loop_body;
		} break;
		case AST_BREAK: {
			Break *_break = (Break *) stmt;

			_break->break_to = current_loop_body;
		} break;
		default:
			infer_type(stmt);
			break;
	}
}

void Typer::type_check_variable_declaration(Declaration *decl) {
	Expression *existing = find_declaration_in_scope(decl->identifier->scope, decl->identifier);

	if (existing && existing != decl) {
		compiler->report_error2(decl->identifier->location, "Identifier is already defined", existing->location, "First definition here");
		return;
	}

	if (decl->type_info) {
		resolve_type_force(&decl->type_info);

		if (decl->initializer) {
			infer_type(decl->initializer);

			decl->initializer = check_expression_type_and_cast(decl->initializer, decl->type_info);

			if (!types_match(decl->type_info, decl->initializer->type_info)) {
				compiler->report_error(decl->initializer, "Type of initialization value and type of variable do not match");
			}
		}
	} else {
		assert(decl->initializer);

		infer_type(decl->initializer);
		decl->type_info = decl->initializer->type_info;
	}
}

void Typer::resolve_function_type(AFunction *function) {
	/* already resolved */
	if (function->type_info) return;

	TypeInfo *func_type = new TypeInfo();
	func_type->type = TYPE_FUNCTION;

	for (auto par : function->parameter_scope->declarations) {
		type_check_variable_declaration(static_cast<Declaration *>(par));
		func_type->parameters.add(par->type_info);
	}

	resolve_type_force(&function->return_type);
	func_type->return_type = function->return_type;

	function->type_info = func_type;
}

void Typer::type_check_function(AFunction *function) {
	AFunction *old_current_function = current_function;
	current_function = function;

	resolve_function_type(function);
	
	if (compiler->errors_reported) return;

	function->linkage_name = mangle_name(function);

	if (!(function->flags & FUNCTION_FOREIGN)) {
		type_check_scope(function->block_scope);
	}

	current_function = old_current_function;
}

void Typer::infer_type(Expression *expression) {
	while (expression->substitution) expression = expression->substitution;
	if (expression->type_info) return;

	switch (expression->type) {
	case AST_DECLARATION: {
		auto var_decl = static_cast<Declaration *>(expression);
		type_check_variable_declaration(var_decl);
	} break;
	case AST_LITERAL: {
		Literal *lit = static_cast<Literal *>(expression);
		switch (lit->literal_type) {
		case Literal::BOOL:
			lit->type_info = compiler->type_bool;
			break;
		case Literal::INT:
			lit->type_info = compiler->type_s32;
			break;
		case Literal::FLOAT:
			lit->type_info = compiler->type_f32;
			break;
		case Literal::STRING:
			lit->type_info = compiler->type_string;
			break;
		case Literal::NIL:
			lit->type_info = compiler->type_void_ptr;
			break;
		case Literal::COMPOUND: {
			lit->type_info = lit->compound_type_info;

			resolve_type_force(&lit->type_info);
			TypeInfo *ty = lit->type_info;

			if (is_struct(ty)) {
				if (lit->values.length != ty->struct_members.length) {
					compiler->report_error(lit, "Compound literal does not match struct type");
				}
				for (int i = 0; i < lit->values.length; ++i) {
					infer_type(lit->values[i]);
					auto val_type = lit->values[i]->type_info;
					auto field_type = ty->struct_members[i];
					if (!types_match(val_type, field_type)) {
						compiler->report_error(lit->values[i], "Compound literal does not match struct type");
					}
				}
			} else if (is_array(ty)) {
				auto arr_type = ty->element_type;

				if (!ty->is_dynamic) {
					if (ty->array_size == -1) {
						ty->array_size = lit->values.length;
					} else if (ty->array_size != lit->values.length) {
						compiler->report_error(lit, "Number of values in compound literal does not match array type");
					}
				} else {
					compiler->report_error(lit, "Compound literal is invalid for dynamic arrays");
				}

				for (int i = 0; i < lit->values.length; ++i) {
					infer_type(lit->values[i]);
					auto val_type = lit->values[i]->type_info;
					if (!types_match(val_type, arr_type)) {
						compiler->report_error(lit->values[i], "Compound literal does not match array type");
					}
				}
			} else {
				compiler->report_error(lit, "Illegal type for compound literal.\nExpected struct or array type!");
			}
		} break;
		}
	} break;
	case AST_IDENTIFIER: {
		Identifier *id = static_cast<Identifier *>(expression);
		Expression *decl = find_declaration_by_id(id);

		if (!decl) {
			compiler->report_error(id, "Variable is not defined");
			return;
		}

		if (decl->type == AST_FUNCTION) {
			resolve_function_type(static_cast<AFunction *>(decl));
			id->type_info = decl->type_info;
		} else {
			if (!decl->type_info) {
				compiler->report_error(id, "Variable not defined");
				return;
			}

			id->type_info = decl->type_info;
		}
	} break;
	case AST_CAST: {
		Cast *cast = static_cast<Cast *>(expression);
		infer_type(cast->expression);

		if (cast->target_type) {
			cast->type_info = cast->target_type;
		} else {
			cast->type_info = cast->expression->type_info;
			cast->type_info->auto_cast = true;
			cast->target_type = cast->type_info;
		}

		resolve_type_force(&cast->type_info);
	} break;
	case AST_CALL: {
		Call *call = static_cast<Call *>(expression);
		/* TODO: search for mangeled name instead */
		Expression *decl = find_declaration_by_id(call->identifier);

		if (!decl) {
			compiler->report_error(call->identifier, "Symbol not defined");
			return;
		}

		AFunction *function;

		if (decl->type == AST_FUNCTION) {
			function = static_cast<AFunction *>(decl);
			resolve_function_type(function);
		} else {
			infer_type(call->identifier);

			auto cit = call->identifier->type_info;

			if (is_function(cit)) {
				call->by_function_pointer = true;
			} else {
				compiler->report_error(call->identifier, "Symbol is not a function");
				return;
			}
		}

		if (call->by_function_pointer) {
			auto cit = call->identifier->type_info;

			/* TODO: allow template functions and varargs for function pointers */
			call->type_info = cit->return_type;

			if (!compare_arguments(call->identifier, &call->arguments, &cit->parameters, false)) {
				compiler->report_error(call, "Argument count does not match parameter count");

				return;
			}
		} else {
			if (function->flags & FUNCTION_TEMPLATE) {
				function = get_polymorph_function(call, function);
				call->type_info = function->return_type;
				call->resolved_function = function;
			} else {
				call->type_info = function->return_type;
				call->resolved_function = function;

				if (!compare_arguments(call->identifier, &call->arguments, &function->type_info->parameters, function->flags & FUNCTION_VARARG)) {
					compiler->report_error2(call->location, "Argument count does not match parameter count", function->location, "Parameters declared here");

					return;
				}
			}
		}
	} break;
	case AST_BINARY: {
		Binary *binary = static_cast<Binary *>(expression);
		infer_type(binary->lhs);
		infer_type(binary->rhs);

		auto lhs_type = binary->lhs->type_info;
		auto rhs_type = binary->rhs->type_info;
		auto eval_type = lhs_type;

		if (binop_is_conditional(binary->op)) {
			eval_type = compiler->type_bool;
		}

		if (binop_is_logical(binary->op)) {
			eval_type = compiler->type_bool;

			if (!is_bool(lhs_type)) {
				binary->lhs = make_compare_zero(binary->lhs);
			}

			if (!is_bool(rhs_type)) {
				binary->rhs = make_compare_zero(binary->rhs);
			}
		}

		if (binop_is_assign(binary->op)) {
			if (!expr_is_targatable(binary->lhs)) {
				compiler->report_error(binary, "Can't assign value to this expression");
			}

			if (binary->lhs->type == AST_IDENTIFIER) {
				Identifier *id = static_cast<Identifier *>(binary->lhs);
				Expression *var_expr = find_declaration_by_id(id);

				if (var_expr->type != AST_DECLARATION) {
					compiler->report_error(id, "Expected name of variable - not of struct or function");
					return;
				}

				Declaration *decl = static_cast<Declaration *>(var_expr);
				if (decl->flags & VAR_CONSTANT) {
					compiler->report_error(binary->lhs, "Can't assign value to constant variable");
				}
			}
		}

		if (binop_is_binary(binary->op)) {
			if (!(is_pointer(lhs_type) && is_int(rhs_type)) &&
				(!is_int(lhs_type) && !is_int(rhs_type)) &&
				(!is_float(lhs_type) && !is_float(rhs_type))) {
				/* TODO: check whether this if even makes sense */
				if (is_pointer(lhs_type) && (binary->op != '+' && binary->op != '-')) {
					compiler->report_error(binary, "Illegal binary operator for pointer type");
				}
				compiler->report_error(binary, "Illegal type for binary expression");
			}
		}

		/* TODO: check bug where eval_type is not set to bool and types get casted
		 * is eval_type then outdated? */
		binary->rhs = check_expression_type_and_cast(binary->rhs, binary->lhs->type_info);
		if (!types_match(binary->rhs->type_info, binary->lhs->type_info)) {
			binary->lhs = check_expression_type_and_cast(binary->lhs, binary->rhs->type_info);
			if (!types_match(binary->rhs->type_info, binary->lhs->type_info)) {
				compiler->report_error(binary, "Lhs and Rhs of binary expression are of different types");
			}
		}

		binary->type_info = eval_type;
	} break;
	case AST_UNARY: {
		Unary *unary = static_cast<Unary *>(expression);
		infer_type(unary->target);

		auto target_type = unary->target->type_info;

		switch (unary->op) {
		case TK_PLUS_PLUS:
		case TK_MINUS_MINUS:
			if (!is_int(target_type)) {
				compiler->report_error(unary, "Can't use ++ or -- on non-integer expression");
				break;
			}
			break;
		case '!':
			if (!is_bool(target_type)) {
				unary->target = make_compare_zero(unary->target);
			}
			target_type = compiler->type_bool;
			break;
		case '*': {
			if (!is_pointer(target_type)) {
				compiler->report_error(unary->target, "Can't dereference non variable");
				break;
			}

			target_type = target_type->element_type;
		} break;
		case '&':
			if (!expr_is_targatable(unary->target)) {
				compiler->report_error(unary->target, "Can't reference non variable");
				break;
			}

			TypeInfo *new_ty = new TypeInfo();

			new_ty->type = TYPE_POINTER;
			new_ty->element_type = target_type;

			target_type = new_ty;
			break;
		}

		unary->type_info = target_type;
	} break;
	case AST_SIZEOF: {
		Sizeof *size = static_cast<Sizeof *>(expression);
		size->type_info = compiler->type_s32;

		resolve_type_force(&size->target_type);
	} break;
	case AST_INDEX: {
		Index *index = static_cast<Index *>(expression);

		infer_type(index->expression);
		infer_type(index->index);

		auto expr_type = index->expression->type_info;

		if (!is_array(expr_type) && !is_string(expr_type) && !is_pointer(expr_type)) {
			compiler->report_error(index->index, "Cannot index dereference type that is not a string, array or pointer");
			break;
		}

		if (!is_int(index->index->type_info)) {
			compiler->report_error(index->index, "Expected an integer type as index to array");
			break;
		}

		if (is_string(expr_type)) {
			index->type_info = compiler->type_u8;
		} else {
			index->type_info = expr_type->element_type;
		}
	} break;
	case AST_MEMBER: {
		Member *member = static_cast<Member *>(expression);

		infer_type(member->left);

		if (is_pointer(member->left->type_info)) {
			auto un = new Unary();
			un->op = '*';
			un->target = member->left;
			copy_location_info(un, member->left);

			infer_type(un);

			member->left = un;
		}

		Atom *field_atom = member->field->atom;
		TypeInfo *left_type = member->left->type_info;

		if (is_array(left_type)) {
			if (left_type->array_size == -1) {
				if (field_atom == compiler->atom_data) {
					member->field_index = 0;
					member->type_info = make_pointer_type(left_type->element_type);
				} else if (field_atom == compiler->atom_length) {
					member->field_index = 1;
					member->type_info = compiler->type_s64;
				} else if (left_type->is_dynamic && field_atom == compiler->atom_capacity) {
					member->field_index = 2;
					member->type_info = compiler->type_s64;
				} else {
					String field_name = field_atom->id;
					compiler->report_error(member, "No member '%.*s' in type array.\n", field_name.length, field_name.data);
				}
			} else {
				if (field_atom == compiler->atom_data) {
					auto index_lit = make_integer_literal(0, compiler->type_s64);
					copy_location_info(index_lit, member);

					Index *index = new Index();
					index->expression = member->left;
					index->index = index_lit;
					copy_location_info(index, member);

					Unary *addr = new Unary();
					addr->op = '*';
					addr->target = index;
					copy_location_info(addr, member);

					infer_type(addr);
					member->substitution = addr;
					member->type_info = compiler->type_s64;
				} else if (field_atom == compiler->atom_length) {
					auto lit = make_integer_literal(left_type->array_size, compiler->type_s64);
					copy_location_info(lit, member);
					member->substitution = lit;
					member->type_info = compiler->type_s64;
				} else {
					String field_name = field_atom->id;
					compiler->report_error(member, "No member '%.*s' in known-size array.\n", field_name.length, field_name.data);
				}
			}
		} else if (is_struct(left_type)) {
			bool found = false;
			auto strct = left_type->struct_decl;
			int index = 0;

			for (auto mem : strct->members) {
				if (mem->identifier->atom == field_atom) {
					found = true;

					member->field_index = index;
					member->type_info = mem->type_info;
					break;
				}
				index++;
			}

			if (!found) {
				String field_name = field_atom->id;
				String name = left_type->struct_decl->identifier->atom->id;
				compiler->report_error(member, "No member '%.*s' in struct %.*s.\n", field_name.length, field_name.data, name.length, name.data);
			}
		} else if (is_string(left_type)) {
			if (field_atom == compiler->atom_data) {
				member->field_index = 0;
				member->type_info = compiler->type_string_data;
			} else if (field_atom == compiler->atom_length) {
				member->field_index = 1;
				member->type_info = compiler->type_s64;
			} else {
				String field_name = field_atom->id;
				compiler->report_error(member, "No member '%.*s' in type string.\n", field_name.length, field_name.data);
			}
		} else if (is_enum(left_type)) {
			bool found = false;
			for (auto enum_mem : left_type->enum_members) {
				if (field_atom == enum_mem.name) {
					if (enum_mem.value) {
						infer_type(enum_mem.value);
						member->substitution = enum_mem.value;
						member->type_info = enum_mem.value->type_info;
					} else {
						Literal *enum_index = new Literal();
						copy_location_info(enum_index, member);

						enum_index->type_info = compiler->type_s32;
						enum_index->literal_type = Literal::INT;
						enum_index->int_value = enum_mem.index;

						member->substitution = enum_index;
						member->type_info = enum_index->type_info;
					}
					found = true;
				}
			}

			if (!found) {
				compiler->report_error(member->field, "Enum member not found");
			}
		} else {
			compiler->report_error(member, "Tried to access type that is not a string, array or struct");
			return;
		}
	} break;
	}
}

/*
* resolves the types that could not be resolved during the parsing stage
* (mainly user defined types such as structs)
*/
TypeInfo *Typer::resolve_type_info(TypeInfo *type_info) {
	if (is_pointer(type_info) || is_array(type_info)) {
		TypeInfo *current = type_info;

		while (is_pointer(current) || is_array(current)) {
			if (current->element_type->type != TYPE_UNRESOLVED) {
				current = current->element_type;
				continue;
			}

			resolve_type_force(&current->element_type);
		}

		return type_info;
	}

	if (is_function(type_info)) {
		for (int i = 0; i < type_info->parameters.length; ++i) {
			resolve_type_force(&type_info->parameters[i]);
		}

		resolve_type_force(&type_info->return_type);
		return type_info;
	}

	if (type_info->type != TYPE_UNRESOLVED) return type_info;

	Expression *decl = find_declaration_by_id(type_info->unresolved_name);
	if (!decl) {
		return 0;
	}

	switch (decl->type) {
	case AST_STRUCT: {
		auto strct = static_cast<Struct *>(decl);

		TypeInfo *struct_type = strct->type_info;
		struct_type->struct_decl = strct;

		return struct_type;
	} break;
	case AST_TYPE_ALIAS: {
		auto ta = static_cast<TypeAlias *>(decl);
		if (ta->type_info) {
			resolve_type_force(&ta->type_info);
			return ta->type_info;
		}
		return 0;
	} break;
	case AST_DECLARATION: {
		compiler->report_error(type_info->unresolved_name, "Illegal type specifier: variable");
	} break;
	case AST_FUNCTION: {
		compiler->report_error(type_info->unresolved_name, "Illegal type specifier: function");
	} break;
	case AST_ENUM: {
		compiler->report_error(type_info->unresolved_name, "Illegal type specifier: enum");
	} break;
	}

	return type_info;
}

void Typer::resolve_type_force(TypeInfo **type_info) {
	TypeInfo *new_type = resolve_type_info(*type_info);

	if (new_type) {
		*type_info = new_type;
	} else {
		auto name = (*type_info)->unresolved_name;
		compiler->report_error(name,
			"Can't resolve symbol '%.*s'",
			name->atom->id.length, name->atom->id.data);
	}
}

/*
* compares the types of the arguments of a function call to the types in the function declaration
* returns true if they are compatible (with implicit type conversion)
* returns false if they are incompatible
*/
bool Typer::compare_arguments(Identifier *call, Array<Expression *> *args, Array<TypeInfo *> *par_types, bool varags) {
	auto par_count = par_types->length;

	if (par_count > args->length) {
		return false;
	}

	for (int i = 0; i < args->length; ++i) {
		if (i >= par_count) {
			if (varags) {
				infer_type((*args)[i]);
				continue;
			} else {
				return false;
			}
		}

		infer_type((*args)[i]);

		TypeInfo *par_type = (*par_types)[i];

		(*args)[i] = check_expression_type_and_cast((*args)[i], par_type);
		TypeInfo *arg_type = (*args)[i]->type_info;

		if (!types_match(arg_type, par_type)) {
			String par_ty_str = type_to_string(par_type);
			String arg_ty_str = type_to_string(arg_type);

			compiler->report_error(
				(*args)[i],
				"Type of %d. argument (%.*s) does not match type in function declaration (%.*s)",
				i + 1,
				arg_ty_str.length, arg_ty_str.data,
				par_ty_str.length, par_ty_str.data
			);
			/* TODO: return false leads to compiler->report_error called by caller on argument count mismatch */
			return false;
		}
	}

	return true;
}

/*
* compares the type of an expression to a desired type
* If they don't match, it tries implicit type conversion
* It returns the original expression if the types match
* or the Cast expression if it converted the type implictly

* This function does not guarantee that types are compatible even after implicit type conversion
* Is is the callers responsibility to check if the types match in the end
*/
Expression *Typer::check_expression_type_and_cast(Expression *expression, TypeInfo *other_type) {
	while (expression->substitution) expression = expression->substitution;

	auto rtype = expression->type_info;
	auto ltype = other_type;

	Expression *maybe_casted = expression;

	if (!types_match(ltype, rtype)) {
		if (is_int(ltype) && is_int(rtype)) {
			maybe_casted = make_cast(maybe_casted, ltype);
		} else if (is_float(ltype) && is_float(rtype)) {
			maybe_casted = make_cast(maybe_casted, ltype);
		} else if (is_float(ltype) && is_int(rtype)) {
			maybe_casted = make_cast(maybe_casted, ltype);
		} else if (is_int(ltype) && is_float(rtype) && rtype->auto_cast) {
			maybe_casted = make_cast(maybe_casted, ltype);
		} else if (is_pointer(ltype) && is_pointer(rtype)) {
			if (type_points_to_void(ltype) || type_points_to_void(rtype)) {
				auto llevel = get_pointer_level(ltype);
				auto rlevel = get_pointer_level(rtype);  

				if ((llevel == rlevel) || rtype->auto_cast) {
					maybe_casted = make_cast(maybe_casted, ltype);
				}
			} else if (rtype->auto_cast) {
				maybe_casted = make_cast(maybe_casted, ltype);
			}
		} else if (is_function(ltype) && is_pointer(rtype)) {
			maybe_casted = make_cast(maybe_casted, ltype);
		} else if (is_int(ltype) && is_bool(rtype)) {
			maybe_casted = make_cast(maybe_casted, ltype);
		} else if (is_bool(ltype) && is_int(rtype)) {
			maybe_casted = make_cast(maybe_casted, ltype);
		} else if (types_match(ltype, compiler->type_string_data) && is_string(rtype)) {
			maybe_casted = make_member(maybe_casted, compiler->atom_data);
			infer_type(maybe_casted);
		} else if (is_array(ltype) && is_array(rtype)) {
			if (types_match(ltype->element_type, rtype->element_type) && !ltype->is_dynamic && !rtype->is_dynamic && ltype->array_size == -1) {
				maybe_casted = make_cast(maybe_casted, ltype);
			}
		}
	}

	return maybe_casted;
}

Cast *Typer::make_cast(Expression *expression, TypeInfo *target_type) {
	Cast *cast = new Cast();

	cast->location = expression->location;
	cast->expression = expression;
	cast->target_type = target_type;
	cast->type_info = target_type;

	return cast;
}

/*
* Checks if a template functions has already been polymorphed with the argument types of the function call
* If there is already one, return it
* otherwise it creates a new polymorph function, type checks it and returns it
*/
AFunction *Typer::get_polymorph_function(Call *call, AFunction *template_function) {
	for (auto arg : call->arguments) {
		infer_type(arg);
	}

	for (auto overload : template_function->polymorphed_overloads) {
		bool does_match = true;

		if (call->arguments.length != template_function->parameter_scope->declarations.length) {
			compiler->report_error2(call->location, "Argument count does not match parameter count", template_function->location, "Parameters declared here");

			return template_function;
		}

		for (s64 i = 0; i < call->arguments.length; ++i) {
			auto par_type = overload->parameter_scope->declarations[i]->type_info;
			auto arg_type = call->arguments[i]->type_info;

			if (!types_match(par_type, arg_type)) {
				does_match = false;
			}
		}

		if (does_match) {
			return overload;
		}
	}

	auto polymorph = make_polymorph_function(template_function, &call->arguments);

	if (polymorph) {
		type_check_function(polymorph);
		template_function->polymorphed_overloads.add(polymorph);
	}

	return polymorph;
}

/*
* Resolves the type_info that is a TYPE before
*/
void resolve_polymorph_types(AFunction *poly, TypeInfo **type_info) {
	TypeInfo **par_to_set = 0;
		
	TypeInfo *current = *type_info;
	if (is_pointer(current) || is_array(current)) {
		while (is_pointer(current) || is_array(current)) {
			if (current->element_type->type == TYPE_TYPE) {
				par_to_set = &current->element_type;
				break;
			}

			current = current->element_type;
		}
	} else if (current->type == TYPE_TYPE) {
		par_to_set = type_info;
	}

	if (par_to_set) {
		auto par_type_id = (*par_to_set)->unresolved_name;
				
		for (auto tdecl : poly->template_scope->declarations) {
			TypeAlias *ta = (TypeAlias *) tdecl;

			if (ta->identifier->atom->id == par_type_id->atom->id) {
				*par_to_set = ta->type_info;
				break;
			}
		}
	}
}

/*
* Creates the polymorphed function from a template function
* Resolves the generic type aliases
*/
AFunction *Typer::make_polymorph_function(AFunction *template_function, Array<Expression *> *arguments) {
	auto poly = compiler->copier->copy_function(template_function);
	poly->flags &= ~FUNCTION_TEMPLATE;
	
	/* @Note: deliberately set it to zero here so that resolve_function_type doesn't skip it */
	poly->type_info = 0;

	for (s64 i = 0; i < arguments->length; ++i) {
		auto par = poly->parameter_scope->declarations[i];
		auto arg = (*arguments)[i];

		auto par_type = par->type_info;
		auto arg_type = arg->type_info;

		if (!can_fill_polymorph(par_type, arg_type)) {
			compiler->report_error(par, "Failed to fill '%s'", par_type->unresolved_name->atom);
			break;
		}
	}

	if (compiler->errors_reported)
		return poly;

	/* find bug where this triggers even though it shouldnt. For example calling array_reserve from array_add */
	for (auto decl : poly->template_scope->declarations) {
		auto alias = static_cast<TypeAlias *>(decl);
		if (alias->type_info->type == TYPE_TYPE) {
			auto name = alias->identifier->atom->id;

			compiler->report_error(alias, "Failed to fill out type '%.*s'", name.length, name.data);
		}
	}
	
	/* @Note: This is because of some stupid ass bug */
	for (auto decl : poly->parameter_scope->declarations) {
		resolve_polymorph_types(poly, &decl->type_info);
	}
	
	resolve_polymorph_types(poly, &poly->return_type);

	return poly;
}

/*
* Checks if the type of an argument matches the type of the parameter
* If parameter is a generic type alias
* check if it is already filled in
*		if so     -> compare already filled in type with argument type
*		otherwise -> fill in type with argument type
*/
bool Typer::can_fill_polymorph(TypeInfo *par_type, TypeInfo *arg_type) {
	if (par_type->type == TYPE_POINTER) {
		if (arg_type->type != TYPE_POINTER) return false;

		return can_fill_polymorph(par_type->element_type, arg_type->element_type);
	}

	if (is_primitive(par_type)) {
		return types_match(par_type, arg_type);
	}

	if (par_type->type == TYPE_TYPE) {
		auto decl = find_declaration_by_id(par_type->unresolved_name);
		if (!decl) {
			compiler->report_error(par_type->unresolved_name, "Undeclared identifier.\n");
			return false;
		}

		if (compiler->errors_reported) return false;

		if (decl->type == AST_TYPE_ALIAS) {
			auto alias = static_cast<TypeAlias *>(decl);
			if (alias->type_info->type == TYPE_TYPE) {
				alias->type_info = arg_type;
				return true;
			} else {
				return types_match(alias->type_info, arg_type);
			}
		} else if (decl->type == AST_STRUCT) {
			auto _struct = static_cast<Struct *>(decl);
			return types_match(par_type, arg_type);
		} else {
			assert(false);
		}
	}

	if (par_type->type == TYPE_ARRAY) {
		if (arg_type->type != TYPE_ARRAY) return false;

		bool success = can_fill_polymorph(par_type->element_type, arg_type->element_type);
		if (compiler->errors_reported) return false;

		return success;
	}

	return false;
}

/* checks if two types match */
bool Typer::types_match(TypeInfo *t1, TypeInfo *t2) {
	if (t1->type != t2->type) return false;

	if (t1->type == TYPE_POINTER) {
		return types_match(t1->element_type, t2->element_type);
	}

	if (t1->type == TYPE_STRUCT) {
		return t1->struct_decl == t2->struct_decl;
	}

	if (t1->type == TYPE_ARRAY) {
		return types_match(t1->element_type, t2->element_type) &&
			t1->array_size == t2->array_size &&
			t1->is_dynamic == t2->is_dynamic;
	}

	if (t1->type == TYPE_FUNCTION) {
		if (!types_match(t1->return_type, t2->return_type)) return false;
		if (t1->parameters.length != t2->parameters.length) return false;

		for (int i = 0; i < t1->parameters.length; ++i) {
			if (!types_match(t1->parameters[i], t2->parameters[i])) {
				return false;
			}
		}

		return true;
	}

	return t1->size == t2->size;
}

/*
* gets the depth a pointer type
* for example:
*		*u32   -> level 1
*		**u32  -> level 2
*		***u32 -> level 3
*/
s32 Typer::get_pointer_level(TypeInfo *type_info) {
	s32 count = 0;

	while (type_info) {
		if (type_info->type == TYPE_POINTER) {
			type_info = type_info->element_type;
			count++;
		} else {
			break;
		}
	}

	return count;
}

/*
* checks if a pointer type at some level points to a void
* used for void pointer coercion
*/
bool Typer::type_points_to_void(TypeInfo *type_info) {
	while (type_info) {
		if (type_info->type == TYPE_POINTER) {
			type_info = type_info->element_type;
		} else {
			return type_info->type == TYPE_VOID_TYPE;
		}
	}

	return false;
}

/*
* mangles function name
* if it is an external function or the main function -> return original name
* otherwise mangle name in the format:
*			NAME_PARAMETERTYPES
*
* for example: func add(a: s32, b: s32) s32
* becomes:     add_s32s32
*/
String Typer::mangle_name(AFunction *decl) {
	String name = decl->identifier->atom->id;

	if ((decl->flags & FUNCTION_FOREIGN) || decl->identifier->atom == compiler->atom_main)
		return name;

	StringBuilder sb;
	sb.print("%.*s", name.length, name.data);

	sb.putchar('_');

	TypeInfo *func_type = decl->type_info;
	for (auto par_type : func_type->parameters) {
		mangle_type(&sb, par_type);
	}

	return sb.to_string();
}

/*
* mangle type
* pointer        -> p$ELEMENT-TYPE
* void           -> v
* bool           -> b
* signed int     -> s$BYTE_SIZE0
* unsigned int   -> u$BYTE_SIZE
* float          -> f$BYTE_SIZE
* string         -> s
* struct	     -> S_$MEMBER-TYPES
* function	     -> F$RETURN-TYPE_$PARAMETER-TYPES
* dynamic array  -> Ad_$ELEMENT-TYPE_
* static array   -> As_$ELEMENT-TYPE_
* constant array -> Ak$SIZE_$ELEMENT-TYPE_
*/
void Typer::mangle_type(StringBuilder *builder, TypeInfo *type) {
	switch (type->type) {
	case TYPE_POINTER: {
		builder->putchar('p');
		mangle_type(builder, type->element_type);
	} break;
	case TYPE_VOID_TYPE: {
		builder->putchar('v');
	} break;
	case TYPE_BOOL: {
		builder->putchar('b');
	} break;
	case TYPE_INT: {
		if (type->is_signed) builder->putchar('s');
		else builder->putchar('u');

		builder->print("%d", type->size * 8);
	} break;
	case TYPE_FLOAT: {
		builder->putchar('f');
		builder->print("%d", type->size * 8);
	} break;
	case TYPE_STRING: {
		builder->putchar('s');
	} break;
	case TYPE_STRUCT: {
		builder->putchar('S');

		for (auto mem : type->struct_members) {
			mangle_type(builder, mem);
		}
	} break;
	case TYPE_FUNCTION: {
		builder->putchar('F');
		mangle_type(builder, type->return_type);

		builder->putchar('_');
		for (auto par : type->parameters) {
			mangle_type(builder, par);
		}
	} break;
	case TYPE_ARRAY: {
		builder->putchar('A');

		if (type->array_size >= 0) {
			builder->putchar('k');
			builder->print("%d", type->array_size);
		} else if (type->is_dynamic) {
			builder->putchar('d');
		} else {
			builder->putchar('s');
		}

		builder->putchar('_');
		mangle_type(builder, type->element_type);
		builder->putchar('_');
	} break;
	default:
		String unresolved = type->unresolved_name->atom->id;
		printf("Tried to mangle unresolved type %.*s\n", unresolved.length, unresolved.data);

		assert(0);
	}
}

/*
* creates an expression that compares the target expression to is not zero
* used when a non-conditional expression is used as a conditional expression
* for example:
* if 5 {} -> 5 becomes (5 != 0)
*/
Expression *Typer::make_compare_zero(Expression *target) {
	infer_type(target);

	TypeInfo *target_type = target->type_info;

	Literal *lit = new Literal();
	lit->location = target->location;
	lit->type_info = target_type;

	switch (target_type->type) {
	case TYPE_INT:
		lit->literal_type = Literal::INT;
		lit->int_value = 0;
		break;
	case TYPE_FLOAT:
		lit->literal_type = Literal::FLOAT;
		lit->float_value = 0.0;
		break;
	case TYPE_BOOL:
		lit->literal_type = Literal::BOOL;
		lit->int_value = 0;
		break;
	case TYPE_POINTER:
		lit->literal_type = Literal::NIL;
		break;
	default:
		compiler->report_error(target, "Expression has to be of type int, bool, float or pointer for auto compare zero");
	}

	Binary *be = new Binary();
	be->location = target->location;
	be->lhs = target;
	be->rhs = lit;
	be->op = TK_NOT_EQ;
	be->type_info = compiler->type_bool;
	return be;
}

Expression *Typer::find_declaration_in_scope(Scope *scope, Identifier *id) {
	Atom *name = id->atom;

	for (auto decl : scope->declarations) {
		switch (decl->type) {
		case AST_STRUCT: {
			auto strct = static_cast<Struct *>(decl);
			if (strct->identifier->atom == name)
				return decl;
		} break;
		case AST_TYPE_ALIAS: {
			auto ta = static_cast<TypeAlias *>(decl);
			if (ta->identifier->atom == name)
				return decl;
		} break;
		case AST_DECLARATION: {
			auto var_decl = static_cast<Declaration *>(decl);
			if (var_decl->identifier->atom == name)
				return decl;
		} break;
		case AST_FUNCTION: {
			auto fun = static_cast<AFunction *>(decl);
			if (fun->identifier->atom == name)
				return decl;
		} break;
		case AST_ENUM: {
			auto e = static_cast<Enum *>(decl);
			if (e->identifier->atom == name)
				return decl;
		} break;
		}
	}

	return 0;
}

Literal *Typer::make_integer_literal(s64 value, TypeInfo *type_info, Ast *source_loc) {
	Literal *lit = new Literal();
	lit->literal_type = Literal::INT;
	lit->int_value = value;
	lit->type_info = type_info;

	if (source_loc) copy_location_info(lit, source_loc);
	return lit;
}

Identifier *Typer::make_identifier(Atom *name) {
	Identifier *ident = new Identifier();
	ident->atom = name;
	return ident;
}

Member *Typer::make_member(Expression *aggregate_expression, Atom *field) {
	auto ident = make_identifier(field);
	copy_location_info(ident, aggregate_expression);

	Member *mem = new Member();
	copy_location_info(mem, aggregate_expression);
	mem->left = aggregate_expression;
	mem->field = ident;
	return mem;
}

Index *Typer::make_index(Expression *array, Expression *index) {
	Index *indx = new Index();
	indx->expression = array;
	indx->index = index;
	return indx;
}

bool expr_is_targatable(Expression *expression) {
	switch (expression->type) {
	case AST_IDENTIFIER:
	case AST_MEMBER:
	case AST_INDEX:
		return true;
	default:
		return false;
	}
}

void copy_location_info(Ast *left, Ast *right) {
	left->location = right->location;
}
