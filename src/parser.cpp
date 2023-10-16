#include "parser.h"
#include "ast.h"
#include "common.h"
#include "lexer.h"
#include "compiler.h"

const int binary_operators[] = {
	TK_ADD_EQ,
	TK_SUB_EQ,
	TK_MUL_EQ,
	TK_DIV_EQ,
	TK_MOD_EQ,
	TK_SHL_EQ,
	TK_SHR_EQ,
	TK_XOR_EQ,
	TK_OR_EQ,
	TK_AND_EQ,
	'=',
	TK_BAR_BAR,
	TK_AND_AND,
	'|', '^', '&',
	TK_EQ_EQ,
	TK_NOT_EQ,
	TK_LT_EQ,
	TK_GT_EQ,
	'<', '>',
	TK_SHL,
	TK_SHR,
	'+', '-', '*', '/', '%',
};

const int binary_operators_precedence[] = {
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	2, 3, 4, 5, 6, 7, 7, 8, 8, 8, 8,
	9, 9, 19, 19, 11, 11, 11
};

Parser::Parser(Compiler *compiler, Lexer *lexer) {
	this->compiler = compiler;
	this->lexer = lexer;
	this->pos = 0;
}

void Parser::parse() {
	Token *t;
	while (token_type(peek()) != TK_END_OF_FILE) {
		Expression *stmt = parse_global();

		current_scope->declarations.add(stmt);
		current_scope->statements.add(stmt);
	}
}

Expression *Parser::parse_global() {
	Token t = peek();
	
	if (expect_eat(TK_STRUCT)) {
		return parse_struct_declaration();
	}
	
	if (expect_eat(TK_ENUM)) {
		return parse_enum_declaration();
	}
		
	if (expect_eat(TK_ALIAS)) {
		return parse_type_alias();
	}
	
	if (expect_eat(TK_FUNC)) {
		return parse_function_declaration();
	}

	if (expect_eat('#')) {
		return parse_directive();
	}

	Declaration *var_decl = parse_variable_declaration(true);
	if (var_decl) {
		var_decl->flags |= VAR_GLOBAL;
		return var_decl;
	}

	compiler->report_error(token_location(t), "Unexpected token: Expected function, struct, enum, variable or type declaration");
	return 0;
}

Struct *Parser::parse_struct_declaration() {
	auto s = AST_NEW(Struct);

	s->identifier = parse_identifier();
	if (!s->identifier) {
		compiler->report_error(s->identifier, "Expected struct name");
	}

	TypeInfo *struct_type = new TypeInfo();
	struct_type->type = TypeInfo::STRUCT;
	struct_type->struct_decl = s;

	if (!expect_eat('{')) {
		compiler->report_error(token_location(peek()), "Expected '{' after struct name");
		return 0;
	}

	while (!expect_eat('}')) {
		Declaration *decl = parse_variable_declaration();
		if (!decl) {
			compiler->report_error(token_location(peek()), "Expected variable name");
			return 0;
		}

		s->members.add(decl);
		struct_type->struct_members.add(decl->type_info);

		expect_eat(',');
	}

	s->type_info = struct_type;
	
	return s;
}

Enum *Parser::parse_enum_declaration() {
	auto e = AST_NEW(Enum);

	e->identifier = parse_identifier();

	if (!e->identifier) {
		compiler->report_error(e->identifier, "Expected enum name");
	}

	TypeInfo *enum_type = new TypeInfo();
	enum_type->type = TypeInfo::ENUM;

	if (!expect_eat('{')) {
		compiler->report_error(token_location(peek()), "Expected '{' after enum name");
		return 0;
	}

	s32 index = 0;
	while (!expect_eat('}')) {
		Identifier *mem_id = parse_identifier();
		TypeInfo::EnumMember mem;
		mem.name = mem_id->atom;

		if (expect_eat('=')) {
			mem.value = parse_literal();
		} else {
			if (!expect(',')) {
				compiler->report_error(token_location(peek()), "Expected '=' or ',' after enum member name");
			}
			mem.index = index;
		}
		index++;

		enum_type->enum_members.add(mem);

		expect_eat(',');
	}

	e->type_info = enum_type;

	return e;
}

TypeAlias *Parser::parse_type_alias() {
	auto ta = AST_NEW(TypeAlias);
	
	ta->identifier = parse_identifier();
	if (!ta->identifier) {
		compiler->report_error(ta->identifier, "Expected identifier for type alias");
	}

	ta->type_info = parse_type_specifier();

	if (!expect_eat(';')) {
		compiler->report_error(token_location(peek()), "Expected ';' after type alias");
	}

	return ta;
}

/* TODO: parse notes */
AFunction *Parser::parse_function_declaration() {
	auto fn = AST_NEW(AFunction);

	fn->identifier = parse_identifier();
	
	if (!fn->identifier) {
		compiler->report_error(fn->identifier, "Expected identifier for function name");
	}

	if (expect_eat('<')) {
		push_scope();

		fn->template_scope = current_scope;
		fn->flags |= FUNCTION_TEMPLATE;

		while (!expect_eat('>')) {
			TypeAlias *alias = AST_NEW(TypeAlias);
			alias->identifier = parse_identifier();
			alias->type_info = new TypeInfo();
			alias->type_info->type = TypeInfo::TYPE;

			if (!alias->identifier) {
				compiler->report_error(alias, "Expected template type name identifier");
			}

			fn->template_scope->declarations.add(alias);
		}
	}

	if (!expect_eat('(')) {
		compiler->report_error(token_location(peek()), "Expected '(' after function name");
	}

	push_scope();
	fn->parameter_scope = current_scope;
	while (!expect_eat(')')) {
		Declaration *par_decl = parse_variable_declaration();
		if (!par_decl) {
			compiler->report_error(token_location(peek()), "Expected variable name");
			return fn;
		}

		if (par_decl->initializer) {
			compiler->report_error(par_decl, "Can't initialize parameter");
		}

		current_scope->declarations.add(par_decl);

		if (!expect(')')) {
			if (!expect_eat(',')) {
				compiler->report_error(token_location(peek()), "Expected ',' after parameter");
			}
		}
	}

	bool is_decl = false;
	if (expect('{') || expect(';')) {
		fn->return_type = compiler->type_void;
		is_decl = expect(';');
		next();
	} else {
		if (!expect('#')) {
			fn->return_type = parse_type_specifier();
		} else {
			fn->return_type = compiler->type_void;
		}

		while (expect_eat('#')) {
			if (!expect(TK_ATOM)) {
				compiler->report_error(token_location(peek()), "Expected identifier after '#'");
			}

			Token note_token = peek();
			TokenData note_td = token_data(note_token);

			String token = note_td.lexeme;

			if (token == "foreign") {
				fn->flags |= FUNCTION_FOREIGN;
			} else if (token == "vararg") {
				fn->flags |= FUNCTION_VARARG;
			}

			next();
		}

		if (expect_eat(';')) {
			is_decl = true;
		} else if (!expect_eat('{')) {
			compiler->report_error(token_location(peek()), "Expected '{' or ';' after return type specifier");
		}
	}

	if (!is_decl) {
		push_scope();
		fn->block_scope = current_scope;

		while (!expect_eat('}')) {
			Expression *statement_or_declaration = parse_declaration_or_statement();
			if (!statement_or_declaration) {
				return fn;
			}

			current_scope->statements.add(statement_or_declaration);

			switch (statement_or_declaration->type) {
			case Ast::DECLARATION:
			case Ast::STRUCT:
			case Ast::ENUM:
			case Ast::TYPE_ALIAS:
			case Ast::FUNCTION:
				current_scope->declarations.add(statement_or_declaration);
				break;
			default:
				break;

			}
		}
	} else {
		fn->block_scope = 0;
	}

	if (fn->template_scope)
		pop_scope();

	if (fn->parameter_scope)
		pop_scope();

	if (!is_decl) {
		pop_scope();
	}

	return fn;
}

Declaration *Parser::parse_variable_declaration(bool expect_semicolon) {
	Declaration *var_decl = AST_NEW(Declaration);

	var_decl->identifier = parse_identifier();

	if (!var_decl->identifier) {
		return 0;
	}

	if (expect_eat(':')) {
		parse_variable_declaration_base(var_decl);
	} else {
		compiler->report_error(token_location(peek()), "Expected ':' after variable name");
		return 0;
	}

	if (expect_semicolon) {
		if (!expect_eat(';')) {
			compiler->report_error(token_location(peek()), "Expected ';' after variable declaration");
		}
	}

	return var_decl;
}

void Parser::parse_variable_declaration_base(Declaration *var_decl) {
	if (expect_eat('=')) {
		var_decl->initializer = parse_expression();
	} else if (expect_eat(':')) {
		var_decl->flags |= VAR_CONSTANT;
		var_decl->initializer = parse_expression();
	} else {
		var_decl->type_info = parse_type_specifier();

		if (expect_eat('=')) {
			var_decl->initializer = parse_expression();
		}
	}
}

Expression *Parser::parse_directive() {
	Directive *directive = AST_NEW(Directive);

	auto id = next();
	auto id_ty = token_type(id);
	auto id_td = token_data(id);

	if (id_ty == TK_ATOM) {
		if (id_td.lexeme == to_string("use")) {
			directive->directive_type = Directive::USE;
		} else if (id_td.lexeme == to_string("include")) {
			directive->directive_type = Directive::INCLUDE;
		}

		auto token = peek();
		if (!expect_eat(TK_STRING_LIT)) {
			compiler->report_error(token_location(token), "Expected string literal after #include or #use");
			return directive;
		}

		String name = token_data(token).lexeme;
		String base_path = basepath(lexer->file);

		if (!expect_eat(';')) {
			compiler->report_error(token_location(token), "Expected ';'");
			return directive;
		}

		if (directive->directive_type == Directive::USE) {
			directive->file = name;
		} else {
			const int MAX_PATH = 512;
			char fullname[MAX_PATH];
			snprintf(fullname, MAX_PATH, "%.*s%.*s", base_path.length, base_path.data, name.length, name.data);

			directive->file = copy_string(to_string(fullname));
		}
	} else if (id_ty == TK_IF) {
		auto cond = next();
		auto cond_ty = token_type(cond);
		auto cond_td = token_data(cond);

		if (cond_ty == TK_ATOM) {
			Expression *if_case = parse_declaration_or_statement();
			Expression *else_case = 0;

			if (token_type(peek()) == '#' &&
				token_type(peek(1)) == TK_ATOM &&
				token_data(peek(1)).lexeme == to_string("else")) {

				next();
				next();

				else_case = parse_declaration_or_statement();
			}

			bool cond_true = false;

			for (auto def : compiler->definitions) {
				if (def == cond_td.lexeme) {
					cond_true = true;
					break;
				}
			}

			if (cond_true) {
				return if_case;
			} else {
				if (else_case) {
					return else_case;
				}
				return parse_declaration_or_statement();
			}
		} else {
			compiler->report_error(token_location(cond), "Expected identifier after #if");
			return 0;
		}
	}

	compiler->handle_directive(directive);
	return directive;
}

Expression *Parser::parse_declaration_or_statement(bool expect_semicolon) {
	if (expect(TK_RETURN)) {
		Return *ret = AST_NEW(Return);
		next();
		if (!expect_eat(';')) {
			ret->return_value = parse_expression();
			if (!expect_eat(';') && expect_semicolon) {
				compiler->report_error(token_location(peek()), "expected ';' after return value");
			}
		}

		return ret;
	}

	if (expect(TK_IF)) {
		If *_if = AST_NEW(If);
		next();

		_if->condition = parse_expression();
        if (compiler->errors_reported) return _if;
        
        if (!_if->condition) {
            compiler->report_error(_if, "'if' must be followed by an expression.\n");
            return _if;
        }
        
        _if->then_statement = parse_declaration_or_statement();
        
        if (expect(TK_ELSE)) {
            next();
            
            _if->else_statement = parse_declaration_or_statement();
        }

		return _if;
	}

	if (expect(TK_WHILE)) {
		While *_while = AST_NEW(While);
		next();

		_while->condition = parse_expression();
        
        if (!_while->condition) {
            compiler->report_error(_while, "'while' must be followed by an expression.\n");
            return _while;
        }
        
        _while->statement = parse_declaration_or_statement();

		return _while;
	}

	if (expect(TK_FOR)) {
		For *_for = AST_NEW(For);
		next();

		push_scope();

		Identifier *first_operand = parse_identifier();
		if (!first_operand) {
			compiler->report_error(token_location(peek()), "Expected identifier after 'for'");
		}

		_for->index_declaration = AST_NEW(Declaration);
		_for->index_declaration->identifier = first_operand;

		current_scope->declarations.add(_for->index_declaration);

		if (expect_eat(',')) {
			Identifier *second_operand = parse_identifier();
			if (!second_operand) {
				compiler->report_error(token_location(peek()), "Expected identifier after ','");
			}
			
			_for->value_declaration = AST_NEW(Declaration);
			_for->value_declaration->identifier = second_operand;

			current_scope->declarations.add(_for->value_declaration);
		}

		if (!expect_eat(TK_IN)) {
			compiler->report_error(token_location(peek()), "Expected 'in' after for loop variables");
		}
		
		_for->iterator_expr = parse_expression();

		auto token = peek();
		if (expect_eat(TK_DOT_DOT)) {
			if (!_for->iterator_expr) {
				compiler->report_error(token_location(token), ".. operator must be preceeded by an expression.\n");
				return _for;
			}

			_for->upper_range_expr = parse_expression();
		}

		_for->body = parse_declaration_or_statement();

		pop_scope();

		return _for;
	}

	if (expect_eat('{')) {
		push_scope();
		Scope *scope = current_scope;

		while (!expect_eat('}')) {
			Expression *statement_or_declaration = parse_declaration_or_statement();
			if (!statement_or_declaration) {
				return scope;
			}

			current_scope->statements.add(statement_or_declaration);

			switch (statement_or_declaration->type) {
				case Ast::DECLARATION:
				case Ast::STRUCT:
				case Ast::ENUM:
				case Ast::TYPE_ALIAS:
				case Ast::FUNCTION:
					current_scope->declarations.add(statement_or_declaration);
					break;
				default:
					break;

			}
		}

		pop_scope();
		return scope;
	}

	if (expect(TK_CONTINUE)) {
		auto _continue = AST_NEW(Continue);
		next();
		
		if (!expect_eat(';') && expect_semicolon) {
			compiler->report_error(token_location(peek()), "expected ';' after 'continue'");
		}
			
		return _continue;
	}

	if (expect(TK_BREAK)) {
		auto _continue = AST_NEW(Break);
		next();

		if (!expect_eat(';') && expect_semicolon) {
			compiler->report_error(token_location(peek()), "expected ';' after 'break'");
		}

		return _continue;
	}

	if (expect_eat('#')) {
		return parse_directive();
	}
	 
	Expression *expr = parse_expression();
    if (!expect_eat(';') && expect_semicolon) {
        compiler->report_error(token_location(peek()), "expected ';' after expression");
    }
    return expr;
}

Expression *Parser::parse_expression(int precedence) {
	return parse_binary(precedence);
}

Expression *Parser::parse_binary(int precedence) {
	auto lhs = parse_unary();

	while (true) {
		auto tok = peek();
		auto tok_type = token_type(tok);

		int op_prec = 0;
		int index = 0;
		for (auto bin_op : binary_operators) {
			if (bin_op == tok_type) {
				op_prec = binary_operators_precedence[index];
				break;
			}
			index++;
		}

		if (!op_prec || op_prec < precedence) {
			break;
		}

		next();

		Binary *binary = AST_NEW(Binary);
		binary->lhs = lhs;
		binary->op = tok_type;
        binary->rhs = parse_binary(op_prec + 1);

        lhs = binary;
	}

	return lhs;
}

Expression *Parser::parse_unary() {
	auto tok = peek();

	if (expect('&')) {
		auto expr = AST_NEW(Unary);
		next();

		expr->target = parse_postfix();
		expr->op = '&';

		return expr;
	} else if (expect('*')) {
		auto expr = AST_NEW(Unary);
		next();

		expr->target = parse_unary();
		expr->op = '*';

		return expr;
	} else if (expect_eat('+')) {
		return parse_postfix();
	} else if (expect('!')) {
		auto expr = AST_NEW(Unary);
		next();

		expr->target = parse_postfix();
		expr->op = '!';

		return expr;
	} else if (expect('-')) {
		auto expr = AST_NEW(Unary);
		next();

		expr->target = parse_postfix();
		expr->op = '-';

		return expr;
	} else if (expect(TK_PLUS_PLUS) || expect(TK_MINUS_MINUS)) {
		auto expr = AST_NEW(Unary);
		next();

		expr->target = parse_postfix();
		expr->is_pre = true;
		expr->op = token_type(tok);

		return expr;
	} else if (expect(TK_CAST)) {
		auto cast = AST_NEW(Cast);
		next();

		cast->target_type = 0;
        if (expect_eat('(')) {
			cast->target_type = parse_type_specifier();

			if (!expect_eat(')')) {
				compiler->report_error(token_location(peek()), "Expected ')'");
				return cast;
			}
		}

		cast->expression = parse_expression();
		return cast;
	}

	return parse_postfix();
}

Expression *Parser::parse_postfix() {
	auto target = parse_primary();
	auto tok = peek();

    while (!expect(TK_END_OF_FILE)) {
        if (expect('.')) {
            Member *member = AST_NEW(Member);
            next();
            
            auto right = parse_identifier();
            if (!right) return nullptr;
            
            member->left = target;
            member->field = right;
            
            target = member;
        } else if (expect('[')) {
			Index *index = AST_NEW(Index);
            next();

            index->expression = target;
            index->index = parse_expression();
            
			if (!expect_eat(']')) {
				compiler->report_error(token_location(peek()), "Expected ']'");
			}
            
            target = index;
        } else {
            break;
        }
	}

	tok = peek();
	if (expect_eat(TK_PLUS_PLUS) || expect_eat(TK_MINUS_MINUS)) {
		auto expr = AST_NEW(Unary);

		expr->target = target;
		expr->is_pre = false;
		expr->op = token_type(tok);

		return expr;
	}

	return target;
}

Expression *Parser::parse_primary() {
	Token t = peek();

	if (expect_eat('(')) {
		auto expr = parse_expression();
		if (!expect_eat(')')) {
			compiler->report_error(token_location(peek()), "Expected ')'");
			next();
		}
		return expr;
	}

	if (expect(TK_ATOM)) {
		TokenData td = token_data(t);
		Identifier *id = AST_NEW(Identifier);
		id->atom = compiler->make_atom(td.lexeme);
		id->scope = current_scope;

		next();

		if (expect_eat(':')) {
			Declaration *decl = AST_NEW(Declaration);
			decl->location = id->location;
			decl->identifier = id;

			parse_variable_declaration_base(decl);

			return decl;
		}

		if (expect_eat('(')) {
			Call *call = AST_NEW(Call);
			call->identifier = id;

			while(!expect_eat(')')) {
				call->arguments.add(parse_expression());

				if (!expect(')')) {
					if (!expect_eat(',')) {
						compiler->report_error(token_location(peek()), "Expected ',' after argument");
					}
				}
			}

			return call;
		}

		return id;
	}

	if (expect(TK_NIL)) {
		Literal *lit = AST_NEW(Literal);
		next();

		lit->literal_type = Literal::NIL;
		return lit;
	}

	if (expect(TK_SIZEOF)) {
        Sizeof *size = AST_NEW(Sizeof);
        next();
        
        if (!expect_eat('(')) {
        	compiler->report_error(token_location(peek()), "Expected '('");
        	return size;
		}
        
        size->target_type = parse_type_specifier();
        
        if (!expect_eat(')')) {
        	compiler->report_error(token_location(peek()), "Expected ')'");
			return size;
		}
        
        return size;
    }

	return parse_literal();
}

Literal *Parser::parse_literal() {
	auto t = peek();
	TokenData td = token_data(t);

	/* temporary solution to having negative numbers also in compound literals which calls this function directly */
	bool negate = expect('-');
	if (negate) {
		next();
		t = peek();
	}

	if (expect(TK_INT_LIT)) {
		Literal *lit = AST_NEW(Literal);
		next();

		lit->literal_type = Literal::INT;
		lit->int_value = td.int_literal;
		if (negate) {
			lit->int_value = -td.int_literal;
		}
		return lit;
	}

	if (expect(TK_FLOAT_LIT)) {
		Literal *lit = AST_NEW(Literal);
		next();

		lit->literal_type = Literal::FLOAT;
		lit->float_value = td.float_literal;
		if (negate) {
			lit->float_value = -td.float_literal;
		}
		return lit;
	}
	
	if (expect(TK_STRING_LIT)) {
		Literal *lit = AST_NEW(Literal);
		next();

		lit->literal_type = Literal::STRING;
		lit->string_value = td.lexeme;
		return lit;
	}

	if (expect(TK_TRUE)) {
		Literal *lit = AST_NEW(Literal);
		next();

		lit->literal_type = Literal::BOOL;
		lit->int_value = 1;
		return lit;
	}
	
	if (expect(TK_FALSE)) {
		Literal *lit = AST_NEW(Literal);
		next();

		lit->literal_type = Literal::BOOL;
		lit->int_value = 0;
		return lit;
	}

	if (expect('{')) {
		Literal *lit = AST_NEW(Literal);
		next();

		lit->literal_type = Literal::COMPOUND;

		while (!expect_eat('}')) {
			lit->values.add(parse_literal());

			if (!expect('}')) {
				if (!expect_eat(',')) {
					compiler->report_error(token_location(peek()), "Expected ','");
				}
			}
	 	}

		lit->compound_type_info = parse_type_specifier();

		return lit;
	}

	return 0;
}

Identifier *Parser::parse_identifier() {
	if(!expect(TK_ATOM)) {
		return 0;
	}

	auto id = AST_NEW(Identifier);
	Token t = next();
	TokenData td = token_data(t);

	id->atom = compiler->make_atom(td.lexeme);
	id->scope = current_scope;

	return id;
}

TypeInfo *Parser::parse_type_specifier() {
	TypeInfo *type_info = 0;
	Token t = peek();
	int tt = token_type(t);
	
	if (tt == TK_ATOM) {
		TokenData td = token_data(t);
		if (td.lexeme == "str") type_info = compiler->type_string;
		else if (td.lexeme == "void") type_info = compiler->type_void;
		else if (td.lexeme == "bool") type_info = compiler->type_bool;
		else if (td.lexeme == "s8") type_info = compiler->type_s8;
		else if (td.lexeme == "s16") type_info = compiler->type_s16;
		else if (td.lexeme == "s32") type_info = compiler->type_s32;
		else if (td.lexeme == "s64") type_info = compiler->type_s64;
		else if (td.lexeme == "u8") type_info = compiler->type_u8;
		else if (td.lexeme == "u16") type_info = compiler->type_u16;
		else if (td.lexeme == "u32") type_info = compiler->type_u32;
		else if (td.lexeme == "u64") type_info = compiler->type_u64;
		else if (td.lexeme == "f32") type_info = compiler->type_f32;
		else if (td.lexeme == "f64") type_info = compiler->type_f64;
	}

	if (type_info) {
		next();
		return type_info;
	}

	if (tt == '*') {
		next();

		TypeInfo *element_type = parse_type_specifier();
		if (!element_type) {
			compiler->report_error(token_location(peek()), "Failed to parse pointer element type");
			return 0;
		}

		type_info = new TypeInfo();
		type_info->type = TypeInfo::POINTER;
		type_info->element_type = element_type;
		type_info->size = 8;
		return type_info;
	}

	if (tt == '[') {
		next();
		
		Token num_token = peek();
		s32 arr_size = -1;
		bool dynamic = false;

		TokenData td = token_data(num_token);

		if (token_type(num_token) == TK_INT_LIT) {
			next();
			arr_size = td.int_literal;
			dynamic = false;
		} else if (expect(TK_DOT_DOT)) {
			next();
			dynamic = true;
		}

		if (!expect_eat(']')) {
			compiler->report_error(token_location(peek()), "Expected ']'");
			return 0;
		}

		TypeInfo *element_type = parse_type_specifier();
		if (!element_type) {
			compiler->report_error(token_location(peek()), "Failed to parse element type of array type");
			return 0;
		}

		type_info = new TypeInfo();
		type_info->type = TypeInfo::ARRAY;
		type_info->is_dynamic = dynamic;
		type_info->array_size = arr_size; 
		type_info->element_type = element_type;
		return type_info;
	}

	if (tt == '(') {
		next();

		type_info = new TypeInfo();
		type_info->type = TypeInfo::FUNCTION;

		while (!expect_eat(')')) {
			TypeInfo *par_type = parse_type_specifier();
			if (!par_type) {
				compiler->report_error(token_location(peek()), "Expected type");
				return 0;
			}

			type_info->parameters.add(par_type);

			if (!expect(')')) {
				if (!expect_eat(',')) {
					compiler->report_error(token_location(peek()), "Expected ',' after parameter type");
				}
			}
		}

		type_info->return_type = parse_type_specifier();
		if (!type_info->return_type)
			compiler->report_error(token_location(peek()), "Expected return type");

		return type_info;
	}

	if (tt == TK_ATOM) {
		type_info = new TypeInfo();
		type_info->type = TypeInfo::UNRESOLVED;
		type_info->unresolved_name = parse_identifier();
		return type_info;
	}

	return 0;
}

Ast *Parser::ast_init(Ast *ast) {
	ast->location = token_location(peek());
	return ast;
}

void Parser::push_scope() {
	Scope *new_scope = AST_NEW(Scope);
	new_scope->parent = current_scope;
	current_scope = new_scope;
}

void Parser::pop_scope() {
	assert(current_scope->parent);
	current_scope = current_scope->parent;
}

bool Parser::expect_eat(int type) {
	if (expect(type)) {
		pos++;
		return true;
	}
	return false;
}

bool Parser::expect(int type, int off) {
	Token tok = peek(off);
	int tt = token_type(tok);

	if (tt == type) {
		return true;
	}

	if (tt == TK_END_OF_FILE) {
		compiler->report_error(token_location(tok), "Encountered unexpected 'end of file' during parsing");
		return true;
	}

	return false;
}

Token Parser::peek(int off) {
	if (pos + off >= lexer->tokens.length) {
		Token t = lexer->tokens[lexer->tokens.length - 1];
		return t;
	}
	return lexer->tokens[pos + off];
}

Token Parser::next() {
	return lexer->tokens[pos++];
}

TokenData Parser::token_data(Token token) {
	return lexer->data[token_index(token)];
}

SourceLocation Parser::token_location(Token token) {
	return lexer->locations[token_index(token)];
}
