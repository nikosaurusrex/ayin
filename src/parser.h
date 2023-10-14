#ifndef PARSER_H
#define PARSER_H

#include "common.h"
#include "ast.h"
#include "lexer.h"

#define AST_NEW(type) (type *) ast_init(new type())

struct Compiler;
struct Parser {
	Compiler *compiler;
	Lexer *lexer;
	s64 pos;

	Scope *current_scope = 0;

	Parser(Compiler *compiler, Lexer *lexer);

	void parse();

	Expression *parse_global();

	Struct *parse_struct_declaration();
	Enum *parse_enum_declaration();
	TypeAlias *parse_type_alias();
	AFunction *parse_function_declaration();
	Declaration *parse_variable_declaration(bool expect_semicolon=false);
	void parse_variable_declaration_base(Declaration *var_decl);

	Expression *parse_directive();

	Expression *parse_declaration_or_statement(bool expect_semicolon=true);
	Expression *parse_expression(int precedence = 1);

	Expression *parse_binary(int precedence);
	Expression *parse_unary();
	Expression *parse_postfix();
	Expression *parse_primary();
	Literal *parse_literal();

	Identifier *parse_identifier();

	TypeInfo *parse_type_specifier();

	Ast *ast_init(Ast *ast);

	void push_scope();
	void pop_scope();

	bool expect_eat(int type);
	bool expect(int type, int off=0);
	Token peek(int off=0);
	Token next();

	TokenData token_data(Token token);
	SourceLocation token_location(Token token);
};

#endif
