#ifndef LEXER_H
#define LEXER_H

#include "common.h"

struct SourceLocation {
	String file;
	s64 col;
	s64 line;
	s64 length;
};

enum TokenType {
	TK_ERROR = 0,
	TK_END_OF_FILE = 1,

	TK_DOT_DOT,
	TK_DOT_DOT_DOT,

	TK_PLUS_PLUS,
	TK_MINUS_MINUS,

	TK_ADD_EQ,
	TK_SUB_EQ,
	TK_MUL_EQ,
	TK_DIV_EQ,
	TK_MOD_EQ,

	TK_EQ_EQ,
	TK_NOT_EQ,
	TK_LT_EQ,
	TK_GT_EQ,

	TK_SHL,
	TK_SHR,
	TK_SHR_EQ,
	TK_SHL_EQ,
	TK_AND_EQ,
	TK_OR_EQ,
	TK_XOR_EQ,

	TK_AND_AND,
	TK_BAR_BAR,

	TK_ATOM = 128,
	TK_INT_LIT,
	TK_FLOAT_LIT,
	TK_STRING_LIT,
	TK_CHAR_LIT,

	TK_RETURN,
	TK_TRUE,
	TK_FALSE,
	TK_IF,
	TK_ELSE,
	TK_WHILE,
	TK_FOR,
	TK_ENUM,
	TK_FUNC,
	TK_STRUCT,
	TK_ALIAS,
	TK_BREAK,
	TK_CONTINUE,
	TK_CAST,
	TK_DEFER,
	TK_USING,
	TK_NIL,
	TK_SIZEOF,
	TK_IN
};

struct TokenData {
	String lexeme;
	
	union {
		s64 int_literal;
		f64 float_literal;
	};
};

typedef u32 Token;

struct Compiler;
struct Lexer {
	Array<char *> preproc_definitions;
	
	Allocator allocator;
	Array<Token> tokens = {};
	Array<TokenData> data = {};
	Array<SourceLocation> locations = {};

	Compiler *compiler;
	String input;
  	String file;
	s64 input_len;

	s64 line;
	s64 col;
	s64 pos;

	Lexer(Compiler *compiler, String path, String code);

	void tokenize();

	u8 read_atom_or_keyword(TokenData *td);
	u8 read_number(TokenData *td);
	u8 read_string_literal(TokenData *td);
	u8 read_char_literal(TokenData *td);

	void read_token();

	SourceLocation get_current_location();

	char peek_char(int ahead = 0);
	void eat_char();
    
	void init_preproc_definitions();
};

u32 token_index(Token token);
u8 token_type(Token token);

#endif
