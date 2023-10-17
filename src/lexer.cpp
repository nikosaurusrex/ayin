#include <iostream>
#include <sstream>

#include "common.h"
#include "lexer.h"
#include "compiler.h"

extern Allocator string_allocator = libc_allocator();

static Token make_token(u32 index, u8 type);

Lexer::Lexer(Compiler *compiler, String path, String code) {
	this->compiler = compiler;
    this->file = path;
	this->input = code;

	init_preproc_definitions();

	input_len = code.length;
	col = 0;
	line = 0;
	pos = 0;
}

void Lexer::tokenize() {
	while (pos < input_len) {
		read_token();
	}
        
    assert(tokens.length > 0);
	if (token_type(tokens[tokens.length - 1])  != TK_END_OF_FILE) {
		Token eof = make_token(tokens.length, TK_END_OF_FILE);
		tokens.add(eof);
		locations.add(SourceLocation{});
		data.add(TokenData{});
	}
}

SourceLocation Lexer::get_current_location() {
	SourceLocation loc = {};

	loc.file = file;
	loc.col = col;
	loc.line = line;
	loc.length = 1;

	return loc;
}

u8 Lexer::read_atom_or_keyword(TokenData *td) {
	auto start_pos = pos;	
	auto c = peek_char();

	while (c == '_' || isalnum(c)) {
		// @Robustness: check for end of file
		
		eat_char();
		c = peek_char();
	}

	auto end_pos = pos;
	auto len = end_pos - start_pos;
	String lexeme = input.substring(start_pos, len);

	td->lexeme = lexeme;

	switch (lexeme.length) {
		case 2: {
			if (lexeme == "if") return TK_IF;
			else if (lexeme == "in") return TK_IN;
			else return TK_ATOM;
		} break;
		case 3: {
			if (lexeme == "for") return TK_FOR;
			else return TK_ATOM;
		} break;
		case 4: {
			if (lexeme == "true") return TK_TRUE;
			else if (lexeme == "else") return TK_ELSE;
			else if (lexeme == "enum") return TK_ENUM;
			else if (lexeme == "func") return TK_FUNC;
			else if (lexeme == "null") return TK_NIL;
			else if (lexeme == "cast") return TK_CAST;
			else return TK_ATOM;
		} break;
		case 5: {
			if (lexeme == "false") return TK_FALSE;
			else if (lexeme == "alias") return TK_ALIAS;
			else if (lexeme == "break") return TK_BREAK;
			else if (lexeme == "defer") return TK_DEFER;
			else if (lexeme == "while") return TK_WHILE;
			else return TK_ATOM;
		} break;
		case 6: {
			if (lexeme == "return") return TK_RETURN;
			else if (lexeme == "struct") return TK_STRUCT;
			else if (lexeme == "sizeof") return TK_SIZEOF;
			else return TK_ATOM;
		} break;
		case 8: {
			if (lexeme == "CONTINUE") return TK_CONTINUE;
			else return TK_ATOM;
		} break;
		default: {
			return TK_ATOM;
		};
	}
}

u8 Lexer::read_number(TokenData *td) {
	char c = peek_char();
	char peek_one = peek_char(1);
	if (c == '0'&& (peek_one == 'x' || peek_one == 'o' || peek_one == 'b')) {
		int base = 0;

		eat_char();
		eat_char();

		if (peek_one == 'x') {
			base = 16;
		} else if (peek_one == 'o') {
			base = 8;
		} else if (peek_one == 'b') {
			base = 2;
		}

		auto start_pos = pos;	
		auto c = peek_char();

		while (isdigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) {
			// @Robustness: check for end of file
			
			eat_char();
			c = peek_char();
		}

		auto end_pos = pos;
		auto len = end_pos - start_pos;
		String lexeme = input.substring(start_pos, len);

		td->lexeme = lexeme;

		char *c_str_lexeme = to_c_string(lexeme);

		// @Robustness: check conversion errors
		td->int_literal = strtoll(c_str_lexeme, 0, base);
		return TK_INT_LIT;
	} else {
		auto start_pos = pos;	
		auto c = peek_char();
		bool has_dot = false;

		while (isdigit(c) || (c == '.' && peek_char(1) != '.')) {
			// @Robustness: check for end of file
			
			if (c == '.') {
				if (has_dot) {
					compiler->report_error(get_current_location(), "Unexpected '.' while parsing float");
				}

				has_dot = true;
			}
			
			eat_char();
			c = peek_char();
		}

		auto end_pos = pos;
		auto len = end_pos - start_pos;
		String lexeme = input.substring(start_pos, len);

		td->lexeme = lexeme;

		char *c_str_lexeme = to_c_string(lexeme);
		if (has_dot) {
			// @Robustness: check conversion errors
			td->float_literal = atof(c_str_lexeme);

			return TK_FLOAT_LIT;
		} else {
			// @Robustness: check conversion errors
			td->int_literal = strtoll(c_str_lexeme, 0, 10);
			
			return TK_INT_LIT;
		}
	}

	assert(0 && "unreachable");
	return 0;
}

// @Incomplete check for unicode chars and escape codes
u8 Lexer::read_string_literal(TokenData *td) {
	eat_char();

	auto c = peek_char();

	StringBuilder sb = {};

	c = peek_char();
	while (c != '"') {
		// @Robustness: check for end of file

		if (c == '\\') {
			eat_char();
			c = peek_char();
			
			switch (c) {
				case '\\': c = '\\'; break;
				case 'n':  c = '\n';  break;
				case 't':  c = '\t';  break;
				case '0':  c = 0;    break;
				case '"':  c = '"';  break;
				case '\'': c = '\''; break;
				case 'b':  c = '\b';  break;
				case 'f':  c = '\f';  break;
				case 'r':  c = '\r';  break;
			}

			sb.putchar(c);
		} else {
			sb.putchar(c);
		}
		
		eat_char();
		c = peek_char();
	}

	eat_char();

	String lexeme = sb.to_string();
	td->lexeme = lexeme;
	return TK_STRING_LIT;
}

u8 Lexer::read_char_literal(TokenData *td) {
	// @Incomplete: add unicode characters
	
	eat_char();
	char c = peek_char();

	if (c == '\'') {
		compiler->report_error(get_current_location(), "Empty character literals are not allowed");
	}
	
	if (c == '\\') {
		eat_char();

		c = peek_char();
		
		switch (c) {
            case '\\': c = '\\'; break;
            case 'n':  c = '\n';  break;
            case 't':  c = '\t';  break;
            case '0':  c = 0;    break;
			case '"':  c = '"';  break;
            case '\'': c = '\''; break;
            case 'b':  c = '\b';  break;
            case 'f':  c = '\f';  break;
            case 'r':  c = '\r';  break;
            default: {
				compiler->report_error(get_current_location(), "Unknown escape character encountered when parsing character literal '%c'", c);
            }
        }
	}

	eat_char();

	td->int_literal = c;
	return TK_CHAR_LIT;
}


void Lexer::read_token() {
	auto c = peek_char();

	while (isspace(c)) {
		// @Robustness: check for end of file
		eat_char();
		c = peek_char();
	}

	if (c == '/' && peek_char(1) == '/') {
		while (c != '\n') {
			// @Robustness: check for end of file
			eat_char();
			c = peek_char();
		}
		eat_char();
		return read_token();
	}

	if (c == '/' && peek_char(1) == '*') {
		while (c != '*' || peek_char(1) != '/') {
			// @Robustness: check for end of file
			eat_char();
			c = peek_char();
		}
		eat_char();
		eat_char();
		return read_token();
	}

	u32 index = tokens.length;
	u8 type = 0;

	SourceLocation location = get_current_location();
	TokenData tdata = {};

	if (c == '\0') {
		type = TK_END_OF_FILE;
		tdata.lexeme = "eof";
	} else if (c == '_' || isalpha(c)) {
		type = read_atom_or_keyword(&tdata);
	} else if (isdigit(c)) {
		type = read_number(&tdata);
	} else if (c == '"') {
		type = read_string_literal(&tdata);
	} else if (c == '\'') {
		type = read_char_literal(&tdata);
	} else {
		switch (c) {
			case '{': case '}': case '[': case ']':
			case '(': case ')': case ';': case ',':
			case ':': case '#': case '$': {
				type = c;
			 	eat_char();
			} break;
			default: {
				if (c == '.') {
					if (peek_char(1) == '.') {
						if (peek_char(2) == '.') {
							type = TK_DOT_DOT_DOT;
			 				eat_char();
						}
						type = TK_DOT_DOT;
						eat_char();
					} else {
						type = '.';
					}
					eat_char();
				} else if (c == '+') {
					if (peek_char(1) == '+') {
						type = TK_PLUS_PLUS;
						eat_char();
					} else if (peek_char(1) == '=') {
						type = TK_ADD_EQ;
						eat_char();
					} else {
						type = '+';
					}
					eat_char();
				} else if (c == '-') {
					if (peek_char(1) == '-') {
						type = TK_MINUS_MINUS;
						eat_char();
					} else if (peek_char(1) == '=') {
						type = TK_SUB_EQ;
						eat_char();
					} else {
						type = '-';
					}
					eat_char();
				} else if (c == '*') {
					if (peek_char(1) == '=') {
						type = TK_MUL_EQ;
						eat_char();
					} else {
						type = '*';
					}
					eat_char();
				} else if (c == '/') {
					if (peek_char(1) == '=') {
						type = TK_DIV_EQ;
						eat_char();
					} else {
						type = '/';
					}
					eat_char();
				} else if (c == '%') {
					if (peek_char(1) == '=') {
						type = TK_MOD_EQ;
						eat_char();
					} else {
						type = '%';
					}
					eat_char();
				} else if (c == '=') {
					if (peek_char(1) == '=') {
						type = TK_EQ_EQ;
						eat_char();
					} else {
						type = '=';
					}
					eat_char();
				} else if (c == '<') {
					if (peek_char(1) == '<') {
						if (peek_char(2) == '=') {
							type = TK_SHL_EQ;
							eat_char();
						} else {
							type = TK_SHL;
						}
						eat_char();
					} else if (peek_char(1) == '=') {
						type = TK_LT_EQ;
						eat_char();
					} else {
						type = '<';
					}
					eat_char();
				} else if (c == '>') {
					if (peek_char(1) == '>') {
						if (peek_char(2) == '=') {
							type = TK_SHR_EQ;
							eat_char();
						} else {
							type = TK_SHR;
						}
						eat_char();
					} else if (peek_char(1) == '=') {
						type = TK_GT_EQ;
						eat_char();
					} else {
						type = '>';
					}
					eat_char();
				} else if (c == '&') {
					if (peek_char(1) == '=') {
						type = TK_AND_EQ;
						eat_char();
					} else if (peek_char(1) == '&') {
						type = TK_AND_AND;
						eat_char();
					} else {
						type = '&';
					}
					eat_char();
				} else if (c == '|') {
					if (peek_char(1) == '=') {
						type = TK_OR_EQ;
						eat_char();
					} else if (peek_char(1) == '|') {
						type = TK_BAR_BAR;
						eat_char();
					} else {
						type = '|';
					}
					eat_char();
				} else if (c == '!') {
					if (peek_char(1) == '=') {
						type = TK_NOT_EQ;
						eat_char();
					} else {
						type = '!';
					}
					eat_char();
				} else if (c == '^') {
					if (peek_char(1) == '=') {
						type = TK_XOR_EQ;
						eat_char();
					} else {
						type = '^';
					}
					eat_char();
				}
			}
		}
	}

	location.length = col - location.col;
	
	tokens.add(make_token(index, type));
	data.add(tdata);
	locations.add(location);
}

char Lexer::peek_char(int ahead) {
	auto p = pos + ahead;
	if (p < input_len)
		return input[p];
	return '\0';
}

void Lexer::eat_char() {
	auto c = input[pos];
	if (c == '\n') {
		line++;
		col = 0;
		pos++;
		return;
	}
	col++;
	pos++;
}

void Lexer::init_preproc_definitions() {
#if defined(_WIN64)
    preproc_definitions.add("windows");
#elif defined(__APPLE__) || defined(__MACH__)
    preproc_definitions.add("unix");
    preproc_definitions.add("macos");
#elif defined(__linux__)
    preproc_definitions.add("unix");
    preproc_definitions.add("linux");
#endif
}


Token make_token(u32 index, u8 type) {
	return (type << 24) | index;
}

u32 token_index(Token token) {
	return token & 0x00FFFFFF;
}

u8 token_type(Token token) {
	return (token >> 24);
}
