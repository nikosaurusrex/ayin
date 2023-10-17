#ifndef H
#define H

#include "common.h"
#include "lexer.h"

namespace llvm {
	class Value;
};

struct Ast;

struct Scope;
struct Directive;

struct Expression;

struct TypeInfo;

struct Declaration;
struct AFunction;
struct Struct;
struct Enum;
struct TypeAlias;

struct Sizeof;
struct Unary;
struct Binary;
struct Identifier;
struct Literal;
struct Cast;
struct Index;
struct Member;

struct If;
struct While;
struct For;
struct Continue;
struct Break;

struct Return;
struct Defer;

enum AstType {
	AST_SCOPE = 0,
	AST_DECLARATION = 1,
	AST_FUNCTION = 2,
	AST_STRUCT = 3,
	AST_ENUM = 4,
	AST_TYPE_ALIAS = 5,
	AST_IDENTIFIER = 6,
	AST_LITERAL = 7,
	AST_CAST = 8,
	AST_RETURN = 9,
	AST_CALL = 10,
	AST_BINARY = 11,
	AST_UNARY = 12,
	AST_SIZEOF = 13,
	AST_IF = 14,
	AST_WHILE = 15,
	AST_INDEX = 16,
	AST_MEMBER = 17,
	AST_CONTINUE = 18,
	AST_BREAK = 19,
	AST_DIRECTIVE = 20,
	AST_FOR = 21,
	AST_DEFER = 22,
};

struct Ast {
	SourceLocation location;
	int type;
};

struct Expression : Ast {
	Expression *substitution = 0;

	TypeInfo *type_info = 0;
};

/*
* Equivalent of a C++ preprocessor directive
* #include  -> includes local file
* #use      -> includes file from library path
* #if/#else -> static/compile-time if for conditional code compilation
*/
enum DirectiveType {
    DIRECTIVE_INCLUDE,
	DIRECTIVE_USE,
};

struct Directive : Expression {
	int directive_type;
	String file;

	Directive() {
		type = AST_DIRECTIVE;
	}
};

/*
* Represents a scope in the program
* Default scope is the global scope, which has a parent of null
*/
struct Scope : Expression {
	Scope() { 
		type = AST_SCOPE;
	}

	Scope *parent = 0;

	Array<Expression *> declarations;
	Array<Expression *> statements;
	Array<Defer *> defers;
};

/*
* Unresolved -> a non-primitive type name that cannot be resolved during the parsing stage,
*				but is being resolved in the semantic analysis
* 
* Type		 -> type is used in type aliases and marks a generic type
*/
enum BaseTypeInfo {
    TYPE_UNRESOLVED = 0,

    TYPE_STRUCT,
    TYPE_ENUM,
    TYPE_ARRAY,

    TYPE_FUNCTION,

    TYPE_POINTER,
    /* type so it doesnt conflict with stupid Windows.h macro VOID */
    TYPE_VOID_TYPE,
    TYPE_BOOL,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_STRING,

    TYPE_TYPE,
};

struct TypeInfo {
	struct EnumMember {
		Atom *name;
		Literal *value = 0;
		s32 index;
	};

	int type;

	/* element type is used by the pointer and array type */
	TypeInfo *element_type = 0;
	Identifier *unresolved_name;

	/* element count for static arrays */
	s32 array_size = 0;

	Struct *struct_decl;
	Array<TypeInfo *> struct_members;
	Array<EnumMember> enum_members;

	/* function type */
	Array<TypeInfo *> parameters;
	TypeInfo *return_type;

	/* integer type */
	bool is_signed = false;
	/* is dynamic array */
	bool is_dynamic = false;

	bool auto_cast = false;

	/* size in bytes for int and float */
	s32 size = 0;
};

/* Variable declaration flags */
const u8 VAR_GLOBAL = 0x1;
const u8 VAR_CONSTANT = 0x2;

struct Declaration : Expression {
	/* llvm_reference is set in the llvm code generation stage */
	llvm::Value *llvm_reference = 0;
	Identifier *identifier = 0;
	/* Initializer not null for defintions, null for declarations */
	Expression *initializer = 0;
	u8 flags = 0;

	Declaration() {
		type = AST_DECLARATION;
	}
};

/*
* Function declaration flags
* Only external functions can have a variable length parameter
* Extern functions can not be template functions
*/
const u8 FUNCTION_TEMPLATE = 0x1;
const u8 FUNCTION_VARARG = 0x2;
const u8 FUNCTION_FOREIGN = 0x4;

struct AFunction : Expression {
	/* llvm_reference is set in the llvm code generation stage */
	llvm::Value *llvm_reference = 0;
	Identifier *identifier = 0;

	TypeInfo *return_type = 0;

	Scope *parameter_scope = 0;
	Scope *block_scope = 0;
	String linkage_name;

	u8 flags = 0;

	Scope *template_scope = 0;
	Array<AFunction *> polymorphed_overloads;

	AFunction() {
		type = AST_FUNCTION;
	}
};

struct Struct : Expression {
	llvm::Value *llvm_reference = 0;
	Identifier *identifier = 0;
	Array<Declaration *> members;

	Struct() {
		type = AST_STRUCT;
	}
};

struct Enum : Expression {
	Identifier *identifier = 0;

	Enum() {
		type = AST_ENUM;
	}
};

struct TypeAlias : Expression {
	Identifier *identifier = 0;

	TypeAlias() {
		type = AST_TYPE_ALIAS;
	}
};

struct Sizeof : Expression {
	TypeInfo *target_type = 0;

	Sizeof() {
		type = AST_SIZEOF;
	}
};

struct Unary : Expression {
	Expression *target = 0;
	int op;
	bool is_pre = false;

	Unary() {
		type = AST_UNARY;
	}
};

struct Binary : Expression {
	Expression *lhs;
	Expression *rhs;
	int op;

	Binary() {
		type = AST_BINARY;
	}
};

struct Identifier : Expression {
	Atom *atom = 0;
	Scope *scope = 0;

	Identifier() {
		type = AST_IDENTIFIER;
	}
};

struct Literal : Expression {
	enum Literal_Type {
		BOOL,
		FLOAT,
		INT,
		STRING,
		NIL,
		COMPOUND
	};

	Literal_Type literal_type;
	s64 int_value;
	f64 float_value;
	String string_value;

	Array<Expression *> values;
	TypeInfo *compound_type_info;

	Literal() {
		type = AST_LITERAL;
	}
};

struct Cast : Expression {
	Expression *expression = 0;
	TypeInfo *target_type = 0;

	Cast() {
		type = AST_CAST;
	}
};

struct Index : Expression {
	Expression *expression = 0;	
	Expression *index = 0;

	Index() {
		type = AST_INDEX;
	}
};

struct Member : Expression {
	Expression *left = 0;
	Identifier *field = 0;

	s64 field_index = -1;

	Member() {
		type = AST_MEMBER;
	}
};

struct Return : Expression {
	Expression *return_value = 0;

	Return() {
		type = AST_RETURN;
	}
};

struct Defer : Expression {
	Expression *target;

	Defer() {
		type = AST_DEFER;
	}
};

struct Call : Expression {
	Identifier *identifier;
	Array<Expression *> arguments;
	AFunction *resolved_function = 0;

	bool by_function_pointer = false;

	Call() {
		type = AST_CALL;
	}
};

struct If : Expression {
	Expression *condition = 0;
    
    Expression *then_statement = 0;
    Expression *else_statement = 0;

	If() {
		type = AST_IF;
	}
};

struct While : Expression {
    Expression *condition = 0;
    Expression *statement = 0;

    While() {
    	type = AST_WHILE;
	}
};

struct For : Expression {
	Expression *iterator_expr = 0;
	Expression *upper_range_expr = 0;
	
	Declaration *index_declaration = 0;
	Declaration *value_declaration = 0;

	Expression *body = 0;

	For() {
		type = AST_FOR;
	}
};

struct Continue : Expression {
	Continue() {
		type = AST_CONTINUE;
	}
};
struct Break : Expression {
	Break() {
		type = AST_BREAK;
	}
};

inline bool is_enum(TypeInfo *type_info) {
	return type_info->type == TYPE_ENUM;
}

inline bool is_struct(TypeInfo *type_info) {
	return type_info->type == TYPE_STRUCT;
}

inline bool is_array(TypeInfo *type_info) {
	return type_info->type == TYPE_ARRAY;
}

inline bool is_bool(TypeInfo *type_info) {
	return type_info->type == TYPE_BOOL;
}

inline bool is_int(TypeInfo *type_info) {
	return type_info->type == TYPE_INT;
}

inline bool is_float(TypeInfo *type_info) {
	return type_info->type == TYPE_FLOAT;
}

inline bool is_pointer(TypeInfo *type_info) {
	return type_info->type == TYPE_POINTER;
}

inline bool is_function(TypeInfo *type_info) {
	return type_info->type == TYPE_FUNCTION;
}

inline bool is_string(TypeInfo *type_info) {
	return type_info->type == TYPE_STRING;
}

inline bool is_primitive(TypeInfo *type_info) {
	switch (type_info->type) {
		case TYPE_FLOAT:
		case TYPE_INT:
		case TYPE_VOID_TYPE:
		case TYPE_BOOL:
			return true;
		default:
			return false;
	}
}

inline bool binop_is_assign(int op) {
	switch (op) {
		case '=':
		case TK_ADD_EQ:
		case TK_SUB_EQ:
		case TK_MUL_EQ:
		case TK_DIV_EQ:
		case TK_MOD_EQ:
		case TK_SHL_EQ:
		case TK_SHR_EQ:
			return true;
		default:
			return false;
	}
}

inline bool binop_is_logical(int op) {
	return op == TK_AND_AND || op == TK_BAR_BAR;
}

inline bool binop_is_binary(int op) {
    switch (op) {
		case '+':
		case '-':
		case '*':
		case '/':
		case '%':
		case '&':
		case '|':
		case '^':
        case TK_SHL:
        case TK_SHR:
            return true;
		default:
            return false;
    }
}

inline bool binop_is_conditional(int op) {
	switch (op) {
		case TK_EQ_EQ:
		case TK_NOT_EQ:
		case '<':
		case '>':
		case TK_LT_EQ:
		case TK_GT_EQ:
			return true;
		default:
			return false;
	}
}

#endif
