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

struct Ast {
	enum Type : u32 {
		SCOPE = 0,
		DECLARATION = 1,
		FUNCTION = 2,
		STRUCT = 3,
		ENUM = 4,
		TYPE_ALIAS = 5,
		IDENTIFIER = 6,
		LITERAL = 7,
		CAST = 8,
		RETURN = 9,
		CALL = 10,
		BINARY = 11,
		UNARY = 12,
		SIZEOF = 13,
		IF = 14,
		WHILE = 15,
		INDEX = 16,
		MEMBER = 17,
		CONTINUE = 18,
		BREAK = 19,
		DIRECTIVE = 20,
		FOR = 21,
	};

	SourceLocation location;
	Type type;
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
struct Directive : Expression {

	enum Directive_Type : u32 {
		INCLUDE,
		USE,
	};

	Directive_Type directive_type;
	String file;

	Directive() {
		type = Ast::DIRECTIVE;
	}
};

/*
* Represents a scope in the program
* Default scope is the global scope, which has a parent of null
*/
struct Scope : Expression {
	Scope() { 
		type = Ast::SCOPE;
	}

	Scope *parent = 0;

	Array<Expression *> declarations;
	Array<Expression *> statements;
};

struct TypeInfo {

	/*
	* Unresolved -> a non-primitive type name that cannot be resolved during the parsing stage,
	*				but is being resolved in the semantic analysis
	* 
	* Type		 -> type is used in type aliases and marks a generic type
	*/
	enum BaseType : u32 {
		UNRESOLVED = 0,

		STRUCT,
		ENUM,
		ARRAY,

		FUNCTION,

		POINTER,
		/* type so it doesnt conflict with stupid Windows.h macro VOID */
		VOID_TYPE,
		BOOL,
		INT,
		FLOAT,
		STRING,

		TYPE,
	};

	struct EnumMember {
		Atom *name;
		Literal *value = 0;
		s32 index;
	};

	BaseType type;

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
		type = Ast::DECLARATION;
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
		type = Ast::FUNCTION;
	}
};

struct Struct : Expression {
	llvm::Value *llvm_reference = 0;
	Identifier *identifier = 0;
	Array<Declaration *> members;

	Struct() {
		type = Ast::STRUCT;
	}
};

struct Enum : Expression {
	Identifier *identifier = 0;

	Enum() {
		type = Ast::ENUM;
	}
};

struct TypeAlias : Expression {
	Identifier *identifier = 0;

	TypeAlias() {
		type = Ast::TYPE_ALIAS;
	}
};

struct Sizeof : Expression {
	TypeInfo *target_type = 0;

	Sizeof() {
		type = Ast::SIZEOF;
	}
};

struct Unary : Expression {
	Expression *target = 0;
	int op;
	bool is_pre = false;

	Unary() {
		type = Ast::UNARY;
	}
};

struct Binary : Expression {
	Expression *lhs;
	Expression *rhs;
	int op;

	Binary() {
		type = Ast::BINARY;
	}
};

struct Identifier : Expression {
	Atom *atom = 0;
	Scope *scope = 0;

	Identifier() {
		type = Ast::IDENTIFIER;
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
		type = Ast::LITERAL;
	}
};

struct Cast : Expression {
	Expression *expression = 0;
	TypeInfo *target_type = 0;

	Cast() {
		type = Ast::CAST;
	}
};

struct Index : Expression {
	Expression *expression = 0;	
	Expression *index = 0;

	Index() {
		type = Ast::INDEX;
	}
};

struct Member : Expression {
	Expression *left = 0;
	Identifier *field = 0;

	s64 field_index = -1;

	Member() {
		type = Ast::MEMBER;
	}
};

struct Return : Expression {
	Expression *return_value = 0;

	Return() {
		type = Ast::RETURN;
	}
};

struct Call : Expression {
	Identifier *identifier;
	Array<Expression *> arguments;
	AFunction *resolved_function = 0;

	bool by_function_pointer = false;

	Call() {
		type = Ast::CALL;
	}
};

struct If : Expression {
	Expression *condition = 0;
    
    Expression *then_statement = 0;
    Expression *else_statement = 0;

	If() {
		type = Ast::IF;
	}
};

struct While : Expression {
    Expression *condition = 0;
    Expression *statement = 0;

    While() {
    	type = Ast::WHILE;
	}
};

struct For : Expression {
	Declaration *iterator_decl = 0;
	Declaration *iterator_index_decl = 0;

	Expression *initial_iterator_expression = 0;
	Expression *upper_range_expression = 0;

	Scope *iterator_declaration_scope = 0;
	Expression *body = 0;

	For() {
		type = Ast::FOR;
	}
};

struct Continue : Expression {
	Continue() {
		type = Ast::CONTINUE;
	}
};
struct Break : Expression {
	Break() {
		type = Ast::BREAK;
	}
};

inline bool type_is_enum(TypeInfo *type_info) {
	return type_info->type == TypeInfo::ENUM;
}

inline bool type_is_struct(TypeInfo *type_info) {
	return type_info->type == TypeInfo::STRUCT;
}

inline bool type_is_array(TypeInfo *type_info) {
	return type_info->type == TypeInfo::ARRAY;
}

inline bool type_is_bool(TypeInfo *type_info) {
	return type_info->type == TypeInfo::BOOL;
}

inline bool type_is_int(TypeInfo *type_info) {
	return type_info->type == TypeInfo::INT;
}

inline bool type_is_float(TypeInfo *type_info) {
	return type_info->type == TypeInfo::FLOAT;
}

inline bool type_is_pointer(TypeInfo *type_info) {
	return type_info->type == TypeInfo::POINTER;
}

inline bool type_is_function(TypeInfo *type_info) {
	return type_info->type == TypeInfo::FUNCTION;
}

inline bool type_is_string(TypeInfo *type_info) {
	return type_info->type == TypeInfo::STRING;
}

inline bool type_is_primitive(TypeInfo *type_info) {
	switch (type_info->type) {
		case TypeInfo::FLOAT:
		case TypeInfo::INT:
		case TypeInfo::VOID_TYPE:
		case TypeInfo::BOOL:
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
