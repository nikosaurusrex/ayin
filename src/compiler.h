#ifndef COMPILER_H_
#define COMPILER_H_

#include <cstdarg>

#include "ast.h"
#include "common.h"
#include "copier.h"
#include "lexer.h"
#include "llvm.h"
#include "typer.h"

struct CompileOptions {
	String input_file;
	String output_file;
	bool optimize = false;
	bool debug = false;
	bool emit_llvm = false;
	bool compile_only = false;

	Array<String> libraries;
	Array<String> linker_paths;
};

struct CompileStatistics {
	int lines_of_code = 0;
	int frontend_time = 0;
	int gen_time = 0;
	int link_time = 0;
};

struct Compiler {
	CompileStatistics statistics;
	CompileOptions *options;
	LLVMConverter *llvm_converter;
	Copier *copier;
	Typer *typer;

	/* default scope */
	Scope *global_scope;

	/* source file management for error reporting */
	Array<String> source_table_files;
	Array<String> source_table_contents;
	Atom_Table atom_table;
	s32 errors_reported = 0;

	/* primitive type definitions */
	TypeInfo *type_string;
	TypeInfo *type_string_data;
	TypeInfo *type_void;
	TypeInfo *type_void_ptr;
	TypeInfo *type_bool;
	TypeInfo *type_s8;
	TypeInfo *type_s16;
	TypeInfo *type_s32;
	TypeInfo *type_s64;
	TypeInfo *type_u8;
	TypeInfo *type_u16;
	TypeInfo *type_u32;
	TypeInfo *type_u64;
	TypeInfo *type_f32;
	TypeInfo *type_f64;

	Atom *atom_main;
	Atom *atom_data;
	Atom *atom_length;
	Atom *atom_capacity;
	Atom *atom_it;
	Atom *atom_it_index;

	Array<String> definitions;

	String stdlib_path;

	Compiler(CompileOptions *options);

	void run();

	void link_program();

	void parse_file(String file_path);

	Atom *make_atom(String name);

	u8 *get_command_line(Array<String> *strings);

	void report_error_base(SourceLocation loc, const char *msg);
	void report_error2(SourceLocation loc1, const char *msg1, SourceLocation loc2, const char *msg2);
	void report_error(SourceLocation location, const char *fmt, va_list args);
	void report_error(SourceLocation location, const char *fmt, ...);
	void report_error(Ast *ast, const char *fmt, ...);

	void init_definitions();
	void handle_directive(Directive *directive);

	String get_line(SourceLocation location);
};

/* helper functions to create types */
TypeInfo *make_int_type(bool is_signed, s32 bytes);
TypeInfo *make_float_type(s32 bytes);
TypeInfo *make_pointer_type(TypeInfo *element_type);

Expression *find_declaration_by_name(Atom *name, Scope *scope);
Expression *find_declaration_by_id(Identifier *id);

String type_to_string(TypeInfo *type);
void type_to_string_builder(TypeInfo *type, StringBuilder *builder);

#endif
