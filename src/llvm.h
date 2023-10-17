#ifndef LLVM_H
#define LLVM_H

namespace llvm {
	class Module;
	class LLVMContext;

	class Constant;
	class Value;
	class Type;
	class Instruction;
	class StructType;
	class Function;
	class DataLayout;
	class Argument;

	class BasicBlock;

	class TargetMachine;
	
    class ConstantFolder;
    class IRBuilderDefaultInserter;

    template<typename T> class ArrayRef;

	template<typename T, typename Inserter> class IRBuilder;

	class DICompileUnit;
	class DIBuilder;
	class DIType;
	class DIFile;
	class DISubprogram;
};

#include "ast.h"

struct LLVMConverter;
struct DebugInfo {
	llvm::DICompileUnit *cu;
	llvm::DIBuilder *db;
	llvm::DIFile *file;
	const llvm::DataLayout *layout;
	llvm::DISubprogram *current_sp;
	Array<llvm::Instruction *> debug_values;

	void init(LLVMConverter *converter, String entry_file);

	void add_function(AFunction *ast_func, llvm::Function *f);
	void add_parameter(Identifier *id, llvm::Value *var, int arg_index, llvm::Argument &arg, llvm::BasicBlock *block);
	void add_variable(Identifier *id, llvm::Value *var, llvm::BasicBlock *block);
	void add_inst(Expression *expr, llvm::Instruction *inst);

	llvm::DIType *convert_type(llvm::Type *type);

	llvm::DIFile *create_file(String file);

	void finalize();
};

struct CompileOptions;
struct Compiler;
struct LLVMConverter {
	Compiler *compiler;
	DebugInfo debug;
	CompileOptions *options;

	llvm::Module *llvm_module;
	llvm::LLVMContext *llvm_context;
	llvm::TargetMachine *target_machine;
	llvm::IRBuilder<llvm::ConstantFolder, llvm::IRBuilderDefaultInserter> *irb;

	llvm::Function *current_function = 0;
	llvm::BasicBlock *current_entry = 0;

	Array<llvm::BasicBlock *> continue_blocks;
	Array<llvm::BasicBlock *> break_blocks;

	llvm::Type *type_void;
	llvm::Type *type_i1;
	llvm::Type *type_i8;
	llvm::Type *type_i16;
	llvm::Type *type_i32;
	llvm::Type *type_i64;
	llvm::Type *type_f32;
	llvm::Type *type_f64;
	llvm::StructType *type_string;

	Scope *current_scope = 0;
	s32 global_constants_count = 0;

	LLVMConverter(Compiler *compiler);

	void convert(Scope *scope);
	void convert_scope(Scope *scope);

	void enter_scope(Scope *scope);
	void exit_scope(Scope *scope);

	void convert_statement(Expression *expression);

	llvm::Value *convert_expression(Expression *expression, bool is_lvalue=false);
	llvm::Value *convert_binary(Binary *binary);

	llvm::Type *convert_type(TypeInfo *type_info);

	void convert_function(AFunction *fun);

	llvm::Value *gen_constant_compound_lit_var(Literal *lit);
	llvm::Constant *gen_constant_compound_lit(Literal *lit);

	void optimize();
	void emit_llvm_ir();
	void emit_object_file();

	llvm::Function *get_or_create_function(AFunction *function); 
	llvm::Value *lalloca(llvm::Type *ty);
	llvm::Value *load(Expression *expr, llvm::Value *value);
	llvm::Value *store(Expression *expr, llvm::Value *value, llvm::Value *ptr);
	llvm::Value *gep(Expression *expr, llvm::Value *ptr, llvm::ArrayRef<llvm::Value *> idx_list);
};

#endif
