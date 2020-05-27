#pragma once

#define SAFEALLOC(var,Type) if((var=(Type*)malloc(sizeof(Type)))==NULL)err("not enough memory");

typedef struct Token {
	int code; // codul (numele)
	union {
		char* text; // folosit pentru ID, CT_STRING (alocat dinamic)
		long int i; // folosit pentru CT_INT, CT_CHAR
		double r; // folosit pentru CT_REAL
	};
	int line; // linia din fisierul de intrare
	struct Token* next; // inlantuire la urmatorul AL
}Token;

enum atoms {
	ID, INT, BREAK, CHAR, DOUBLE, ELSE, FOR, IF, STRUCT, VOID, WHILE, RETURN, CT_INT, CT_REAL, CT_CHAR, CT_STRING, ADD,
	SUB, DIV, MUL, DOT, COMMA, SEMICOLON, AND, OR, NOT, LPAR, RPAR, LBRAKET, RBRAKET, LACC, RACC, ASSIGN,
	EQUALS, NOTEQ, LESS, LESSEQ, GREATER, GREATEREQ, END
};

extern char* pch; // iterator pentru cifre/litere
extern Token* tokens;/*inceputul listei de tokeni*/


/*
Sintactic structures
*/

struct Token* crtTk; // tokenul curent din lista de tokeni.
struct Token* consumedTk;// tokenul care a tocmai a fost consumat de consume

struct _Symbol;
typedef struct _Symbol Symbol;

typedef struct {
	Symbol** begin; // the beginning of the symbols, or NULL
	Symbol** end; // the position after the last symbol
	Symbol** after; // the position after the allocated space
}Symbols;

enum { CLS_VAR, CLS_FUNC, CLS_EXTFUNC, CLS_STRUCT };
enum { MEM_GLOBAL, MEM_ARG, MEM_LOCAL };
enum { TB_INT, TB_DOUBLE, TB_CHAR, TB_STRUCT, TB_VOID };

typedef struct {
	int typeBase; // TB_*
	Symbol* s; // struct definition for TB_STRUCT
	int nElements; // >0 array of given size, 0=array without size, <0 non array
}Type;

typedef struct _Symbol {
	const char* name; // a reference to the name stored in a token
	int cls; // CLS_*
	int mem; // MEM_*
	Type type;
	int depth; // 0-global, 1-in function, 2... - nested blocks in function
	union {
		Symbols args; // used only of functions
		Symbols members; // used only for structs
	};
	union {
		void* addr; // vm: the memory address for global symbols
		int offset; // vm: the stack offset for local symbols
	};
}Symbol;


//type analysis
typedef union {
	long int i; // int, char
	double d; // double
	const char* str; // char[]
}CtVal;
//type analysis
typedef struct {
	Type type; // type of the result
	int isLVal; // if it is a LVal
	int isCtVal; // if it is a constant value (int, real, char, char[])
	CtVal ctVal; // the constat value
}RetVal;

extern Symbols symbols;
extern int sizeArgs, offset;

/*
END of sintactic structures
*/

/*
Sintactic function definitions
*/
void tkerr(const Token* tk, const char* fmt, ...);
Type createType(int typeBase, int nElements);
void cast(Type* dst, Type* src);
Type getArithType(Type* s1, Type* s2);
Symbol* addSymbol(Symbols* symbols, const char* name, int cls);
Symbol* addExtFunc(const char* name, Type type, void* addr);
Symbol* findSymbol(Symbols* symbols, const char* name);
void deleteSymbolsAfter(Symbols* symbols, Symbol* start);
void addVar(Token* tkName, Type* t);
void printSymbols(Symbols* symbols);
void addPredefinedFunctions();
int typeFullSize(Type* type);
int typeArgSize(Type* type);

/*
END of sintactic function definitions
*/

/*
Lexical function definitions
*/
void err(const char* fmt, ...);
const char* codeName(int code);
void printAtom();
Token* addTk(int code);
char* createString(char* start, char* pch);
char createChar(char* start, char* pch);
int createInt(char* intStart, char* pch);
double createDouble(char* start, char* pch);
int getNextToken();
/*
END Lexical function definitions
*/

int unit();
void addPredefinedFunctions();


/*
 VM Structures
*/

enum {
	O_ADD_C, O_ADD_D, O_ADD_I, O_CALL, O_CALLEXT, O_CAST_I_D, O_CAST_C_I,
	O_CAST_C_D, O_CAST_I_C, O_DROP,	O_ENTER, O_EQ_D, O_HALT, O_INSERT, 
	O_JT_I, O_LOAD, O_OFFSET,	O_PUSHFPADDR, O_PUSHCT_A, O_RET,
	O_STORE, O_SUB_D, O_PUSHCT_I, O_SUB_I, O_CAST_D_C, O_CAST_D_I,
	O_JF_A, O_JF_C, O_JF_D, O_JF_I, O_JMP, O_NOP, O_OR_A, O_OR_I,
	O_OR_D, O_OR_C, O_AND_A, O_AND_I, O_AND_D, O_AND_C, O_EQ_A,
	O_NOTEQ_A, O_EQ_I, O_EQ_C, O_NOTEQ_I, O_NOTEQ_D, O_NOTEQ_C,
	O_LESS_I, O_LESS_D, O_LESS_C, O_LESSEQ_I, O_LESSEQ_D, O_LESSEQ_C,
	O_GREATER_I, O_GREATER_D, O_GREATER_C, O_GREATEREQ_I, O_GREATEREQ_D,
	O_GREATEREQ_C, O_SUB_C, O_MUL_I, O_MUL_D, O_MUL_C, O_DIV_I, O_DIV_D,
	O_DIV_C, O_NEG_C, O_NEG_I, O_NEG_D, O_NOT_C, O_NOT_I, O_NOT_D, O_NOT_A,
	O_PUSHCT_D, O_PUSHCT_C, 
}; // all opcodes; each one starts with O_

typedef struct _Instr {
	int opcode; // O_*
	union {
		int i; // int, char
		double d;
		void* addr;
	}args[2];
	struct _Instr* last, * next; // links to last, next instructions
}Instr;

extern Instr* instructions, * lastInstruction; // double linked list
extern Instr* crtLoopEnd;

/*
	END of VM structures
*/

/*
	VM FUNCTIONS
*/

void mvTest();
Instr* createInstr(int opcode);
void insertInstrAfter(Instr* after, Instr* i);
Instr* addInstr(int opcode);
Instr* addInstrAfter(Instr* after, int opcode);
Instr* addInstrA(int opcode, void* addr);
Instr* addInstrI(int opcode, int val);
Instr* addInstrII(int opcode, int val1, int val2);
void deleteInstructionsAfter(Instr* start);
void* allocGlobal(int size);
void run(Instr* IP);

/*
	END OF VM FUNCTIONS
*/