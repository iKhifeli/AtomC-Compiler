
/************************************************************
ANALIZOR SINTACTIC + ANALIZOR DE TIPURI + ANALIZOR DE DOMENII
by GELU POPA
************************************************************/

#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>

#include "Header.h"

Symbols symbols;
int sizeArgs, offset;
Instr* crtLoopEnd;
extern Instr* instructions, * lastInstruction; // double linked list

int crtDepth = 0; // adancimea contextului
Symbol* crtFunc = NULL; // pointer la simbolul functiei daca in functie, altfel null
Symbol* crtStruct = NULL; // pointer la simbolul structurii daca in structura, altfel null

Instr* getRVal(RetVal* rv) {
	if (rv->isLVal) {
		switch (rv->type.typeBase) {
		case TB_INT:
		case TB_DOUBLE:
		case TB_CHAR:
		case TB_STRUCT:
			addInstrI(O_LOAD, typeArgSize(&rv->type));
			break;
		default:tkerr(crtTk, "unhandled type: %d", rv->type.typeBase);
		}
	}
	return lastInstruction;
}

Instr* createCondJmp(RetVal* rv)
{
	if (rv->type.nElements >= 0) {  // arrays
		return addInstr(O_JF_A);
	}
	else {                              // non-arrays
		getRVal(rv);
		switch (rv->type.typeBase) {
		case TB_CHAR:return addInstr(O_JF_C);
		case TB_DOUBLE:return addInstr(O_JF_D);
		case TB_INT:return addInstr(O_JF_I);
		default:return NULL;
		}
	}
}

Instr* appendInstr(Instr* i) { ///////////////////////////////////////////////////////// NOT SURE
	instructions = lastInstruction;
	lastInstruction = i;
	return lastInstruction;
}

void addCastInstr(Instr* after, Type* actualType, Type* neededType)
{
	if (actualType->nElements >= 0 || neededType->nElements >= 0)return;
	switch (actualType->typeBase) {
	case TB_CHAR:
		switch (neededType->typeBase) {
		case TB_CHAR:break;
		case TB_INT:addInstrAfter(after, O_CAST_C_I); break;
		case TB_DOUBLE:addInstrAfter(after, O_CAST_C_D); break;
		}
		break;
	case TB_INT:
		switch (neededType->typeBase) {
		case TB_CHAR:addInstrAfter(after, O_CAST_I_C); break;
		case TB_INT:break;
		case TB_DOUBLE:addInstrAfter(after, O_CAST_I_D); break;
		}
		break;
	case TB_DOUBLE:
		switch (neededType->typeBase) {
		case TB_CHAR:addInstrAfter(after, O_CAST_D_C); break;
		case TB_INT:addInstrAfter(after, O_CAST_D_I); break;
		case TB_DOUBLE:break;
		}
		break;
	}
}

int typeBaseSize(Type* type)
{
	int size = 0;
	Symbol** is;
	switch (type->typeBase) {
	case TB_INT:size = sizeof(long int); break;
	case TB_DOUBLE:size = sizeof(double); break;
	case TB_CHAR:size = sizeof(char); break;
	case TB_STRUCT:
		for (is = type->s->members.begin; is != type->s->members.end; is++) {
			size += typeFullSize(&(*is)->type);
		}
		break;
	case TB_VOID:size = 0; break;
	default:err("invalid typeBase: %d", type->typeBase);
	}
	return size;

}

int typeFullSize(Type* type)
{
	return typeBaseSize(type) * (type->nElements > 0 ? type->nElements : 1);
}

int typeArgSize(Type* type)
{
	if (type->nElements >= 0)return sizeof(void*);
	return typeBaseSize(type);
}

void initSymbols(Symbols* symbols)
{
	symbols->begin = NULL;
	symbols->end = NULL;
	symbols->after = NULL;
}

void tkerr(const Token* tk, const char* fmt, ...)
{
	va_list va;
	va_start(va, fmt);
	fprintf(stderr, "error in line %d: ", tk->line);
	vfprintf(stderr, fmt, va);
	fputc('\n', stderr);
	va_end(va);
	exit(-1);
}

//type analysis
Type createType(int typeBase, int nElements) {
	Type t;
	t.typeBase = typeBase;
	t.nElements = nElements;
	return t;
}
//type analysis
void cast(Type* dst, Type* src)
{
	if (src->nElements > -1) {
		if (dst->nElements > -1) {
			if (src->typeBase != dst->typeBase)
				tkerr(crtTk, "an array cannot be converted to an array of another type");
		}
		else {
			tkerr(crtTk, "an array cannot be converted to a non-array");
		}
	}
	else {
		if (dst->nElements > -1) {
			tkerr(crtTk, "a non-array cannot be converted to an array");
		}
	}
	switch (src->typeBase) {
	case TB_CHAR:
	case TB_INT:
	case TB_DOUBLE:
		switch (dst->typeBase) {
		case TB_CHAR:
		case TB_INT:
		case TB_DOUBLE:
			return;
		}
	case TB_STRUCT:
		if (dst->typeBase == TB_STRUCT) {
			if (src->s != dst->s)
				tkerr(crtTk, "a structure cannot be converted to another one");
			return;
		}
	}
	tkerr(crtTk, "incompatible types");
}
//type analysis
Type getArithType(Type* s1, Type* s2)
{
	if (s1->typeBase == TB_DOUBLE) {
		return *s1;
	}
	else if (s2->typeBase == TB_DOUBLE) {
		return *s2;
	}
	else if (s1->typeBase == TB_INT && s2->typeBase == TB_INT) {
		return *s1;
	}
	else if (s1->typeBase == TB_CHAR && s2->typeBase == TB_INT) {
		return *s2;
	}
	else if (s2->typeBase == TB_CHAR && s1->typeBase == TB_INT) {
		return *s1;
	}
	else if (s1->typeBase == TB_CHAR || s2->typeBase == TB_CHAR) {
		return *s1;
	}
	else {
		printf("\n!! TIPURI COMPATIBILE : CHAR/INT/DOUBLE !!\n");
		exit(-123);
	}
}

Symbol* addSymbol(Symbols* symbols, const char* name, int cls) {
	Symbol* s;
	if (symbols->end == symbols->after) { // create more room
		int count = symbols->after - symbols->begin;
		int n = count * 2; // double the room
		if (n == 0)n = 1; // needed for the initial case
		symbols->begin = (Symbol**)realloc(symbols->begin, n * sizeof(Symbol*));
		if (symbols->begin == NULL)err("not enough memory");
		symbols->end = symbols->begin + count;
		symbols->after = symbols->begin + n;
	}
	SAFEALLOC(s, Symbol);
	*symbols->end++ = s;
	s->name = name;
	s->cls = cls;
	s->depth = crtDepth;
	return s;
}
//type analysis
Symbol* addExtFunc(const char* name, Type type, void* addr)
{
	Symbol* s = addSymbol(&symbols, name, CLS_EXTFUNC);
	s->type = type;
	s->addr = addr;
	initSymbols(&s->args);
	return s;
}
//type analysis

Symbol* findSymbol(Symbols* symbols, const char* name) {
	if (symbols->begin == NULL) {
		printf("Tabela de simboluri goala!\n");
		return NULL;
	}
	for (Symbol** aux = symbols->end - 1; symbols->begin <= aux; aux--) {
		if (!strcmp((*aux)->name, name)) {
			return *aux;
		}
	}
	return NULL;
}

void deleteSymbolsAfter(Symbols* symbols, Symbol* start) {
	if (symbols->begin == NULL) {
		printf("\nLista de simboluri este goala!\n");
	}
	if (findSymbol(symbols, start->name) == NULL) {
		printf("\nNU exista simbolul %s in lista de simboluri!\n", start->name);
		return;
	}
	Symbol** aux = NULL; // pentru a parcurge lista de simboluri
	Symbol** foo = NULL; // pentru a salva adresa de memorie a simbolului pe care il cautam
	for (aux = symbols->begin; symbols->end > aux; aux++) { // parcurgem lista de simboluri
		if (!strcmp((*aux)->name, start->name)) { // gasim simbolul care ne intereseaza
			break; // iesim
		}
	}
	aux++; // vrem sa stergem tot ce este dupa symbolul cautat, deci trecem la urmatoarea adresa din memorie
	foo = aux; // salvam in foo adresa care dorim sa devina adresa de dupa ultimul simbol din lista
	//adica adresa de dupa simbolul pe care il cautam.
	while (aux < symbols->end) { // parcugem symbolurile de dupa simbolul cautat si le setam pe null
		*aux = NULL;
		aux++;
	}
	symbols->end = foo; // setam ca adresa de sfarsit de simboluri, adresa de dupa simbolul cautat.
}

void addVar(Token* tkName, Type* t) {
	Symbol* s;
	if (crtStruct) {
		if (findSymbol(&crtStruct->members, tkName->text))
			tkerr(crtTk, "symbol redefinition: %s", tkName->text);
		s = addSymbol(&crtStruct->members, tkName->text, CLS_VAR);
	}
	else if (crtFunc) {
		s = findSymbol(&symbols, tkName->text);
		if (s && s->depth == crtDepth)
			tkerr(crtTk, "symbol redefinition: %s", tkName->text);
		s = addSymbol(&symbols, tkName->text, CLS_VAR);
		s->mem = MEM_LOCAL;
	}
	else {
		if (findSymbol(&symbols, tkName->text))
			tkerr(crtTk, "symbol redefinition: %s", tkName->text);
		s = addSymbol(&symbols, tkName->text, CLS_VAR);
		s->mem = MEM_GLOBAL;
	}
	s->type = *t;
	if (crtStruct || crtFunc) {
		s->offset = offset;
	}
	else {
		s->addr = allocGlobal(typeFullSize(&s->type));
	}
	offset += typeFullSize(&s->type);
}

void printSymbols(Symbols* symbols) {
	if (symbols->begin == NULL) {
		printf("Tabela de simboluri goala!");
		return;
	}
	for (Symbol** aux = symbols->begin; symbols->end > aux; aux++) {
		Symbol* check = *aux;
		printf("%s \n", check->name);
	}
}

/////////////////////////////////////////////


int unit(); int declVar(); int declStruct(); int typeBase(Type* ret); int arrayDecl(Type* ret); int typeName(Type* ret); int declFunc();
int funcArg(); int stm(); int stmCompound(); int expr(RetVal* rv); int exprAssign(RetVal* rv); int exprOr(RetVal* rv); 
int exprOrPrim(RetVal* rv); int exprAnd(RetVal* rv); int exprAndPrim(RetVal* rv);int exprEq(RetVal* rv); int exprEqPrim(RetVal* rv);
int exprRel(RetVal* rv); int exprRelPrim(RetVal* rv); int exprAdd(RetVal* rv); int exprAddPrim(RetVal* rv); 
int exprMul(RetVal* rv); int exprMulPrim(RetVal* rv); int exprCast(RetVal* rv); int exprUnary(RetVal* rv); int exprPostFix(RetVal* rv); 
int exprPostFixPrim(RetVal* rv); int exprPrimary(RetVal* rv);

int consume(int code) {
	const char* code_name = codeName(code);	printf("consume(%s)", code_name);
	if (crtTk->code == code) {
		consumedTk = crtTk;
		crtTk = crtTk->next;
		printf(" -> (%s) consumed!\n", code_name);
		return 1;
	}
	printf("\naltceva(%s)\n", code_name);
	return 0;
}
//unit: ( declStruct | declFunc | declVar )* END ;
int unit() {
	Token* start = crtTk;
	printf("@unit (%s)\n", codeName(crtTk->code));
	Instr* labelMain = addInstr(O_CALL);
	addInstr(O_HALT);
	for (;;) {
		if (declStruct()) {}
		else if (declFunc()) {}
		else if (declVar()) {}
		else {
			Symbol* m = findSymbol(&symbols, "main");
			if (m != NULL) {
				labelMain->args[0].addr = m->addr;
			}
			else {
				tkerr(crtTk, " Functia main lipseste!\n");
			}
			break;
		}
	}
	if (consume(END)) {
		return 1;
	}
	crtTk = start;
	return 0;
}
//declVar:  typeBase ID arrayDecl ? (COMMA ID arrayDecl ? ) * SEMICOLON;
// ADAUGAT ERORI
int declVar() {
	printf("@declVar (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	Type t;
	Token* tkName;
	if (typeBase(&t)) {
		if (consume(ID)) {
			tkName = consumedTk;
			if (!arrayDecl(&t)) {
				t.nElements = -1;
			}
			addVar(tkName, &t);
			for (;;) {
				if (consume(COMMA)) {
					if (consume(ID)) {
						tkName = consumedTk;
						if (!arrayDecl(&t)) {
							t.nElements = -1;
						}
						addVar(tkName, &t);
					}
					else tkerr(crtTk, "!! Lipsa nume variabila dupa ',' !!\n");
				}
				else break;
			}
			if (consume(SEMICOLON)) {
				return 1;
			}
			else {
				tkerr(crtTk, "!!! Lipsa ';' dupa declararea variabilei!!\n");
			}
		}
		else tkerr(crtTk, "!!!Lipsa ID variabila dupa declararea tipului!!!\n");
	}
	crtTk = start;
	return 0;
}
//declStruct: STRUCT ID LACC declVar* RACC SEMICOLON; 
int declStruct() {
	printf("@declStruct (%s)\n", codeName(crtTk->code));
	Token* start = crtTk; // This will help us come back where we started if something not matches
	Token* tkName;
	if (consume(STRUCT)) {
		if (consume(ID)) {
			tkName = consumedTk;
			if (consume(LACC)) {
				offset = 0;
				if (findSymbol(&symbols, tkName->text))
					tkerr(crtTk, "symbol redefinition: %s", tkName->text);
				crtStruct = addSymbol(&symbols, tkName->text, CLS_STRUCT);
				initSymbols(&crtStruct->members);
				while (declVar()) {}
				if (consume(RACC)) {
					if (consume(SEMICOLON)) {
						crtStruct = NULL;
						return 1;
					}
				}
			}
		}
	}
	crtTk = start;
	return 0;
}
//typeBase: INT | DOUBLE | CHAR | STRUCT ID ;
int typeBase(Type* ret) {
	printf("@typeBase (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	if (consume(INT)) {
		ret->typeBase = TB_INT;
		return 1;
	}
	else {
		if (consume(DOUBLE)) {
			ret->typeBase = TB_DOUBLE;
			return 1;
		}
		else {
			if (consume(CHAR)) {
				ret->typeBase = TB_CHAR;
				return 1;
			}
			else {
				if (consume(STRUCT)) {
					if (consume(ID)) {
						Token* tkName = consumedTk;
						Symbol* s = findSymbol(&symbols, tkName->text);
						if (s == NULL)tkerr(crtTk, "undefined symbol: %s", tkName->text);
						if (s->cls != CLS_STRUCT)tkerr(crtTk, "%s is not a struct", tkName->text);
						ret->typeBase = TB_STRUCT;
						ret->s = s;
						return 1;
					}
				}
			}
		}
	}
	crtTk = start;
	return 0;
}
//arrayDecl: LBRACKET expr? RBRACKET ;
// ERORI ADAUGATE
int arrayDecl(Type* ret) {
	printf("@arrayDecl (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	RetVal rv;
	Instr* instrBeforeExpr;
	if (consume(LBRAKET)) {
		instrBeforeExpr = lastInstruction;
		if (expr(&rv)) {
			if (!rv.isCtVal)tkerr(crtTk, "the array size is not a constant");
			if (rv.type.typeBase != TB_INT)tkerr(crtTk, "the array size is not an integer");
			ret->nElements = rv.ctVal.i;
		}
		else {
			deleteInstructionsAfter(instrBeforeExpr);
			ret->nElements = 0;
		}
		if (consume(RBRAKET)) {
			return 1;
		}
		else tkerr(crtTk, "!!! Lipsa ']' dupa declararea array-ului!!!\n");
	}
	crtTk = start;
	return 0;
}
//typeName: typeBase arrayDecl? ;
int typeName(Type* ret) {
	printf("@typeName (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	if (typeBase(ret)) {
		if (arrayDecl(ret)) {}
		else ret->nElements = -1;
		return 1;
	}
	crtTk = start;
	return 0;
}
//declFunc: ( typeBase MUL? | VOID ) ID LPAR (funcArg(COMMA funcArg)*) ? RPAR stmCompound;
int declFunc() {
	printf("@declFunc (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	Token* tkName;
	int typeBaseOk = 0;
	Type t;
	Symbol** ps;
	if (typeBase(&t)) {
		typeBaseOk = 1;
		if (consume(MUL)) t.nElements = 0;
		else t.nElements = -1;
	}
	int voidConsumed = 0;
	if ((voidConsumed = consume(VOID)) || typeBaseOk == 1) {
		if (voidConsumed)t.typeBase = TB_VOID;
		if (consume(ID)) {
			sizeArgs = offset = 0;
			tkName = consumedTk;
			if (consume(LPAR)) {
				if (findSymbol(&symbols, tkName->text))
					tkerr(crtTk, "symbol redefinition: %s", tkName->text);
				crtFunc = addSymbol(&symbols, tkName->text, CLS_FUNC);
				initSymbols(&crtFunc->args);
				crtFunc->type = t;
				crtDepth++;
				funcArg();//
				while (consume(COMMA) && funcArg()) {}
				if (consume(RPAR)) {
					crtDepth--;
					crtFunc->addr = addInstr(O_ENTER);
					sizeArgs = offset;
					//update args offsets for correct FP indexing
					for (ps = symbols.begin; ps != symbols.end; ps++) {
						if ((*ps)->mem == MEM_ARG) {
							//2*sizeof(void*) == sizeof(retAddr)+sizeof(FP)
							(*ps)->offset -= sizeArgs + 2 * sizeof(void*);
						}
					}
					offset = 0;
					if (stmCompound()) {
						deleteSymbolsAfter(&symbols, crtFunc);
						((Instr*)crtFunc->addr)->args[0].i = offset;  // setup the ENTER argument 
						if (crtFunc->type.typeBase == TB_VOID) {
							addInstrII(O_RET, sizeArgs, 0);
						}
						crtFunc = NULL;
						return 1;
					}
				}
				crtFunc->addr = addInstr(O_ENTER);
				sizeArgs = offset;
				//update args offsets for correct FP indexing
				for (ps = symbols.begin; ps != symbols.end; ps++) {
					if ((*ps)->mem == MEM_ARG) {
						//2*sizeof(void*) == sizeof(retAddr)+sizeof(FP)
						(*ps)->offset -= sizeArgs + 2 * sizeof(void*);
					}
				}
				offset = 0;
			}
		}
	}
	crtTk = start;
	return 0;
}
//funcArg: typeBase ID arrayDecl? ;
int funcArg() {
	printf("@funcArg (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	Type t;
	Token* tkName;
	if (typeBase(&t)) {
		if (consume(ID)) {
			tkName = consumedTk;
			if (!arrayDecl(&t))t.nElements = -1;
			Symbol* s = addSymbol(&symbols, tkName->text, CLS_VAR);
			s->mem = MEM_ARG;
			s->type = t;
			s = addSymbol(&crtFunc->args, tkName->text, CLS_VAR);
			s->mem = MEM_ARG;
			s->type = t;
			s->offset = offset;
			//only once at the end, after "offset" is used and "s->type" is set
			offset += typeArgSize(&s->type);
			return 1;
		}
	}
	crtTk = start;
	return 0;
}
/*
stm: stmCompound
		   | IF LPAR expr RPAR stm ( ELSE stm )?
		   | WHILE LPAR expr RPAR stm
		   | FOR LPAR expr? SEMICOLON expr? SEMICOLON expr? RPAR stm
		   | BREAK SEMICOLON
		   | RETURN expr? SEMICOLON
		   | expr? SEMICOLON ;
	ADAUGAT ERORI
*/
int stm() {
	printf("@stm (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	RetVal rv;
	Instr* i,*i1,*i2,*i3,*i4,*is,*ib3,*ibs;
	if (stmCompound()) {
		return 1;
	}
	else {
		if (consume(IF)) {
			if (consume(LPAR)) {
				if (expr(&rv)) {
					if (rv.type.typeBase == TB_STRUCT)
						tkerr(crtTk, "a structure cannot be logically tested");
					if (consume(RPAR)) {
						i1 = createCondJmp(&rv);
						if (stm()) {
							if (consume(ELSE)) {
								i2 = addInstr(O_JMP);
								if (stm()) {
									i1->args[0].addr = i2->next;
									i1 = i2;
								}
							}
							i1->args[0].addr = addInstr(O_NOP);
							return 1;
						}
						else tkerr(crtTk, "!!Lipsa corp IF!!\n");
					}
					else tkerr(crtTk, "!!Lipsa ')' dupa expresia din IF!!\n");
				}
				else tkerr(crtTk, "!!Lipsa expresie din IF, sau expresie invalida!!\n");
			}
			else tkerr(crtTk, "!!Lipsa '(' dupa IF !!\n");
		}
		else {
			if (consume(WHILE)) {
				Instr* oldLoopEnd = crtLoopEnd;
				crtLoopEnd = createInstr(O_NOP);
				i1 = lastInstruction;
				if (consume(LPAR)) {
					if (expr(&rv)) {
						if (rv.type.typeBase == TB_STRUCT)
							tkerr(crtTk, "a structure cannot be logically tested");
						if (consume(RPAR)) {
							i2 = createCondJmp(&rv);
							if (stm()) {
								addInstrA(O_JMP, i1->next);
								appendInstr(crtLoopEnd);
								i2->args[0].addr = crtLoopEnd;
								crtLoopEnd = oldLoopEnd;
								return 1;
							}
							else tkerr(crtTk, "!!Lipsa corp WHILE!!\n");
						}
						else tkerr(crtTk, "!!Lipsa ')' dupa expresia din while!!\n");
					}
					else tkerr(crtTk, "!!Expresie din while invalida sau lipsa expresie!!\n");
				}
				else tkerr(crtTk, "!!Lipsa '(' dupa while!!\n");
			}
			else {
				if (consume(FOR)) {
					RetVal rv1, rv2, rv3;
					Instr* oldLoopEnd = crtLoopEnd;
					crtLoopEnd = createInstr(O_NOP);
					if (consume(LPAR)) {
						if (expr(&rv1)) {
							if (typeArgSize(&rv1.type))
								addInstrI(O_DROP, typeArgSize(&rv1.type));
						}
						if (consume(SEMICOLON)) {
							i2 = lastInstruction; /* i2 is before rv2 */
							if (expr(&rv2)) {
								i4 = createCondJmp(&rv2);
							}
							else i4 = NULL;
							if (rv2.type.typeBase == TB_STRUCT)
								tkerr(crtTk, "a structure cannot be logically tested");
							if (consume(SEMICOLON)) {
								ib3 = lastInstruction;
								if (expr(&rv3)) {
									if (typeArgSize(&rv3.type))
										addInstrI(O_DROP, typeArgSize(&rv3.type));
								}
								if (consume(RPAR)) {
									ibs = lastInstruction;
									if (stm()) {
										if (ib3 != ibs) {
											i3 = ib3->next;
											is = ibs->next;
											ib3->next = is;
											is->last = ib3;
											lastInstruction->next = i3;
											i3->last = lastInstruction;
											ibs->next = NULL;
											lastInstruction = ibs;
										}
										addInstrA(O_JMP, i2->next);
										appendInstr(crtLoopEnd);
										if (i4!=NULL)i4->args[0].addr = crtLoopEnd;
										crtLoopEnd = oldLoopEnd;

										return 1;
									}
									else tkerr(crtTk, "!!Lipsa corp FOR!!\n");
								}
								else tkerr(crtTk, "!!Lipsa ')' dupa expresia din FOR!!\n");
							}
							else tkerr(crtTk, "!!Lipsa ';' din FOR!!\n");
						}
						else tkerr(crtTk, "!!Lipsa ';' din FOR!!\n");
					}
					else tkerr(crtTk, "!!Lipsa '(' dupa FOR!!\n");
				}
				else {
					if (consume(BREAK)) {
						if (consume(SEMICOLON)) {
							if (!crtLoopEnd)tkerr(crtTk, "break without for or while");
							addInstrA(O_JMP, crtLoopEnd);
							return 1;
						}
						else tkerr(crtTk, "!!Lipsa ';' dupa BREAK!!\n");
					}
					else {
						if (consume(RETURN)) {
							if (expr(&rv)) {
								i = getRVal(&rv);
								addCastInstr(i, &rv.type, &crtFunc->type);
							}
							if (crtFunc->type.typeBase == TB_VOID)
								tkerr(crtTk, "a void function cannot return a value");
							cast(&crtFunc->type, &rv.type);
							if (consume(SEMICOLON)) {
								if (crtFunc->type.typeBase == TB_VOID) {
									addInstrII(O_RET, sizeArgs, 0);
								}
								else {
									addInstrII(O_RET, sizeArgs, typeArgSize(&crtFunc->type));
								}
								return 1;
							}
							else tkerr(crtTk, "!!Lipsa ';' dupa RETURN sau dupa expresia de dupa RETURN!!\n");
						}
						else {
							if (expr(&rv)) {
								if (typeArgSize(&rv.type))addInstrI(O_DROP, typeArgSize(&rv.type));
							}
							if (consume(SEMICOLON)) {
								return 1;
							}
						}
					}
				}
			}
		}
	}
	crtTk = start;
	return 0;
}
//stmCompound: LACC ( declVar | stm )* RACC ;
int stmCompound() {
	printf("@stmCompound (%s)\n", codeName(crtTk->code));
	Token* tokenstart = crtTk;
	Symbol* start = symbols.end[-1];
	if (consume(LACC)) {
		crtDepth++;
		while (declVar() || stm());
		if (consume(RACC)) {
			crtDepth--;
			deleteSymbolsAfter(&symbols, start);
			return 1;
		}
		else tkerr(crtTk, "!!! Lipsa '}' sau eroare de sintaxa!!!\n");
	}
	crtTk = tokenstart;
	return 0;
}
//expr: exprAssign ;
int expr(RetVal* rv) {
	printf("@expr (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	if (exprAssign(rv)) {
		return 1;
	}
	crtTk = start;
	return 0;
}
//exprAssign: exprUnary ASSIGN exprAssign | exprOr ;
int exprAssign(RetVal* rv) {
	printf("@exprAssign (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	Instr* oldLastInstr = lastInstruction;
	Instr* i;
	if (exprUnary(rv)) {
		if (consume(ASSIGN)) {
			RetVal rve;
			if (exprAssign(&rve)) {
				if (!rv->isLVal)tkerr(crtTk, "cannot assign to a non-lval");
				if (rv->type.nElements > -1 || rve.type.nElements > -1)
					tkerr(crtTk, "the arrays cannot be assigned");
				cast(&rv->type, &rve.type);
				i = getRVal(&rve);
				addCastInstr(i, &rve.type, &rv->type);
				//duplicate the value on top before the dst addr
				addInstrII(O_INSERT, sizeof(void*) + typeArgSize(&rv->type), typeArgSize(&rv->type));
				addInstrI(O_STORE, typeArgSize(&rv->type));
				rv->isCtVal = rv->isLVal = 0;
				return 1;
			}
			else tkerr(crtTk, "!!Expresie invalida dupa egal!!\n");
		}
		crtTk = start; // S-a consumat exprUnary dar nu si Assign, deci mergem pe alta ramura
	}
	if (exprOr(rv)) {
		return 1;
	}
	crtTk = start;
	return 0;
}

/*
exprOr: exprOr OR exprAnd | exprAnd
IS LEFT RECURSIVE =>
exprOr = exprAnd exprOrPrim
exprOrPrim = OR exprAnd exprOrPrim | eps
*/
int exprOrPrim(RetVal* rv) {
	printf("@exprOrPrim (%s)\n", codeName(crtTk->code));
	RetVal rve;
	Instr* i1, *i2;
	Type t, t1, t2;
	if (consume(OR)) {
		i1 = rv->type.nElements < 0 ? getRVal(rv) : lastInstruction;
		t1 = rv->type;
		if (exprAnd(&rve)) {
			if (rv->type.typeBase == TB_STRUCT || rve.type.typeBase == TB_STRUCT)
				tkerr(crtTk, "a structure cannot be logically tested");
			if (rv->type.nElements >= 0) {      // vectors
				addInstr(O_OR_A);
			}
			else {  // non-vectors
				i2 = getRVal(&rve); t2 = rve.type;
				t = getArithType(&t1, &t2);
				addCastInstr(i1, &t1, &t);
				addCastInstr(i2, &t2, &t);
				switch (t.typeBase) {
				case TB_INT:addInstr(O_OR_I); break;
				case TB_DOUBLE:addInstr(O_OR_D); break;
				case TB_CHAR:addInstr(O_OR_C); break;
				}
			}
			rv->type = createType(TB_INT, -1);
			rv->isCtVal = rv->isLVal = 0;
			if (exprOrPrim(rv))
				return 1;
		}
	}
	return 1;
}
int exprOr(RetVal* rv) {
	printf("@exprOr (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	if (exprAnd(rv)) {
		if (exprOrPrim(rv)) {
			return 1;
		}
	}
	crtTk = start;
	return 0;
}
/*
exprAnd: exprAnd AND exprEq | exprEq ;
IS LEFT RECURSIVE =>
exprAnd = exprEq exprAndPrim
exprAndPrim = AND exprEq exprAndPrim | eps
*/
int exprAndPrim(RetVal* rv) {
	printf("@exprAndPrim (%s)\n", codeName(crtTk->code));
	RetVal rve;
	Instr *i1, *i2;
	Type t, t1, t2;
	if (consume(AND)) {
		i1 = rv->type.nElements < 0 ? getRVal(rv) : lastInstruction;
		t1 = rv->type;
		if (exprEq(&rve)) {
			if (rv->type.typeBase == TB_STRUCT || rve.type.typeBase == TB_STRUCT)
				tkerr(crtTk, "a structure cannot be logically tested");
			if (rv->type.nElements >= 0) {      // vectors
				addInstr(O_AND_A);
			}
			else {  // non-vectors
				i2 = getRVal(&rve); t2 = rve.type;
				t = getArithType(&t1, &t2);
				addCastInstr(i1, &t1, &t);
				addCastInstr(i2, &t2, &t);
				switch (t.typeBase) {
				case TB_INT:addInstr(O_AND_I); break;
				case TB_DOUBLE:addInstr(O_AND_D); break;
				case TB_CHAR:addInstr(O_AND_C); break;
				}
			}
			rv->type = createType(TB_INT, -1);
			rv->isCtVal = rv->isLVal = 0;
			if (exprAndPrim(rv))
				return 1;
		}
	}
	return 1;
}
int exprAnd(RetVal* rv) {
	printf("@exprAnd (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	if (exprEq(rv)) {
		if (exprAndPrim(rv))
			return 1;
	}
	crtTk = start;
	return 0;
}
/*
exprEq: exprEq ( EQUALS | NOTEQ ) exprRel | exprRel ;
IS LEFT RECURSIVE =>
exprEq = exprRel exprEqPrim
exprEqPrim = (EQUALS | NOTEQ) exprRel ExprEqPrim | Eps
*/
int exprEqPrim(RetVal* rv) {
	printf("@exprEqPrim (%s)\n", codeName(crtTk->code));
	Token* tkop;
	RetVal rve;
	Instr *i1, *i2;
	Type t, t1, t2;
	if (consume(EQUALS) || consume(NOTEQ)) {
		i1 = rv->type.nElements < 0 ? getRVal(rv) : lastInstruction;
		t1 = rv->type;
		tkop = consumedTk;
		if (exprRel(&rve)) {
			if (rv->type.typeBase == TB_STRUCT || rve.type.typeBase == TB_STRUCT)
				tkerr(crtTk, "a structure cannot be compared");
			if (rv->type.nElements >= 0) {      // vectors
				addInstr(tkop->code == EQUALS ? O_EQ_A : O_NOTEQ_A);
			}
			else {  // non-vectors
				i2 = getRVal(&rve); t2 = rve.type;
				t = getArithType(&t1, &t2);
				addCastInstr(i1, &t1, &t);
				addCastInstr(i2, &t2, &t);
				if (tkop->code == EQUALS) {
					switch (t.typeBase) {
					case TB_INT:addInstr(O_EQ_I); break;
					case TB_DOUBLE:addInstr(O_EQ_D); break;
					case TB_CHAR:addInstr(O_EQ_C); break;
					}
				}
				else {
					switch (t.typeBase) {
					case TB_INT:addInstr(O_NOTEQ_I); break;
					case TB_DOUBLE:addInstr(O_NOTEQ_D); break;
					case TB_CHAR:addInstr(O_NOTEQ_C); break;
					}
				}
			}
			rv->type = createType(TB_INT, -1);
			rv->isCtVal = rv->isLVal = 0;
			if (exprEqPrim(rv))
				return 1;
		}
	}
	return 1;
}
int exprEq(RetVal* rv) {
	printf("@exprEq (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	if (exprRel(rv)) {
		if (exprEqPrim(rv))
			return 1;
	}
	crtTk = start;
	return 0;
}
/*
exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd ;
IS LEFT RECURSIVE =>
exprRel = exprAdd exprRelPrim
exprRelPrim = ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd exprRelPrim | eps
*/
int exprRelPrim(RetVal* rv) {
	printf("@exprRelPrim (%s)\n", codeName(crtTk->code));
	Token* tkop;
	RetVal rve;
	Instr *i1, *i2;
	Type t, t1, t2;
	if (consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ)) {
		i1 = getRVal(rv); t1 = rv->type;
		tkop = consumedTk;
		if (exprAdd(&rve)) {
			if (rv->type.nElements > -1 || rve.type.nElements > -1)
				tkerr(crtTk, "an array cannot be compared");
			if (rv->type.typeBase == TB_STRUCT || rve.type.typeBase == TB_STRUCT)
				tkerr(crtTk, "a structure cannot be compared");
			i2 = getRVal(&rve); t2 = rve.type;
			t = getArithType(&t1, &t2);
			addCastInstr(i1, &t1, &t);
			addCastInstr(i2, &t2, &t);
			switch (tkop->code) {
			case LESS:
				switch (t.typeBase) {
				case TB_INT:addInstr(O_LESS_I); break;
				case TB_DOUBLE:addInstr(O_LESS_D); break;
				case TB_CHAR:addInstr(O_LESS_C); break;
				}
				break;
			case LESSEQ:
				switch (t.typeBase) {
				case TB_INT:addInstr(O_LESSEQ_I); break;
				case TB_DOUBLE:addInstr(O_LESSEQ_D); break;
				case TB_CHAR:addInstr(O_LESSEQ_C); break;
				}
				break;
			case GREATER:
				switch (t.typeBase) {
				case TB_INT:addInstr(O_GREATER_I); break;
				case TB_DOUBLE:addInstr(O_GREATER_D); break;
				case TB_CHAR:addInstr(O_GREATER_C); break;
				}
				break;
			case GREATEREQ:
				switch (t.typeBase) {
				case TB_INT:addInstr(O_GREATEREQ_I); break;
				case TB_DOUBLE:addInstr(O_GREATEREQ_D); break;
				case TB_CHAR:addInstr(O_GREATEREQ_C); break;
				}
				break;
			}
			rv->type = createType(TB_INT, -1);
			rv->isCtVal = rv->isLVal = 0;
			if (exprRelPrim(rv))
				return 1;
		}
	}
	return 1;
}
int exprRel(RetVal* rv) {
	printf("@exprRel (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	if (exprAdd(rv)) {
		if (exprRelPrim(rv)) {
			return 1;
		}
	}
	crtTk = start;
	return 0;
}
/*
exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul ;
IS LEFT RECURSIVE =>
exprAdd = exprMul exprAddPrim
exprAddPrim = (ADD | SUB) exprMul exprAddPrim | eps
*/
int exprAddPrim(RetVal* rv) {
	Token* tkop;
	RetVal rve;
	Instr *i1, *i2;
	Type t1, t2;
	printf("@exprAddPrim (%s)\n", codeName(crtTk->code));
	if (consume(ADD) || consume(SUB)) {
		i1 = getRVal(rv); t1 = rv->type;
		tkop = consumedTk;
		if (exprMul(&rve)) {
			if (rv->type.nElements > -1 || rve.type.nElements > -1)
				tkerr(crtTk, "an array cannot be added or subtracted");
			if (rv->type.typeBase == TB_STRUCT || rve.type.typeBase == TB_STRUCT)
				tkerr(crtTk, "a structure cannot be added or subtracted");
			rv->type = getArithType(&rv->type, &rve.type);
			i2 = getRVal(&rve); t2 = rve.type;
			addCastInstr(i1, &t1, &rv->type);
			addCastInstr(i2, &t2, &rv->type);
			if (tkop->code == ADD) {
				switch (rv->type.typeBase) {
				case TB_INT:addInstr(O_ADD_I); break;
				case TB_DOUBLE:addInstr(O_ADD_D); break;
				case TB_CHAR:addInstr(O_ADD_C); break;
				}
			}
			else {
				switch (rv->type.typeBase) {
				case TB_INT:addInstr(O_SUB_I); break;
				case TB_DOUBLE:addInstr(O_SUB_D); break;
				case TB_CHAR:addInstr(O_SUB_C); break;
				}
			}
			rv->isCtVal = rv->isLVal = 0;
			if (exprAddPrim(rv)) {
				return 1;
			}
		}
	}
	return 1;
}
int exprAdd(RetVal* rv) {
	printf("@exprAdd (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	if (exprMul(rv)) {
		if (exprAddPrim(rv))
			return 1;
	}
	crtTk = start;
	return 0;
}
/*
exprMul: exprMul ( MUL | DIV ) exprCast | exprCast ;
IS LEFT RECURSIVE =>
exprMul = exprCast exprMulPrim
exprMulPrim = ( MUL | DIV ) exprCast exprMulPrim | eps
*/
int exprMulPrim(RetVal* rv) {
	printf("@exprMulPrim (%s)\n", codeName(crtTk->code));
	Token* tkop;
	RetVal rve;
	Instr *i1, *i2;
	Type t1, t2;
	if (consume(MUL) || consume(DIV)) {
		i1 = getRVal(rv); t1 = rv->type;
		tkop = consumedTk;
		if (exprCast(&rve)) {
			if (rv->type.nElements > -1 || rve.type.nElements > -1)
				tkerr(crtTk, "an array cannot be multiplied or divided");
			if (rv->type.typeBase == TB_STRUCT || rve.type.typeBase == TB_STRUCT)
				tkerr(crtTk, "a structure cannot be multiplied or divided");
			rv->type = getArithType(&rv->type, &rve.type);
			i2 = getRVal(&rve); t2 = rve.type;
			addCastInstr(i1, &t1, &rv->type);
			addCastInstr(i2, &t2, &rv->type);
			if (tkop->code == MUL) {
				switch (rv->type.typeBase) {
				case TB_INT:addInstr(O_MUL_I); break;
				case TB_DOUBLE:addInstr(O_MUL_D); break;
				case TB_CHAR:addInstr(O_MUL_C); break;
				}
			}
			else {
				switch (rv->type.typeBase) {
				case TB_INT:addInstr(O_DIV_I); break;
				case TB_DOUBLE:addInstr(O_DIV_D); break;
				case TB_CHAR:addInstr(O_DIV_C); break;
				}
			}
			rv->isCtVal = rv->isLVal = 0;
			if (exprMulPrim(rv)) {
				return 1;
			}
		}
	}
	return 1;
}
int exprMul(RetVal* rv) {
	printf("@exprMul (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	if (exprCast(rv)) {
		if (exprMulPrim(rv)) {
			return 1;
		}
	}
	crtTk = start;
	return 0;
}
//exprCast: LPAR typeName RPAR exprCast | exprUnary ;

// DE ADAUGAT ERORI

int exprCast(RetVal* rv) {
	printf("@exprCast (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	Type t;
	RetVal rve;
	Instr* oldLastInstr = lastInstruction;
	if (consume(LPAR)) {
		if (typeName(&t)) {
			if (consume(RPAR)) {
				if (exprCast(&rve)) {
					cast(&t, &rve.type);
					if (rv->type.nElements < 0 && rv->type.typeBase != TB_STRUCT) {
						switch (rve.type.typeBase) {
						case TB_CHAR:
							switch (t.typeBase) {
							case TB_INT:addInstr(O_CAST_C_I); break;
							case TB_DOUBLE:addInstr(O_CAST_C_D); break;
							}
							break;
						case TB_DOUBLE:
							switch (t.typeBase) {
							case TB_CHAR:addInstr(O_CAST_D_C); break;
							case TB_INT:addInstr(O_CAST_D_I); break;
							}
							break;
						case TB_INT:
							switch (t.typeBase) {
							case TB_CHAR:addInstr(O_CAST_I_C); break;
							case TB_DOUBLE:addInstr(O_CAST_I_D); break;
							}
							break;
						}
					}
					rv->type = t;
					rv->isCtVal = rv->isLVal = 0;
					return 1;
				}
			}
		}
	}
	else {
		deleteInstructionsAfter(oldLastInstr);
		if (exprUnary(rv)) {
			return 1;
		}
	}
	crtTk = start;
	return 0;
}
//exprUnary: ( SUB | NOT ) exprUnary | exprPostfix ;
int exprUnary(RetVal* rv) {
	printf("@exprUnary (%s)\n", codeName(crtTk->code));
	Token* tkop;
	Token* start = crtTk;
	if (consume(SUB) || consume(NOT)) {
		tkop = consumedTk;
		if (exprUnary(rv)) {
			if (tkop->code == SUB) {
				if (rv->type.nElements >= 0)tkerr(crtTk, "unary '-' cannot be applied to an array");
				if (rv->type.typeBase == TB_STRUCT)
					tkerr(crtTk, "unary '-' cannot be applied to a struct");
				getRVal(rv);
				switch (rv->type.typeBase) {
				case TB_CHAR:addInstr(O_NEG_C); break;
				case TB_INT:addInstr(O_NEG_I); break;
				case TB_DOUBLE:addInstr(O_NEG_D); break;
				}
			}
			else {  // NOT
				if (rv->type.typeBase == TB_STRUCT)tkerr(crtTk, "'!' cannot be applied to a struct");
				if (rv->type.nElements < 0) {
					getRVal(rv);
					switch (rv->type.typeBase) {
					case TB_CHAR:addInstr(O_NOT_C); break;
					case TB_INT:addInstr(O_NOT_I); break;
					case TB_DOUBLE:addInstr(O_NOT_D); break;
					}
				}
				else {
					addInstr(O_NOT_A);
				}
				rv->type = createType(TB_INT, -1);
			}
			rv->isCtVal = rv->isLVal = 0;
			return 1;
		}
	}
	else {
		if (exprPostFix(rv)) {
			return 1;
		}
	}
	crtTk = start;
	return 0;
}
/*
exprPostfix: exprPostfix LBRACKET expr RBRACKET
		   | exprPostfix DOT ID
		   | exprPrimary ;
IS LEFT RECURSIVE =>
exprPostFix = exprPrimary exprPostFixPrim
exprPostFixPrim = LBRACKET expr RBRACKET exprPostFixPrim
			| DOT ID exprPostFixPrim
			| eps

*/
int exprPostFixPrim(RetVal* rv) {
	printf("@exprPostFixPrim (%s)\n", codeName(crtTk->code));
	RetVal rve;
	if (consume(LBRAKET)) {
		if (expr(&rve)) {
			if (rv->type.nElements < 0)tkerr(crtTk, "only an array can be indexed");
			Type typeInt = createType(TB_INT, -1);
			cast(&typeInt, &rve.type);
			rv->type = rv->type;
			rv->type.nElements = -1;
			rv->isLVal = 1;
			rv->isCtVal = 0;
			if (consume(RBRAKET)) {
				addCastInstr(lastInstruction, &rve.type, &typeInt);
				getRVal(&rve);
				if (typeBaseSize(&rv->type) != 1) {
					addInstrI(O_PUSHCT_I, typeBaseSize(&rv->type));
					addInstr(O_MUL_I);
				}
				addInstr(O_OFFSET);
				if (exprPostFixPrim(rv))
					return 1;
			}
		}
	}
	else {
		if (consume(DOT)) {
			if (consume(ID)) {
				Token* tkName = consumedTk;
				Symbol* sStruct = rv->type.s;
				Symbol* sMember = findSymbol(&sStruct->members, tkName->text);
				if (!sMember)
					tkerr(crtTk, "struct %s does not have a member %s", sStruct->name, tkName->text);
				rv->type = sMember->type;
				rv->isLVal = 1;
				rv->isCtVal = 0;
				if (sMember->offset) {
					addInstrI(O_PUSHCT_I, sMember->offset);
					addInstr(O_OFFSET);
				}
				if (exprPostFixPrim(rv))
					return 1;
			}
		}
	}
	return 1;
}
int exprPostFix(RetVal* rv) {
	printf("@exprPostFix (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	if (exprPrimary(rv)) {
		if (exprPostFixPrim(rv)) {
			return 1;
		}
	}
	crtTk = start;
	return 0;
}
/*
exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?
		   | CT_INT
		   | CT_REAL
		   | CT_CHAR
		   | CT_STRING
		   | LPAR expr RPAR ;
	ERORI ADAUGATE
*/
int exprPrimary(RetVal* rv) {
	printf("@exprPrimary (%s)\n", codeName(crtTk->code));
	Token* start = crtTk;
	int LPARisConsumed;
	int RPARisConsumed;
	RetVal arg;
	Instr* i;
	if (consume(ID)) {
		Token* tkName = consumedTk;
		Symbol* s = findSymbol(&symbols, tkName->text);
		if (!s)tkerr(crtTk, "undefined symbol %s", tkName->text);
		rv->type = s->type;
		rv->isCtVal = 0;
		rv->isLVal = 1;
		LPARisConsumed = consume(LPAR);
		Symbol** crtDefArg = s->args.begin;
		if (LPARisConsumed) {
			if (s->cls != CLS_FUNC && s->cls != CLS_EXTFUNC)
				tkerr(crtTk, "call of the non-function %s", tkName->text);
		}
		if (expr(&arg)) {
			if (crtDefArg == s->args.end)tkerr(crtTk, "too many arguments in call");
			cast(&(*crtDefArg)->type, &arg.type);
			if ((*crtDefArg)->type.nElements < 0) {  //only arrays are passed by addr
				i = getRVal(&arg);
			}
			else {
				i = lastInstruction;
			}
			addCastInstr(i, &arg.type, &(*crtDefArg)->type);
			crtDefArg++;
			while (consume(COMMA)) {
				if (expr(&arg)) {
					if (crtDefArg == s->args.end)tkerr(crtTk, "too many arguments in call");
					cast(&(*crtDefArg)->type, &arg.type);
					if ((*crtDefArg)->type.nElements < 0) {
						i = getRVal(&arg);
					}
					else {
						i = lastInstruction;
					}
					addCastInstr(i, &arg.type, &(*crtDefArg)->type);
					crtDefArg++;
				}
				else tkerr(crtTk, "!!Lipsa expresie dupa ',' !!\n");
			}
		}
		RPARisConsumed = consume(RPAR);
		if (LPARisConsumed && !RPARisConsumed) {
			tkerr(crtTk, "!!Lipsa ')' dupa expresie sau dupa '(' anterioara !!\n");
		}
		if (RPARisConsumed) {
			if (crtDefArg != s->args.end)tkerr(crtTk, "too few arguments in call");
			rv->type = s->type;
			rv->isCtVal = rv->isLVal = 0;
			i = addInstr(s->cls == CLS_FUNC ? O_CALL : O_CALLEXT);
			i->args[0].addr = s->addr;
		}
		/////////////////// NU SUNT SIGUR AICI
		else {
			if (s->cls == CLS_FUNC || s->cls == CLS_EXTFUNC)
				tkerr(crtTk, "missing call for function %s", tkName->text);
			if (s->depth) {
				addInstrI(O_PUSHFPADDR, s->offset);
			}
			else {
				addInstrA(O_PUSHCT_A, s->addr);
			}
		}
		return 1;
	}
	else {
		if (consume(CT_INT)) {
			Token* tki = consumedTk;
			rv->type = createType(TB_INT, -1); rv->ctVal.i = tki->i;
			rv->isCtVal = 1; rv->isLVal = 0;
			addInstrI(O_PUSHCT_I, tki->i);
			return 1;
		}
		else {
			if (consume(CT_REAL)) {
				Token* tkr = consumedTk;
				rv->type = createType(TB_DOUBLE, -1); rv->ctVal.d = tkr->r;
				rv->isCtVal = 1; rv->isLVal = 0;
				i = addInstr(O_PUSHCT_D); i->args[0].d = tkr->r;
				return 1;
			}
			else {
				if (consume(CT_CHAR)) {
					Token* tkc = consumedTk;
					rv->type = createType(TB_CHAR, -1); rv->ctVal.i = tkc->i;
					rv->isCtVal = 1; rv->isLVal = 0;
					addInstrI(O_PUSHCT_C, tkc->i);
					return 1;
				}
				else {
					if (consume(CT_STRING)) {
						Token* tks = consumedTk;
						rv->type = createType(TB_CHAR, 0); rv->ctVal.str = tks->text;
						rv->isCtVal = 1; rv->isLVal = 0;
						addInstrA(O_PUSHCT_A, tks->text);
						return 1;
					}
					else {
						if (consume(LPAR)) {
							if (expr(rv)) {
								if (consume(RPAR)) {
									return 1;
								}
								else tkerr(crtTk, "!!Lipsa ')' dupa expresie!!\n");
							}
						}
					}
				}
			}
		}
	}
	crtTk = start;
	return 0;
}
/////////////////////////////////////////////////////////////////////////////////////////////

