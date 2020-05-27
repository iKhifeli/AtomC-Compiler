
/************************************************************
VIRTUAL MACHINE FOR ATOM C COMPILER
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

Instr* instructions, * lastInstruction; // double linked list

#define STACK_SIZE (32*1024)
char stack[STACK_SIZE];
char* SP; // Stack Pointer
char* stackAfter; // first byte after stack; used for stack limit tests


void pushd(double d)
{
	if (SP + sizeof(double) > stackAfter)err("out of stack");
	*(double*)SP = d;
	SP += sizeof(double);
}
double popd()
{
	SP -= sizeof(double);
	if (SP < stack)err("not enough stack bytes for popd");
	return *(double*)SP;
}
void pusha(void* a)
{
	if (SP + sizeof(void*) > stackAfter)err("out of stack");
	*(void**)SP = a;
	SP += sizeof(void*);
}
void* popa()
{
	SP -= sizeof(void*);
	if (SP < stack)err("not enough stack bytes for popa");
	return *(void**)SP;
}
void pushi(int i) {
	if (SP + sizeof(int) > stackAfter)err("out of stack");
	*(int*)SP = i;
	SP += sizeof(int);
}
int popi() {
	SP -= sizeof(int);
	if (SP < stack)err("not enough stack bytes for popi");
	return *(int*)SP;
}
void pushc(char c) {
	if (SP + sizeof(char) > stackAfter)err("out of stack");
	*(char*)SP = c;
	SP += sizeof(char);
}
char popc() {
	SP -= sizeof(char);
	if (SP < stack)err("not enough stack bytes for popc");
	return *(char*)SP;
}


Instr* createInstr(int opcode)
{
	Instr* i;
	SAFEALLOC(i, Instr);
	i->opcode = opcode;
	return i;
}
void insertInstrAfter(Instr* after, Instr* i)
{
	i->next = after->next;
	i->last = after;
	after->next = i;
	if (i->next == NULL)lastInstruction = i;
}
Instr* addInstr(int opcode)
{
	Instr* i = createInstr(opcode);
	i->next = NULL;
	i->last = lastInstruction;
	if (lastInstruction) {
		lastInstruction->next = i;
	}
	else {
		instructions = i;
	}
	lastInstruction = i;
	return i;
}
Instr* addInstrAfter(Instr* after, int opcode)
{
	Instr* i = createInstr(opcode);
	insertInstrAfter(after, i);
	return i;
}
Instr* addInstrA(int opcode, void* addr) {
	Instr* i = addInstr(opcode);
	i->args[0].addr = addr;
	return i;
}
Instr* addInstrI(int opcode, int val) {
	Instr* i = addInstr(opcode);
	i->args[0].i = val;
	return i;
}
Instr* addInstrII(int opcode, int val1, int val2) {
	Instr* i = addInstr(opcode);
	i->args[0].i = val1;
	i->args[1].i = val2;
	return i;
}

void deleteInstructionsAfter(Instr* start) {
	start->next = NULL;
	lastInstruction = start;
}
/*
void delInstr(Instr* instr) {
	if (instr == NULL) {
		return;
	}
	if (instr == instructions) {
		free(instructions->next);
	}
	if (instr == lastInstruction) {
		return;
	}
	for (Instr* aux = instr->next; aux != NULL; aux = aux->next) {
		free(aux);
	}
	instr->next = NULL;
	lastInstruction = instr;
	
}
*/
/* ---------------------------------------------------------------------------------------------- */

#define GLOBAL_SIZE (32*1024)
char globals[GLOBAL_SIZE];
int nGlobals;

void* allocGlobal(int size)
{
	void* p = globals + nGlobals;
	if (nGlobals + size > GLOBAL_SIZE)err("insufficient globals space");
	nGlobals += size;
	return p;
}

void mvTest()
{
	Instr* L1;
	int* v = allocGlobal(sizeof(int));
	addInstrA(O_PUSHCT_A, v);
	addInstrI(O_PUSHCT_I, 3);
	addInstrI(O_STORE, sizeof(int));
	L1 = addInstrA(O_PUSHCT_A, v);
	addInstrI(O_LOAD, sizeof(int));
	//addInstrA(O_CALLEXT, requireSymbol(&symbols, "put_i")->addr);
	addInstrA(O_CALLEXT, findSymbol(&symbols, "put_i")->addr);
	addInstrA(O_PUSHCT_A, v);
	addInstrA(O_PUSHCT_A, v);
	addInstrI(O_LOAD, sizeof(int));
	addInstrI(O_PUSHCT_I, 1);
	addInstr(O_SUB_I);
	addInstrI(O_STORE, sizeof(int));
	addInstrA(O_PUSHCT_A, v);
	addInstrI(O_LOAD, sizeof(int));
	addInstrA(O_JT_I, L1);
	addInstr(O_HALT);
}

void put_i()
{
	printf("#%ld\n", popi());
}

void addPredefinedFunctions() {
	//void put_s(char s[])
	//Symbol* put_sFunction = addExtFunc("put_s", createType(TB_VOID, -1), put_s);
	//Symbol* arg_put_sFunction = addSymbol(&put_sFunction->args, "s", CLS_VAR);
	//arg_put_sFunction->type = createType(TB_CHAR, 0);

	//void get_s(char s[])
	//Symbol* get_sFunction = addExtFunc("get_s", createType(TB_VOID, -1),get_s);
	//Symbol* arg_get_sFunction = addSymbol(&get_sFunction->args, "s", CLS_VAR);
	//arg_get_sFunction->type = createType(TB_CHAR, 0);

	//void put_i(int i) 
	Symbol* put_iFunction = addExtFunc("put_i", createType(TB_VOID, -1), put_i);
	Symbol* arg_put_iFunction = addSymbol(&put_iFunction->args, "i", CLS_VAR);
	arg_put_iFunction->type = createType(TB_INT, -1);
	//int get_i() 

	//Symbol* get_iFunction = addExtFunc("get_i", createType(TB_INT, -1), get_i);

	//void put_d(double d) 
	//Symbol* put_dFunction = addExtFunc("put_d", createType(TB_VOID, -1), put_d);
	//Symbol* arg_put_dFunction = addSymbol(&put_dFunction->args, "d", CLS_VAR);
	//arg_put_dFunction->type = createType(TB_DOUBLE, -1);

	//double get_d()
	//Symbol* get_dFunction = addExtFunc("get_d", createType(TB_DOUBLE, -1), get_d);

	//void put_c(char c)
	//Symbol* put_cFunction = addExtFunc("put_c", createType(TB_VOID, -1), put_c);
	//Symbol* arg_put_cFunction = addSymbol(&put_cFunction->args, "c", CLS_VAR);
	//arg_put_cFunction->type = createType(TB_CHAR, -1);

	//char get_c() 
	//Symbol* get_cFunction = addExtFunc("get_c", createType(TB_CHAR, -1), get_c);

	//double seconds() 
	//Symbol* secondsFunction = addExtFunc("seconds", createType(TB_DOUBLE, -1), seconds);
}

void run(Instr* IP)
{
	int iVal1, iVal2;
	double dVal1, dVal2;
	char* aVal1;
	char* FP = 0, * oldSP;
	SP = stack;
	stackAfter = stack + STACK_SIZE;
	while (1) {
		printf("%p/%d\t", IP, SP - stack);
		switch (IP->opcode) {
		case O_CALL:
			aVal1 = IP->args[0].addr;
			printf("CALL\t%p\n", aVal1);
			pusha(IP->next);
			IP = (Instr*)aVal1;
			break;
		case O_CALLEXT:
			printf("CALLEXT\t%p\n", IP->args[0].addr);
			(*(void(*)())IP->args[0].addr)();
			IP = IP->next;
			break;
		case O_CAST_I_D:
			iVal1 = popi();
			dVal1 = (double)iVal1;
			printf("CAST_I_D\t(%ld -> %g)\n", iVal1, dVal1);
			pushd(dVal1);
			IP = IP->next;
			break;
		case O_DROP:
			iVal1 = IP->args[0].i;
			printf("DROP\t%ld\n", iVal1);
			if (SP - iVal1 < stack)err("not enough stack bytes");
			SP -= iVal1;
			IP = IP->next;
			break;
		case O_ENTER:
			iVal1 = IP->args[0].i;
			printf("ENTER\t%ld\n", iVal1);
			pusha(FP);
			FP = SP;
			SP += iVal1;
			IP = IP->next;
			break;
		case O_EQ_D:
			dVal1 = popd();
			dVal2 = popd();
			printf("EQ_D\t(%g==%g -> %ld)\n", dVal2, dVal1, dVal2 == dVal1);
			pushi(dVal2 == dVal1);
			IP = IP->next;
			break;
		case O_HALT:
			printf("HALT\n");
			return;
		case O_INSERT:
			iVal1 = IP->args[0].i; // iDst
			iVal2 = IP->args[1].i; // nBytes
			printf("INSERT\t%ld,%ld\n", iVal1, iVal2);
			if (SP + iVal2 > stackAfter)err("out of stack");
			memmove(SP - iVal1 + iVal2, SP - iVal1, iVal1); //make room
			memmove(SP - iVal1, SP + iVal2, iVal2); //dup
			SP += iVal2;
			IP = IP->next;
			break;
		case O_JT_I:
			iVal1 = popi();
			printf("JT\t%p\t(%ld)\n", IP->args[0].addr, iVal1);
			IP = iVal1 ? IP->args[0].addr : IP->next;
			break;
		case O_LOAD:
			iVal1 = IP->args[0].i;
			aVal1 = popa();
			printf("LOAD\t%ld\t(%p)\n", iVal1, aVal1);
			if (SP + iVal1 > stackAfter)err("out of stack");
			memcpy(SP, aVal1, iVal1);
			SP += iVal1;
			IP = IP->next;
			break;
		case O_OFFSET:
			iVal1 = popi();
			aVal1 = popa();
			printf("OFFSET\t(%p+%ld -> %p)\n", aVal1, iVal1, aVal1 + iVal1);
			pusha(aVal1 + iVal1);
			IP = IP->next;
			break;
		case O_PUSHFPADDR:
			iVal1 = IP->args[0].i;
			printf("PUSHFPADDR\t%ld\t(%p)\n", iVal1, FP + iVal1);
			pusha(FP + iVal1);
			IP = IP->next;
			break;
		case O_PUSHCT_A:
			aVal1 = IP->args[0].addr;
			printf("PUSHCT_A\t%p\n", aVal1);
			pusha(aVal1);
			IP = IP->next;
			break;
		case O_PUSHCT_I: /////////////////////////////
			aVal1 = IP->args[0].i;
			printf("PUSHCT_I\t%p\n", aVal1);
			pushi(aVal1);
			IP = IP->next;
			break;
		case O_RET:
			iVal1 = IP->args[0].i; // sizeArgs
			iVal2 = IP->args[1].i; // sizeof(retType)
			printf("RET\t%ld,%ld\n", iVal1, iVal2);
			oldSP = SP;
			SP = FP;
			FP = popa();
			IP = popa();
			if (SP - iVal1 < stack)err("not enough stack bytes");
			SP -= iVal1;
			memmove(SP, oldSP - iVal2, iVal2);
			SP += iVal2;
			break;
		case O_STORE:
			iVal1 = IP->args[0].i;
			if (SP - (sizeof(void*) + iVal1) < stack)err("not enough stack bytes for SET");
			aVal1 = *(void**)(SP - ((sizeof(void*) + iVal1)));
			printf("STORE\t%ld\t(%p)\n", iVal1, aVal1);
			memcpy(aVal1, SP - iVal1, iVal1);
			SP -= sizeof(void*) + iVal1;
			IP = IP->next;
			break;
		case O_SUB_D:
			dVal1 = popd();
			dVal2 = popd();
			printf("SUB_D\t(%g-%g -> %g)\n", dVal2, dVal1, dVal2 - dVal1);
			pushd(dVal2 - dVal1);
			IP = IP->next;
			break;
		case O_SUB_I: ///////////////////////////
			dVal1 = popi();
			dVal2 = popi();
			printf("SUB_I\t(%g-%g -> %g)\n", dVal2, dVal1, dVal2 - dVal1);
			pushi(dVal2 - dVal1);
			IP = IP->next;
			break;
		default:
			err("invalid opcode: %d", IP->opcode);
		}
	}
}