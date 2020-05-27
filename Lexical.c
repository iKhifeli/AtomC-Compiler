
/************************************************************
ANALIZOR LEXICAL
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

char* pch; // iterator pentru cifre/litere
int line = 1; // current line in code that is being compiled// has to be incremented when '\0' occurs. ****space and comment****
struct Token* lastToken; /*sfarsitul listei de tokeni*/
struct Token* tokens;/*inceputul listei de tokeni*/


static inline char* stringFromEnum(enum atoms z)
{
	static const char* strings[] = { "ID", "INT", "BREAK", "CHAR", "DOUBLE", "ELSE", "FOR", "IF", "STRUCT", "VOID", "WHILE", "RETURN",
		"CT_INT", "CT_REAL", "CT_CHAR", "CT_STRING", "ADD", "SUB", "DIV", "MULL", "DOT", "COMMA", "SEMICOLON", "AND", "OR", "NOT", "LPAR",
		"RPAR", "LBRAKET", "RBRAKET", "LACC", "RACC", "ASSIGN", "EQUALS", "NEQUAL", "LESS", "LESSEQ", "GREATER", "GREATEREQ", "END"
	};
	return strings[z];
}

const char* codeName(int code) {
	return stringFromEnum(code);
}

void err(const char* fmt, ...)
{
	va_list va;
	va_start(va, fmt);
	fprintf(stderr, "error: ");
	vfprintf(stderr, fmt, va);
	fputc('\n', stderr);
	va_end(va);
	exit(-1);
}


void printAtom() {
	Token* i = tokens;
	printf("\nTokenii sunt urmatorii :\n");
	while (i != NULL) {
		if (i->code == 0) {
			printf("%s NUME : %s, line %d\n", codeName(i->code), i->text, i->line);
		}
		else {
			if (i->code == 12) {
				printf("%s Val : %d, line %d\n", codeName(i->code), i->i, i->line);
			}
			else {
				if (i->code == 13) {
					printf("%s Val : %f, line %d\n", codeName(i->code), i->r, i->line);
				}
				else {
					if (i->code == 14) {
						printf("%s Val : %c, line %d\n", codeName(i->code), i->i, i->line);
					}
					else {
						if (i->code == 15) {
							printf("%s Continut : %s, line %d\n", codeName(i->code), i->text, i->line);
						}
						else printf("%s line %d\n", codeName(i->code), i->line);
					}
				}
			}
		}
		i = i->next;
	}
	printf("\n---SFARSIT---\n");
}

Token* addTk(int code)
{
	Token* tk;
	SAFEALLOC(tk, Token)
		tk->code = code;
	tk->line = line;
	tk->next = NULL;
	if (lastToken) {
		lastToken->next = tk;
	}
	else {
		tokens = tk;
	}
	lastToken = tk;
	return tk;
}

char* createString(char* start, char* pch) {
	char* aux = start;
	int n = pch - start + 1;
	char* newText = (char*)malloc(n);
	char* p = newText;
	while (aux < pch) {
		*p = *aux;
		p++;
		aux++;
	}
	*p = '\0';
	return newText;
}

char createChar(char* start, char* pch) {
	char* aux = start;
	char* newChar = (char*)malloc(2);
	char* p = newChar;
	*p = *aux;
	p++;
	p = '\0';
	char val = *newChar;
	free(newChar);
	return val;
}

int createInt(char* intStart, char* pch) {
	char* aux = intStart;
	int n = pch - intStart + 1;
	char* newText = (char*)malloc(n);
	char* p = newText;
	while (aux < pch) {
		*p = *aux;
		p++;
		aux++;
	}
	*p = '\0';
	int val = atoi(newText);
	free(newText);
	return val;
}

double createDouble(char* start, char* pch) {
	char* aux = start;
	int n = pch - start + 1;
	char* newText = (char*)malloc(n);
	char* p = newText;
	while (aux < pch) {
		*p = *aux;
		p++;
		aux++;
	}
	*p = '\0';
	double val = strtod(newText, NULL);
	free(newText);
	return val;
}

int getNextToken() {
	Token* tk;
	int state = 0;
	char* start = NULL; // pointer to remember the start of the id.
	char* numStart = NULL; // pointer to remember the start of the INT/Double number.
	char character = *pch;
	printf("#stare:%d, character:%c (%d)\n", state, character, character); //pt debug
	for (;;) {
		character = *pch;
		switch (state) {
		case 0: {
			if (isalpha(character) || character == '_') {
				state = 23;
				start = pch;
				pch++;
				break;
			}
			if (character == ';') {
				tk = addTk(SEMICOLON);
				pch++;
				return tk->code;
				break;
			}
			if (character == '0') {
				numStart = pch;
				pch++;
				state = 1;
				break;
			}
			if (isdigit(character) && character != '0') {
				state = 6;
				numStart = pch;
				pch++;
				break;
			}
			if (character == '=') {
				pch++;
				state = 29;
				break;
			}
			if (character == '\n') {
				pch++;
				line++;
				break;
			}
			if (character == '(') {
				tk = addTk(LPAR);
				pch++;
				return tk->code;
				break;
			}
			if (character == ')') {
				tk = addTk(RPAR);
				pch++;
				return tk->code;
				break;
			}
			if (character == ' ' || character == '\r' || character == '\t') {
				pch++;
				break;
			}
			if (character == '\'') {
				pch++;
				state = 12;
				break;
			}
			if (character == '"') {
				pch++;
				state = 16;
				start = pch;
				break;
			}
			if (character == '<') {
				state = 27;
				pch++;
				break;
			}
			if (character == '>') {
				state = 28;
				pch++;
				break;
			}
			if (character == '|') {
				state = 26;
				pch++;
				break;
			}
			if (character == '&') {
				pch++;
				state = 25;
				break;
			}
			if (character == '/') {
				state = 19;
				pch++;
				break;
			}
			if (character == '!') {
				pch++;
				state = 24;
				break;
			}
			if (character == '+') {
				tk = addTk(ADD);
				pch++;
				return tk->code;
				break;
			}
			if (character == '-') {
				tk = addTk(SUB);
				pch++;
				return tk->code;
				break;
			}
			if (character == '*') {
				tk = addTk(MUL);
				pch++;
				return tk->code;
				break;
			}
			if (character == '.') {
				tk = addTk(DOT);
				pch++;
				return tk->code;
				break;
			}
			if (character == ',') {
				tk = addTk(COMMA);
				pch++;
				return tk->code;
				break;
			}
			if (character == '[') {
				tk = addTk(LBRAKET);
				pch++;
				return tk->code;
				break;
			}
			if (character == ']') {
				tk = addTk(RBRAKET);
				pch++;
				return tk->code;
				break;
			}
			if (character == '{') {
				tk = addTk(LACC);
				pch++;
				return tk->code;
				break;
			}
			if (character == '}') {
				tk = addTk(RACC);
				pch++;
				return tk->code;
				break;
			}
			////// END ///////
			if (character == '\0') {
				tk = addTk(END);
				return tk->code;
				break;
			}
			printf("!!!CHARACTER INVALID PENTRU STAREA 0!!!");
			break;
		}
		case 1: {
			if (character == 'x' || character == 'X') {
				state = 2;
				pch++;
			}
			else {
				state = 4;
				break;
			}
			break;
		}
		case 2: {
			if (isdigit(character) || character >= 'a' && character <= 'f' || character >= 'A' && character <= 'F') {
				state = 3;
				pch++;
			}
			break;
		}
		case 3: {
			if (isdigit(character) || character >= 'a' && character <= 'f' || character >= 'A' && character <= 'F') {
				pch++;
				break;
			}
			else {
				tk = addTk(CT_INT);
				tk->i = createInt(numStart, pch);
				pch++;
				return tk->code;
			}
			break;
		}
		case 4: {
			if (character >= '0' && character <= '7') {
				pch++;
				break;
			}
			else {
				if (character == '8' || character == '9') {
					state = 5;
					pch++;
					break;
				}
				else {
					if (character == 'E' || character == 'e') {
						state = 9;
						pch++;
						break;
					}
					else {
						if (character == '.') {
							state = 7;
							pch++;
							break;
						}
						else {
							tk = addTk(CT_INT);
							tk->i = createInt(numStart, pch);
							return tk->code;
							break;
						}
					}
				}
			}
			break;
		}
		case 5: {
			if (isdigit(character)) {
				pch++;
				break;
			}
			else {
				if (character == 'e' || character == 'E') {
					state = 9;
					pch++;
					break;
				}
				else {
					if (character == '.') {
						state = 7;
						pch++;
						break;
					}
				}
			}
		}
		case 6: {
			if (isdigit(character)) {
				pch++;
				break;
			}
			else {
				if (character == 'E' || character == 'e') {
					pch++;
					state = 9;
					break;
				}
				else {
					if (character == '.') {
						state = 7;
						pch++;
						break;
					}
					else {
						tk = addTk(CT_INT);
						tk->i = createInt(numStart, pch);
						return tk->code;
						break;
					}
				}
			}
			break;
		}
		case 7: {
			if (isdigit(character)) {
				state = 8;
				pch++;
				break;
			}
			break;
		}
		case 8: {
			if (isdigit(character)) {
				pch++;
				break;
			}
			else {
				if (character == 'e' || character == 'E') {
					state = 9;
					pch++;
					break;
				}
				else {
					tk = addTk(CT_REAL);
					tk->r = createDouble(numStart, pch);
					return tk->code;
					pch++;
					break;
				}
			}
			break;
		}
		case 9: {
			if (character == '+' || character == '-') {
				state = 10;
				pch++;
				break;
			}
			else {
				state = 10;
				pch++;
				break;
			}
			break;
		}
		case 10: {
			if (isdigit(character)) {
				state = 11;
				pch++;
				break;
			}
			break;
		}
		case 11: {
			if (isdigit(character)) {
				pch++;
				break;
			}
			else {
				tk = addTk(CT_REAL);
				tk->r = createDouble(numStart, pch);
				return tk->code;
				pch++;
				break;
			}
			break;
		}
		case 12: {
			if (character == '\\') {
				state = 13;
				pch++;
				break;
			}
			else {
				if (character != '\'' && character != '\\') {
					state = 15;
					start = pch;
					pch++;
					break;
				}
			}
			break;
		}
		case 13: {
			if (character == 'a' || character == 'b'
				|| character == 'f' || character == 'n'
				|| character == 'r' || character == 't'
				|| character == 'v' || character == '?'
				|| character == '\'' || character == '\\'
				|| character == '\0') {
				state = 14;
				pch++;
				break;
			}
			break;
		}
		case 14: {
			if (character == '\'') {
				tk = addTk(CT_CHAR);
				pch++;
				return tk->code;
				break;
			}
			break;
		}
			   // case 15 este identic cu 14, putea fi evitat, dar pt a nu crea confuzii in cod il las asa.
		case 15: {
			if (character == '\'') {
				tk = addTk(CT_CHAR);
				tk->i = createChar(start, pch);
				pch++;
				return tk->code;
				break;
			}
			break;
		}
		case 16: {
			if (character == '\\') {
				state = 17;
				pch++;
				break;
			}
			else {
				if (character == '"') {
					tk = addTk(CT_STRING);
					tk->text = createString(start, pch);
					pch++;
					return tk->code;
					break;
				}
				else {
					pch++;
					break;
				}
			}
			break;
		}
		case 17: {
			if (character == 'a' || character == 'b'
				|| character == 'f' || character == 'n'
				|| character == 'r' || character == 't'
				|| character == 'v' || character == '\''
				|| character == '?' || character == '"'
				|| character == '\\' || character == '\0') {
				state = 18;
				pch++;
				break;
			}
			break;
		}
		case 18: {
			if (character == '"') {
				tk = addTk(CT_STRING);
				tk->text = createString(start, pch);
				pch++;
				return tk->code;
				break;
			}
			else {
				state = 16;
				pch++;
				break;
			}

		}
		case 19: {
			if (character == '*') {
				state = 20;
				pch++;
				break;
			}
			else if (character == '/') {
				state = 22;
				pch++;
				break;
			}
			else {
				tk = addTk(DIV);
				return tk->code;
				break;
			}
			break;
		}
		case 20: {
			if (character == '*') {
				state = 21;
				pch++;
			}
			else {
				pch++;
				if (character == '\n')
					line++;
			}
			break;
		}
		case 21: {
			if (character == '*') {
				pch++;
				break;
			}
			else {
				if (character == '/') {
					state = 0;
					pch++;
					break;
				}
				else {
					pch++;
					state = 20;
					break;
				}
			}
			break;
		}
		case 22: {
			if (character == '\n') {
				line++;
				pch++;
				state = 0;
				break;
			}
			else if (character == '\r' || character == '\t') {
				pch++;
				state = 0;
				break;
			}
			else {
				pch++;
				break;
			}
		}
		case 23: {
			if (isalnum(character) || character == '_')
				pch++;
			else {
				if (start) {
					int charCount; // number of characters 
					charCount = pch - start;
					if (charCount == 2 && !(memcmp("if", start, charCount))) {
						tk = addTk(IF); state = 0; break;
					}
					if (charCount == 4 && !(memcmp("else", start, charCount))) {
						tk = addTk(ELSE); state = 0; break;
					}
					if (charCount == 3 && !(memcmp("int", start, charCount))) {
						tk = addTk(INT); state = 0; break;
					}
					if (charCount == 3 && !(memcmp("for", start, charCount))) {
						tk = addTk(FOR); state = 0; break;
					}
					if (charCount == 5 && !(memcmp("break", start, charCount))) {
						tk = addTk(BREAK); state = 0; break;
					}
					if (charCount == 4 && !(memcmp("void", start, charCount))) {
						tk = addTk(VOID); state = 0; break;
					}
					if (charCount == 5 && !(memcmp("while", start, charCount))) {
						tk = addTk(WHILE); state = 0; break;
					}
					if (charCount == 6 && !(memcmp("double", start, charCount))) {
						tk = addTk(DOUBLE); state = 0; break;
					}
					if (charCount == 4 && !(memcmp("char", start, charCount))) {
						tk = addTk(CHAR); state = 0; break;
					}
					if (charCount == 6 && !(memcmp("struct", start, charCount))) {
						tk = addTk(STRUCT); state = 0; break;
					}
					if (charCount == 6 && !(memcmp("return", start, charCount))) {
						tk = addTk(RETURN); state = 0; break;
					}
					tk = addTk(ID);
					tk->text = createString(start, pch);
					return tk->code;
				}
			}
			break;
		}
		case 24: {
			if (character == '=') {
				tk = addTk(NOTEQ);
				pch++;
				return tk->code;
			}
			else {
				tk = addTk(NOT);
				pch++;
				return tk->code;
			}
			break;
		}
		case 25: {
			if (character == '&') {
				tk = addTk(AND);
				pch++;
				return tk->code;
			}
			break;
		}
		case 26: {
			if (character == '|') {
				tk = addTk(OR);
				pch++;
				return tk->code;
				break;
			}
			break;
		}
		case 27: {
			if (character == '=') {
				tk = addTk(LESSEQ);
				pch++;
				return tk->code;
				break;
			}
			else {
				tk = addTk(LESS);
				pch++;
				return tk->code;
				break;
			}
			break;
		}
		case 28: {
			if (character == '=') {
				tk = addTk(GREATEREQ);
				pch++;
				return tk->code;
			}
			else {
				tk = addTk(GREATER);
				pch++;
				return tk->code;
			}
			break;
		}
		case 29: {
			if (character == '=') {
				tk = addTk(EQUALS);
				pch++;
				return tk->code;
			}
			else {
				tk = addTk(ASSIGN);
				return tk->code;
			}
			break;
		}
		default: err("Stare invalida %d", state);
			break;
		}
	}
	return 0;
}