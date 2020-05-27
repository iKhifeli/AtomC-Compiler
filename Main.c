/************************************************************
ATOM C COMPILER
by GELU POPA
************************************************************/

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#include "Header.h"

int main() {

	FILE* fis = fopen("fisier.c", "r");
	if (fis == NULL) {
		perror("Eroare la deschiderea fisierului!\n");
		exit(-1);
	}
	char buff[5000];
	//fseek(fis, 3, SEEK_SET);
	int n = fread(buff, 1, sizeof(buff), fis);
	if (n < 0) {
		perror("Eroare la citire\n");
		exit(-2);
	}
	buff[n] = '\0';
	fclose(fis);
	//printf("%hs", buff);
	pch = buff;
	while (getNextToken() != END);
	//printAtom();
	printf("\n\n");

	///***************************************************
	//ANALIZOR SINTACTIC + Analiza de domeniu si de tip
	//****************************************************/

	addPredefinedFunctions();

	crtTk = tokens; // setarea tokenului curent la inceputul listei de tokeni.
	if (unit()) {
		printf("SINTAXA OK");
	}
	else {
		tkerr(crtTk, "\n!!!!!Eroare sintactica!!!!!!\n");
	}

	//mvTest();
	//run(instructions);
	return 0;

}