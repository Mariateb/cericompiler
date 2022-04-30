//  A compiler from a very simple Pascal-like structured language LL(k)
//  to 64-bit 80x86 Assembly langage
//  Copyright (C) 2019 Pierre Jourlin
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <https://www.gnu.org/licenses/>.

// Build with "make compilateur"


#include <string>
#include <iostream>
#include <cstdlib>
#include <set>
#include <FlexLexer.h>
#include "tokeniser.h"
#include <cstring>

using namespace std;

// [=======Input Initialization and Functions=======]
	// Variables & Instances
	TOKEN current; // Current token
	FlexLexer* lexer = new yyFlexLexer; // Flex Tokeniser
	// Prototypes
	void readWord(); // Read the next word
	string currentWord(); // Return the current word
	bool compareWord(); // Compare the current word with given key
	TOKEN currentType(); // Return the type of the current word
	// Functions
	void readWord() {
		current = (TOKEN) lexer->yylex();
	}
	string currentWord() {
		return lexer->YYText();
	}
	bool compareWord(const char* key) {
		if(strcmp(currentWord().c_str(), key) == 0) {
			return(true);
		}
		return(false);
	}
	TOKEN currentType() {
		return current;
	}
	bool compareType(TOKEN token) {
		if(token == current) {
			return(true);
		}
		return(false);
	}
// [=======Output Initialization and Functions=======]
	// Variables & Instances
	unsigned long TagNumber = 0; // Tag number for asm labels
	// Prototypes
	void outputWrite(string s); // Write into the output without a comment
	void outputWrite(string s, string c); // Write into the output with a comment
	// Functions
	void outputWrite(string s) {
		// If the line is not a label, tab it
		if(!s.find(":") == -1) {
			cout << "\t";
		}
		cout << s << endl;
	}
	void outputWrite(string s, string c) {
		// Gets length of the command itself
		int length = s.length();
		// A tab is 4 spaces long, we check the equivalent in tabs of the string
		int tabEq = length/8;
		if(s.find(":") == -1) {
			cout << "\t";
			tabEq += 1;
		}
		// Finding how many tabs we need to add
		int tabNeed = 8 - tabEq;
		// Outputing
		cout << s;
		for(int i = 0; i <= tabNeed; i++) {
			cout << "\t";
		}
		cout << "# " << c << endl;
	}
// [=======Compilation tools=======]
	// Variables & Instances
	set<string> DeclaredVariables; // Set of declared variables
	enum OPREL {EQU, DIFF, INF, SUP, INFE, SUPE, WTFR}; // Relationnal Operators
	enum OPADD {ADD, SUB, OR, WTFA}; // Additionnal Operators
	enum OPMUL {MUL, DIV, MOD, AND ,WTFM}; // Multiplicative Operators
	enum TYPE {UNSIGNED_INT, BOOLEAN}; // Data types
	// Prototypes
	bool IsDeclared(string id); // Checks if a variable was declared during the declaration part
	void Error(string s); // Stops the compiler and returns an error to the terminal at current line
	bool checkKeyword(const char* key); // Checks if currently read word is a keyword, and checks if it matches a given key
	// Functions
	bool IsDeclared(string id) {
		return DeclaredVariables.find(id)!=DeclaredVariables.end();
	}
	void Error(string s) {
		cerr << "[=======Erreur=======]" << endl;
		cerr << "LIGNE #" << lexer->lineno() << " - LU : '" << lexer->YYText() << "'(" << current << ") - ERREUR : ";
		cerr << s << endl;
		cerr << "[====================]" << endl;
		exit(-1);
	}
	bool checkKeyword(const char* key) {
		if(!compareType(KEYWORD)) {
			Error("KEYWORD attendu.");
		}
		return(compareWord(key));
	}

// [=======Syntax rules=======]
	// Program := [DeclarationPart] StatementPart
	void Program();
	// DeclarationPart := "[" Letter {"," Letter} "]"
	void DeclarationPart();
	// StatementPart := Statement {";" Statement} "."
	void StatementPart();

	// Statement := AssignementStatement | IfStatement | WhileStatement | ForStatement | BlockStatement
	void Statement();
	// AssignementStatement := Identifier ":=" Expression
	string AssignementStatement();
	// IfStatement := "IF" Expression "THEN" Statement [ "ELSE" Statement ]
	void IfStatement();
	// WhileStatement := "WHILE" Expression DO Statement
	void WhileStatement();
	// ForStatement := "FOR" AssignementStatement "To" Expression "DO" Statement
	void ForStatement();
	// BlockStatement := "BEGIN" Statement { ";" Statement } "END"
	void BlockStatement();
	// DisplayStatement := "DISPLAY" Expression
	void DisplayStatement();

	// Expression := SimpleExpression [RelationalOperator SimpleExpression]
	TYPE Expression();
	// SimpleExpression := Term {AdditiveOperator Term}
	TYPE SimpleExpression();
	// Term := Factor {MultiplicativeOperator Factor}
	TYPE Term();
	// Factor := Number | Letter | "(" Expression ")"| "!" Factor
	TYPE Factor();
	// Number := Digit{Digit}
	TYPE Number();

	// AdditiveOperator := "+" | "-" | "||"
	OPADD AdditiveOperator();
	// MultiplicativeOperator := "*" | "/" | "%" | "&&"
	OPMUL MultiplicativeOperator();
	// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="
	OPREL RelationalOperator();
	// Identifier := Letter{Letter}
	TYPE Identifier();

int main() { // Source code on standard input, Output on standard output
	outputWrite("\t\t# This code was produced by the CERI compiler"); // Header
	readWord(); // Start the reading
	Program(); // Start the analysis and code production
	// Trailer for the gcc assembler / linker
	outputWrite("movq %rbp, %rsp", "Restore the position of the stack's top");
	outputWrite("ret","Return from main function");
	if(!compareType(FEOF)) {
		cerr << "Caractères en trop à la fin du programme : [" << current << "]";
		Error("."); // Unexpected characters at the end of the program
	}
}

// Program := [DeclarationPart] StatementPart
void Program() {
	if(compareType(RBRACKET)) {
		DeclarationPart();
	}
	StatementPart();
}

// DeclarationPart := "[" Ident {"," Ident} "]"
void DeclarationPart(){
	if(!compareType(RBRACKET)) {
		Error("caractère '[' attendu");
	}
	outputWrite(".data");
	outputWrite(".align 8");
	outputWrite("FormatString1:	.string \"%llu\\n\"");
	readWord();
	if(!compareType(ID)) {
		Error("Un identificater était attendu");
	}
	outputWrite(currentWord() + ":\t.quad 0");
	DeclaredVariables.insert(currentWord());
	readWord();
	while(compareType(COMMA)) {
		readWord();
		if(!compareType(ID)) {
			Error("Un identificateur était attendu");
		}
		outputWrite(currentWord() + ":\t.quad 0");
		DeclaredVariables.insert(currentWord());
		readWord();
	}
	if(!compareType(LBRACKET)) {
		Error("caractère ']' attendu");
	}
	readWord();
}

// StatementPart := Statement {";" Statement} "."
void StatementPart() {
	outputWrite(".text", "The following lines contain the program");
	outputWrite(".globl main", "The main function must be visible from outside");
	outputWrite("main:","Main function body");
	outputWrite("movq %rsp, %rbp", "Save the position of the stack's top");
	Statement();
	while(compareType(SEMICOLON)) {
		readWord();
		Statement();
	}
	if(!compareType(DOT)) {
		Error("Caractère '.' attendu pour terminer le programme.");
	}
	readWord();
}

// Statement := AssignementStatement | IfStatement | WhileStatement | ForStatement | BlockStatement | DisplayStatement
void Statement() {
	if(compareType(ID)) {
		AssignementStatement();
	} else if(compareType(KEYWORD)) {
		if(checkKeyword("IF")) {
			IfStatement();
		} else if (checkKeyword("WHILE")) {
			WhileStatement();
		} else if (checkKeyword("FOR")) {
			ForStatement();
		} else if (checkKeyword("BEGIN")) {
			BlockStatement();
		} else if (checkKeyword("DISPLAY")) {
			DisplayStatement();
		} else {
			Error("Identificateur ou mot-clé attendus.");
		}
	}
}

// AssignementStatement := Identifier ":=" Expression
string AssignementStatement() {
	string variable;
	if(!compareType(ID)) {
		Error("Identificateur attendu");
	}
	if(!IsDeclared(currentWord())) {
		cerr << "Erreur : Variable '" << currentWord() << "' non déclarée" << endl;
		exit(-1);
	}
	variable = currentWord();
	readWord();
	if(!compareType(ASSIGN)) {
		Error("caractères ':=' attendus");
	}
	readWord();
	TYPE expressionType = Expression();
	outputWrite("pop " + variable);
	return(variable);
}

// IfStatement := "IF" Expression "THEN" Statement [ "ELSE" Statement ]
void IfStatement() {
	TagNumber += 1;
	string ifTag = to_string(TagNumber);
	string ifTagString = to_string(TagNumber) + string(":");
	outputWrite("IfLoop" + ifTagString);
	if(!checkKeyword("IF")) {
		Error("Mot clé IF attendu");
	}
	readWord();
	Expression();
	outputWrite("pop %rax", "The result of the comparison is at the stack top");
	outputWrite("cmpq $0, %rax", "Comparison");
	outputWrite("je IfElse" + ifTag, "Jump into the else part if the condition was false");
	if(!checkKeyword("THEN")) {
		Error("Mot clé THEN attendu");
	}
	outputWrite("IfThen" + ifTagString);
	readWord();
	Statement();
	outputWrite("jmp IfEnd" + ifTag, "The condition was true, no need to do the else part");
	outputWrite("IfElse" + ifTagString);
	if(current == KEYWORD) {
		if(compareWord("ELSE")) {
			readWord();
			Statement();
		}
	}
	outputWrite("IfEnd" + ifTagString);
}

// WhileStatement := "WHILE" Expression DO Statement
void WhileStatement() {
	TagNumber += 1;
	string whileTag = to_string(TagNumber);
	string whileTagString = to_string(TagNumber) + string(":");
	if(!checkKeyword("WHILE")) {
		Error("Mot clé WHILE attendu");
	}
	outputWrite("WhileLoop" + whileTagString);
	readWord();
	Expression();
	outputWrite("pop %rax");
	outputWrite("cmpq $0, %rax", "If the condition is no longer fulfilled, the loop is finished.");
	outputWrite("je WhileEnd" + whileTag);
	if(!checkKeyword("DO")) {
		Error("Mot clé DO attendu");
	}
	readWord();
	Statement();
	outputWrite("jmp WhileLoop" + whileTag);
	outputWrite("WhileEnd" + whileTagString);
}

// ForStatement := "FOR" AssignementStatement "TO" Expression "DO" Statement
void ForStatement() {
	TagNumber += 1;
	string forTag = to_string(TagNumber);
	string forTagString = to_string(TagNumber) + string(":");
	if(!checkKeyword("FOR")) {
		Error("Mot clé FOR attendu");
	}
	readWord();
	string laVar = AssignementStatement();
	outputWrite("ForLoop" + forTagString);
	bool isTo; // Works as a check to see if we have a TO or a DOWNTO
	if(checkKeyword("TO")) {
		isTo = true;
	} else if(checkKeyword("DOWNTO")) {
		isTo = false;
	} else {
		Error("Mots-clés TO ou DOWNTO attendus.");
	}
	readWord();
	Expression();
	outputWrite("pop %rax");
	outputWrite("push " + laVar);
	outputWrite("pop %rbx");
	outputWrite("cmpq %rbx, %rax", "If we got beyond the stopping point, we stop");
	if(isTo) {
		outputWrite("jb ForEnd" + forTag, "TO");
	} else {
		outputWrite("ja ForEnd" + forTag, "DOWNTO");
	}
	if(!checkKeyword("DO")) {
		Error("Mot clé DO attendu");
	}
	readWord();
	Statement();
	outputWrite("push " + laVar);
	outputWrite("pop %rax");
	if(isTo) {
		outputWrite("addq $1, %rax");
	} else {
		outputWrite("subq $1, %rax");
	}
	outputWrite("push %rax");
	outputWrite("pop " + laVar, "New value of variable for next check");
	outputWrite("jmp ForLoop" + forTag);
	outputWrite("ForEnd" + forTagString);
}

// BlockStatement := "BEGIN" Statement { ";" Statement } "END"
void BlockStatement() {
	if(!checkKeyword("BEGIN")) {
		Error("Mot clé BEGIN attendu");
	}
	readWord();
	Statement();
	while(compareType(SEMICOLON)) {
		readWord();
		Statement();
	}
	if(!checkKeyword("END")) {
		Error("Mot clé END attendu");
	}
	readWord();
}

// PrintStatement := "DISPLAY" Expression
void DisplayStatement() {
	if(!checkKeyword("DISPLAY")) {
		Error("Mot clé DISPLAY attendu");
	}
	readWord();
	TYPE typeDisplay = Expression();
	if(typeDisplay != UNSIGNED_INT) {
		Error("Entier attendu.");
	}
	outputWrite("pop %rdx", "The value to be displayed");
	outputWrite("movq $FormatString1, %rsi", "\"%llu\\n\"");
	outputWrite("movl $1, %edi");
	outputWrite("movl $0, %eax");
	outputWrite("call __printf_chk@PLT");
}

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
TYPE Expression() {
	OPREL oprel;
	TYPE aReturn;
	aReturn = SimpleExpression();
	if(compareType(RELOP)){
		oprel = RelationalOperator();
		TYPE newReturn = SimpleExpression();
		if(aReturn != newReturn) {
			Error("Erreur de type incompatibles (Expression)");
		}
		outputWrite("pop %rax");
		outputWrite("pop %rbx");
		outputWrite("cmpq %rax, %rbx");
		TagNumber += 1;
		string expTag = to_string(TagNumber);
		string expTagString = to_string(TagNumber) + string(":");
		switch(oprel){
			case EQU:
				outputWrite("je Vrai" + expTag, "If equal");
				break;
			case DIFF:
				outputWrite("jne Vrai" + expTag, "If different");
				break;
			case SUPE:
				outputWrite("jae Vrai" + expTag, "If above or equal");
				break;
			case INFE:
				outputWrite("jbe Vrai" + expTag, "If below or equal");
				break;
			case INF:
				outputWrite("jb Vrai" + expTag, "If below");
				break;
			case SUP:
				outputWrite("ja Vrai" + expTag, "If above");
				break;
			default:
				Error("Opérateur de comparaison inconnu");
		}
		outputWrite("push $0", "False");
		outputWrite("jmp Suite" + expTag);
		outputWrite("Vrai" + expTagString);
		outputWrite("push $0xFFFFFFFFFFFFFFFF", "True");
		outputWrite("Suite" + expTagString);
		return(BOOLEAN);
	} else {
		return(aReturn);
	}
}

// SimpleExpression := Term {AdditiveOperator Term}
TYPE SimpleExpression() {
	OPADD adop;
	TYPE aReturn;
	aReturn = Term();
	while(compareType(ADDOP)){
		adop = AdditiveOperator();		// Save operator in local variable
		TYPE newReturn = Term();
		if(aReturn != newReturn) {
			Error("Erreur de types non compatibles (SimpleExpression)");
		}
		outputWrite("pop %rbx", "First operand");
		outputWrite("pop %rax", "Second operand");
		switch(adop){
			case OR:
				outputWrite("addq %rbx, %rax", "OR");
				break;			
			case ADD:
				outputWrite("addq %rbx, %rax", "ADD");
				break;			
			case SUB:
				outputWrite("subq %rbx, %rax", "SUB");
				break;
			default:
				Error("opérateur additif inconnu");
		}
		outputWrite("push %rax");
	}
	return(aReturn);
}

// Term := Factor {MultiplicativeOperator Factor}
TYPE Term() {
	OPMUL mulop;
	TYPE aReturn;
	aReturn = Factor();
	while(current==MULOP){
		mulop=MultiplicativeOperator();		// Save operator in local variable
		TYPE newReturn = Factor();
		if(aReturn != newReturn) {
			Error("Types non compatibles (Term).");
		}
		cout << "\tpop %rbx"<<endl;	// get first operand
		cout << "\tpop %rax"<<endl;	// get second operand
		switch(mulop){
			case AND:
				cout << "\tmulq	%rbx"<<endl;	// a * b -> %rdx:%rax
				cout << "\tpush %rax\t# AND"<<endl;	// store result
				break;
			case MUL:
				cout << "\tmulq	%rbx"<<endl;	// a * b -> %rdx:%rax
				cout << "\tpush %rax\t# MUL"<<endl;	// store result
				break;
			case DIV:
				cout << "\tmovq $0, %rdx"<<endl; 	// Higher part of numerator  
				cout << "\tdiv %rbx"<<endl;			// quotient goes to %rax
				cout << "\tpush %rax\t# DIV"<<endl;		// store result
				break;
			case MOD:
				cout << "\tmovq $0, %rdx"<<endl; 	// Higher part of numerator  
				cout << "\tdiv %rbx"<<endl;			// remainder goes to %rdx
				cout << "\tpush %rdx\t# MOD"<<endl;		// store result
				break;
			default:
				Error("opérateur multiplicatif attendu");
		}
	}
	return(aReturn);
}

TYPE Factor(void) {
	TYPE aReturn;
	if(compareType(RPARENT)) {
		readWord();
		aReturn = Expression();
		if(!compareType(LPARENT)) {
			Error("')' était attendu");		// ")" expected
		} else {
			readWord();
		}
	} else {
		if (compareType(NUMBER)) {
			aReturn = Number();
		} else {
				if(compareType(ID))
					aReturn = Identifier();
				else
					Error("'(' ou chiffre ou lettre attendue");
			}
	}
	return aReturn;
}

TYPE Number(void) {
	outputWrite("push $" + currentWord());
	readWord();
	return(UNSIGNED_INT);
}

// AdditiveOperator := "+" | "-" | "||"
OPADD AdditiveOperator(void){
	OPADD opadd;
	if(strcmp(lexer->YYText(),"+")==0)
		opadd=ADD;
	else if(strcmp(lexer->YYText(),"-")==0)
		opadd=SUB;
	else if(strcmp(lexer->YYText(),"||")==0)
		opadd=OR;
	else opadd=WTFA;
	current=(TOKEN) lexer->yylex();
	return opadd;
}

// MultiplicativeOperator := "*" | "/" | "%" | "&&"
OPMUL MultiplicativeOperator(void){
	OPMUL opmul;
	if(strcmp(lexer->YYText(),"*")==0)
		opmul=MUL;
	else if(strcmp(lexer->YYText(),"/")==0)
		opmul=DIV;
	else if(strcmp(lexer->YYText(),"%")==0)
		opmul=MOD;
	else if(strcmp(lexer->YYText(),"&&")==0)
		opmul=AND;
	else opmul=WTFM;
	current=(TOKEN) lexer->yylex();
	return opmul;
}

// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
OPREL RelationalOperator(void){
	OPREL oprel;
	if(strcmp(lexer->YYText(),"==")==0)
		oprel=EQU;
	else if(strcmp(lexer->YYText(),"!=")==0)
		oprel=DIFF;
	else if(strcmp(lexer->YYText(),"<")==0)
		oprel=INF;
	else if(strcmp(lexer->YYText(),">")==0)
		oprel=SUP;
	else if(strcmp(lexer->YYText(),"<=")==0)
		oprel=INFE;
	else if(strcmp(lexer->YYText(),">=")==0)
		oprel=SUPE;
	else oprel=WTFR;
	current=(TOKEN) lexer->yylex();
	return oprel;
}

TYPE Identifier(void) {
	outputWrite("push " + currentWord());
	readWord();
	return(UNSIGNED_INT);
}