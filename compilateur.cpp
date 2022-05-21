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
#include <map>
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
		if(s.find(":") == -1) {
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
	enum OPREL {EQU, DIFF, INF, SUP, INFE, SUPE, WTFR}; // Relationnal Operators
	enum OPADD {ADD, SUB, OR, WTFA}; // Additionnal Operators
	enum OPMUL {MUL, DIV, MOD, AND ,WTFM}; // Multiplicative Operators
	enum TYPE {INTEGER, BOOLEAN, DOUBLE, CHAR}; // Data types
	map<string, TYPE> DeclaredVariables; // Set of declared variables
	// Prototypes
	bool IsDeclared(string id); // Checks if a variable was declared during the declaration part
	void Error(string s); // Stops the compiler and returns an error to the terminal at current line
	bool checkKeyword(const char* key); // Checks if currently read word is a keyword, and checks if it matches a given key
	// Functions
	bool IsDeclared(string id) {
		return DeclaredVariables.find(id) != DeclaredVariables.end();
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
	// VarDeclarationPart := "VAR" VarDeclaration {";" VarDeclaration} "."
	void VarDeclarationPart();
	// VarDeclaration := Ident {"," Ident} ":" Type
	void VarDeclaration();
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
	TYPE CharConst();
	TYPE Type();

int main() { // Source code on standard input, Output on standard output
	outputWrite("", "This code was produced by the CERI compiler"); // Header
	outputWrite(".data");
	outputWrite(".align 8");
	outputWrite("FormatInteger:	.string \"%llu\"", "To display 64-bit unsigned integers");
	outputWrite("FormatDouble: .string \"%lf\"", "To display 64-bit doubles");
	outputWrite("FormatChar: .string \"%c\"", "To display 8-bit chars");
	outputWrite("FormatTrue: .string \"TRUE\"", "To display the boolean TRUE");
	outputWrite("FormatFalse: .string \"FALSE\"", "To display the boolean FALSE");
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
	if(checkKeyword("VAR")) {
		VarDeclarationPart();
	}
	StatementPart();
}

// VarDeclarationPart := "VAR" VarDeclaration {";" VarDeclaration} "."
void VarDeclarationPart() {
	if(!checkKeyword("VAR")) {
		Error("Mot clé 'VAR' attendu.");
	}
	readWord();
	VarDeclaration();
	while(compareType(SEMICOLON)) {
		readWord();
		VarDeclaration();
	}
	if(!compareType(DOT)) {
		Error("'.' attendu.");
	}
	readWord();
}

// VarDeclaration := Ident {"," Ident} ":" Type
void VarDeclaration() {
	set<string> variableList;
	if(!compareType(ID)) {
		Error("Identificateur attendu.");
	}
	variableList.insert(currentWord());
	readWord();
	while(compareType(COMMA)) {
		readWord();
		variableList.insert(currentWord());
		readWord();
	}
	if(!compareType(COLON)) {
		Error("':' attendu.");
	}
	readWord();
	if(!compareType(KEYWORD)) {
		Error("Type attendu.");
	}
	for(set<string>::iterator iterateur = variableList.begin(); iterateur != variableList.end(); ++iterateur) {
		if(compareWord("INTEGER")) {
			DeclaredVariables.insert({*iterateur, INTEGER});
			outputWrite(*iterateur + ":\t.quad 0");
		} else if(compareWord("BOOLEAN")) {
			DeclaredVariables.insert({*iterateur, BOOLEAN});
			outputWrite(*iterateur + ":\t.quad 0");
		} else if(compareWord("DOUBLE")) {
			DeclaredVariables.insert({*iterateur, DOUBLE});
			outputWrite(*iterateur + ":\t.double 0.0");
		} else if(compareWord("CHAR")) {
			DeclaredVariables.insert({*iterateur, CHAR});
			outputWrite(*iterateur + ":\t.byte 0");
		} else {
			Error("Mauvais type donné.");
		}
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
	variable = currentWord(); TYPE type = DeclaredVariables[variable];
	readWord();
	if(!compareType(ASSIGN)) {
		Error("caractères ':=' attendus");
	}
	readWord();
	TYPE expressionType = Expression();
	if(type != expressionType) {
		cerr << "Type de la variable : " << type << endl;
		cerr << "Type de l'expression " << expressionType << endl;
		Error("Types incompatibles dans l'affectation.");
	}
	if(type == CHAR) {
		outputWrite("pop %rax");
		outputWrite("movb %al, " + variable);
	} else {
		outputWrite("pop " + variable);
	}
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
	TYPE type = Expression();
	if(type != BOOLEAN) {
		Error("Expression booléenne requise pour le IF.");
	}
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
	TYPE type = Expression();
	if(type != BOOLEAN) {
		Error("L'expression doit être booléenne pour le WHILE");
	}
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
	int valeurStep;
	if(checkKeyword("STEP")) {
		readWord();
		valeurStep = stoi(currentWord());
		readWord();
	} else {
		valeurStep = 1;
	}
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
	string endIncr = ", %rax";
	if(isTo) {
		outputWrite("addq $" + to_string(valeurStep) + endIncr);
	} else {
		outputWrite("subq $" + to_string(valeurStep) + endIncr);
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
	unsigned long long displayTag = ++TagNumber;

	switch(typeDisplay) {
		case INTEGER:
			outputWrite("pop %rsi", "The value to be displayed");
			outputWrite("movq $FormatInteger, %rdi");
			outputWrite("movl $0, %eax");
			outputWrite("call printf@PLT");
			break;
		case BOOLEAN:
			outputWrite("pop %rdx");
			outputWrite("cmpq $0, %rdx");
			outputWrite("je False" + displayTag);
			outputWrite("movq $FormatTrue, $rdi");
			outputWrite("jmp Next" + displayTag);
			outputWrite("False" + displayTag + string(":"));
			outputWrite("movq $FormatFalse, %rdi");
			outputWrite("Next" + displayTag + string(":"));
			outputWrite("call puts@PLT");
			break;
		case DOUBLE:
			outputWrite("movsd (%rsp), %xmm0");
			outputWrite("subq $16, %rsp");
			outputWrite("movsd %xmm0, 8(%rsp)");
			outputWrite("movq $FormatDouble, %rdi");
			outputWrite("movq $1, %rax");
			outputWrite("call printf");
			outputWrite("nop");
			outputWrite("addq $24, %rsp");
			break;
		case CHAR:
			outputWrite("pop %rsi");
			outputWrite("movq $FormatChar, %rdi");
			outputWrite("movl $0, %eax");
			outputWrite("call printf@PLT");
			break;
		default:
			Error("Type invalide (DISPLAY)");
	}
}

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
TYPE Expression() {
	OPREL oprel;
	TYPE aReturn = SimpleExpression();
	if(compareType(RELOP)){
		oprel = RelationalOperator();
		TYPE newReturn = SimpleExpression();
		if(aReturn != newReturn) {
			Error("Erreur de type incompatibles (Expression)");
		}
		if(aReturn == DOUBLE) {
			outputWrite("fldl (%rsp)");
			outputWrite("fldl 8(%rsp)");
			outputWrite("addq $16, %rsp");
			outputWrite("fcomip %st(1)");
			outputWrite("faddp %st(1)");
		} else {
			outputWrite("pop %rax");
			outputWrite("pop %rbx");
			outputWrite("cmpq %rax, %rbx");
		}
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
		switch(adop){
			case OR:
				if(newReturn != BOOLEAN) {
					Error("Booléen requis pour l'opérateur OR");
				}
				outputWrite("pop %rbx");
				outputWrite("pop %rax");
				outputWrite("orq %rbx, %rax", "OR");
				outputWrite("push %rax");
				break;			
			case ADD:
				if(newReturn != INTEGER && newReturn != DOUBLE) {
					Error("Entier ou Double requis pour l'addition");
				}
				if(newReturn == INTEGER) {
					// Adding integers
					outputWrite("pop %rbx");
					outputWrite("pop %rax");
					outputWrite("addq %rbx, %rax", "ADD");
					outputWrite("push %rax"); // Store result
				} else {
					// Adding doubles
					outputWrite("fldl 8(%rsp)");
					outputWrite("fldl (%rsp)");
					outputWrite("faddp %st(0), %st(1)");
					outputWrite("fstpl 8(%rsp)");
					outputWrite("addq %8, %rsp"); // Result on stack top
				}
				break;			
			case SUB:
				if(newReturn != INTEGER && newReturn != DOUBLE) {
					Error("Entier ou Double requis pour la soustraction");
				}
				if(newReturn == INTEGER) {
					// Subbing integers
					outputWrite("pop %rbx");
					outputWrite("pop %rax");
					outputWrite("subq %rbx, %rax", "SUB");
					outputWrite("push %rax"); // Store result
				} else {
					// Subbing doubles
					outputWrite("fldl (%rsp)");
					outputWrite("fldl 8(%rsp)");
					outputWrite("fsubq %st(0), %st(1)");
					outputWrite("fstpl 8(%rsp)");
					outputWrite("addq %8, %rsp"); // Result on stack top
				}
				break;
			default:
				Error("Opérateur Additif (OPADD) attendu.");
		}
	}
	return(aReturn);
}

// Term := Factor {MultiplicativeOperator Factor}
TYPE Term() {
	TYPE aReturn; OPMUL mulop;
	aReturn = Factor();
	while(compareType(MULOP)){
		mulop = MultiplicativeOperator();		// Save operator in local variable
		TYPE newReturn = Factor();
		if(aReturn != newReturn) {
			Error("Types non compatibles (Term).");
		}
		switch(mulop){
			case AND:
				if(newReturn != BOOLEAN) {
					Error("Type non booléen pour l'opérateur AND.");
				}
				outputWrite("pop %rbx", "First operand");
				outputWrite("pop %rax", "Second operand");
				outputWrite("mulq %rbx");
				outputWrite("push %rax", "AND"); // Store result
				break;
			case MUL:
				if(newReturn != INTEGER && newReturn != DOUBLE) {
					Error("Entier ou Double requis pour la multiplication.");
				}
				if(newReturn == INTEGER) {
					// Multiplying integers
					outputWrite("pop %rbx", "First operand");
					outputWrite("pop %rax", "Second operand");
					outputWrite("mulq %rbx");
					outputWrite("push %rax", "MUL"); // Store result
				} else {
					// Multiplying doubles
					outputWrite("fldl 8(%rsp)", "First operand in st(0)");
					outputWrite("fldl (%rsp)", "Second operand in st(1)");
					outputWrite("fmulp %st(0), %st(1)");
					outputWrite("fstpl 8(%rsp)");
					outputWrite("addq $8, %rsp", "MUL"); // Result is on stack top
				}
				break;
			case DIV:
				if(newReturn != INTEGER && newReturn != DOUBLE) {
					Error("Entier ou Double requis pour la division.");
				}
				if(newReturn == INTEGER) {
					// Dividing integers
					outputWrite("pop %rbx", "First operand");
					outputWrite("pop %rax", "Second operand");
					outputWrite("movq $0, %rdx");
					outputWrite("div %rbx");
					outputWrite("push %rax", "DIV"); // Store quotient
				} else {
					// Dividing doubles
					outputWrite("fldl (%rsp)", "First operand in st(0)");
					outputWrite("fldl 8(%rsp)", "Second operand in st(1)");
					outputWrite("fdivp %st(0), %st(1)");
					outputWrite("fstpl 8(%rsp)");
					outputWrite("addq $8, %rsp", "DIV"); // Quotient is on stack top
				}
				break;
			case MOD:
				if(newReturn != INTEGER) {
					Error("Entier requis pour le modulo.");
				}
				outputWrite("movq $0, %rdx");
				outputWrite("div %rbx"); // Remainder goes to %rdx
				outputWrite("push %rdx", "MOD"); // Store remainder
				break;
			default:
				Error("Opérateur Multiplicatif (OPMUL) attendu.");
		}
	}
	return(aReturn);
}

TYPE Factor() {
	TYPE aReturn;
	switch(current) {
		case RPARENT:
			readWord();
			aReturn = Expression();
			if(current != LPARENT) {
				Error("')' attendu.");
			}
			readWord();
			break;
		case NUMBER:
			aReturn = Number();
			break;
		case ID:
			aReturn = Identifier();
			break;
		case CHARCONST:
			aReturn = CharConst();
			break;
		default:
			Error("'(' ou constante ou variable attendue.");
	}
	return aReturn;
}

TYPE Number() {
	string number = currentWord();
	if(number.find(".") != string::npos) {
		// We have a double
		double d = stof(currentWord());
		unsigned int *i = (unsigned int *) &d; // i points to the const double
		outputWrite("subq $8, %rsp");
		outputWrite("movl $" + to_string(*i) + string(", (%rsp)"), "Conversion of " + to_string(d) + string(" (32-bit high part)"));
		outputWrite("movl $" + to_string(*(i+1)) + string(", 4(%rsp)"), "Conversion of " + to_string(d) + string(" (32-bit low part)"));
		readWord();
		return DOUBLE;
	} else {
		// We have an integer
		outputWrite("push $" + currentWord());
		readWord();
		return INTEGER;
	}
}

// AdditiveOperator := "+" | "-" | "||"
OPADD AdditiveOperator() {
	OPADD opadd;
	if(compareWord("+")) {
		opadd = ADD;
	} else if(compareWord("-")) {
		opadd = SUB;
	} else if(compareWord("||")) {
		opadd = OR;
	} else {
		opadd = WTFA;
	}
	readWord();
	return(opadd);
}

// MultiplicativeOperator := "*" | "/" | "%" | "&&"
OPMUL MultiplicativeOperator() {
	OPMUL opmul;
	if(compareWord("*")) {
		opmul = MUL;
	} else if(compareWord("/")) {
		opmul = DIV;
	} else if(compareWord("%")) {
		opmul = MOD;
	} else if(compareWord("&&")) {
		opmul = AND;
	} else {
		opmul = WTFM;
	}
	readWord();
	return(opmul);
}

// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
OPREL RelationalOperator() {
	OPREL oprel;
	if(compareWord("==")) {
		oprel = EQU;
	} else if(compareWord("!=")) {
		oprel = DIFF;
	} else if(compareWord("<")) {
		oprel = INF;
	} else if(compareWord(">")) {
		oprel = SUP;
	} else if(compareWord("<=")) {
		oprel = INFE;
	} else if(compareWord(">=")) {
		oprel = SUPE;
	} else {
		oprel = WTFR;
	}
	readWord();
	return oprel;
}

TYPE Identifier() {
	TYPE type;
	if(!IsDeclared(lexer->YYText())) {
		Error("Variable " + currentWord() + " non déclarée");
	}
	type = DeclaredVariables[lexer->YYText()];
	outputWrite("push " + currentWord());
	readWord();
	return(type);
}

TYPE CharConst() {
	outputWrite("movq $0, %rax");
	outputWrite("movb $" + currentWord() + string(", %al"));
	outputWrite("push %rax", "Push a 64 bit version of " + currentWord());
	readWord();
	return(CHAR);
}

TYPE Type() {
	if(current != KEYWORD) {
		Error("Type attendu.");
	}
	TYPE type;
	readWord();
	if(compareWord("BOOLEAN")) {
		type = BOOLEAN;
	} else if(compareWord("INTEGER")) {
		type = INTEGER;
	} else if(compareWord("DOUBLE")) {
		type = DOUBLE;
	} else if(compareWord("CHAR")) {
		type = CHAR;
	} else {
		Error("Type inconnu.");
	}
	return(type);
}