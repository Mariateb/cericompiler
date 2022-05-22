# CERIcompiler

A simple compiler.
From : Pascal-like imperative LL(k) langage
To : 64 bit 80x86 assembly langage (AT&T)

**Download the repository :**

> git clone https://github.com/Mariateb/cericompiler

**Build the compiler and test it :**

> make

**Have a look at the output :**

> nano test.s

**Debug the executable :**

> gdb ./test

# The Syntax rules

**The structure of a program :**

> Program := [DeclarationPart] StatementPart
> VarDeclarationPart := "VAR" VarDeclaration {";" VarDeclaration} "."
> VarDeclaration := Ident {"," Ident} ":" Type
> DeclarationPart := "[" Letter {"," Letter} "]"
> StatementPart := Statement {";" Statement} "."

**Statements :**

> Statement := AssignementStatement | IfStatement | WhileStatement | ForStatement | BlockStatement
> AssignementStatement := Identifier ":=" "TRUE"|"FALSE"|Expression
> IfStatement := "IF" Expression "THEN" Statement [ "ELSE" Statement ]
> WhileStatement := "WHILE" Expression DO Statement
> ForStatement := "FOR" AssignementStatement "To" Expression "DO" Statement
> BlockStatement := "BEGIN" Statement { ";" Statement } "END"
> CaseStatement := "CASE" Expression OF CaseListElement {; CASE LIST ELEMENT} END
> CaseListElement := CaseLabelList : Statement | Empty
> CaseLabelList := Constant {, Constant}
> RepeatUntilStatement := "REPEAT" Statement "UNTIL" Expression
> DisplayStatement := "DISPLAY" Expression

**Calculations and expressions :**

> Expression := SimpleExpression [RelationalOperator SimpleExpression]
> SimpleExpression := Term {AdditiveOperator Term}
> Term := Factor {MultiplicativeOperator Factor}
> Factor := Number | Letter | "(" Expression ")"| "!" Factor
> Number := Digit{Digit}

**Operators, variables and constants :**

> AdditiveOperator := "+" | "-" | "||"
> MultiplicativeOperator := "*" | "/" | "%" | "&&"
> RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="
> Identifier := Letter{Letter}
> Constant := Number | CharConst

# The Available Statements
