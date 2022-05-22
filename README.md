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

# The Data Types

There are four data types for a variable :
- INTEGER, an unsigned 64-bit integer
- BOOLEAN, either TRUE (0x0) or FALSE (0xFFFFFFFFFFFFFFFF).
- DOUBLE, a 64-bit floating point number
- CHAR, a 8-bit ASCII value

# The Available Statements

**Assignment Statement :**


Allows you to assign a value to a declared variable. You must follow these rules :
- The variable and the expression must be of the same type
- If the variable is a boolean, you must write TRUE or FALSE instead

Examples :

```
a := 4;
b := 3.5;
c := 'e';
d := FALSE;
```

**If Statement :**

Allows you to check the validity of a condition, and act accordingly. You must follow these rules :
- The condition is an expression of type BOOLEAN.

Examples :

```
IF a == 0 THEN
    DISPLAY a
ELSE
    DISPLAY b;
```

**While Statement :**

This statement is a loop that will keep looping the statements inside it until the condition is no longer met. You must follow these rules :
- The condition is an expression of type BOOLEAN.

Examples :

```
a := 1.0;
WHILE a < 4.5 DO
BEGIN
    DISPLAY a;
    a := a + 1.1
END
```

**For Statement :**

This statement is a loop with an initialisation and a stopping point. It will keep looping and incrementing until the stopping point is hit. You must follow these rules :
- The variable assigned must be an INTEGER.
- You can use TO or DOWNTO to choose between incrementation and decrementation.
- You can use STEP to change the incrementation value.

Examples :
```
FOR a := 4 TO 8 DO
BEGIN
    DISPLAY a;
END;

FOR a := 12 DOWNTO 2 STEP 2 DO
BEGIN
    DISPLAY a;
END
```

**Block Statement :**

This statement's only purpose is to group other statements inside it.

Examples :

```
BEGIN
    DISPLAY a;
    a := b + 3
END;
```

**Case Statement :**

This statement takes a variable (or an expression), and matches it with constants. It there is a match, then the according statement will be executed. You must follow these rules :
- The expression and constants must ALL be of the same type.
- You cannot use DOUBLES in the case statement.

Examples :

```
CASE a + 1 OF
    3: DISPLAY a;
    4,5,6:
        BEGIN
            a := a + 3;
            DISPLAY a;
        END
    7: DISPLAY b
END;
```

**Repeat Statement :**

This statement will loop the statements in it until the condition at the end is met. You must follow these rules :
- The condition must be a BOOLEAN expression.
- There is only one statement in the REPEAT statement. If you want more, use the Block.

Examples :

```
REPEAT
BEGIN
	DISPLAY a;
	a := a + 1
END
UNTIL a == 5;
```

**Display Statement :**

This statement displays the variable or constant given.

Examples :

```
DISPLAY a;
DISPLAY b + 3.1;
DISPLAY 'a';
DISPLAY '\n';
DISPLAY FALSE;
```
