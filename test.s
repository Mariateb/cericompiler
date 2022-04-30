		# This code was produced by the CERI compiler
.data
.align 8
FormatString1:	.string "%llu\n"
a:	.quad 0
b:	.quad 0
	.text								# The following lines contain the program
	.globl main							# The main function must be visible from outside
main:									# Main function body
	movq %rsp, %rbp							# Save the position of the stack's top
push $1
pop a
ForLoop1:
push $10
pop %rax
push a
pop %rbx
	cmpq %rbx, %rax							# If we got beyond the stopping point, we stop
	jb ForEnd1							# TO
push $1
pop b
WhileLoop2:
push b
push a
pop %rax
pop %rbx
cmpq %rax, %rbx
	jb Vrai3							# If below
	push $0								# False
jmp Suite3
Vrai3:
	push $0xFFFFFFFFFFFFFFFF					# True
Suite3:
pop %rax
	cmpq $0, %rax							# If the condition is no longer fulfilled, the loop is finished.
je WhileEnd2
IfLoop4:
push a
push b
	pop %rbx							# First operand
	pop %rax							# Second operand
movq $0, %rdx
div %rbx
	push %rdx							# MOD
push $0
pop %rax
pop %rbx
cmpq %rax, %rbx
	je Vrai5							# If equal
	push $0								# False
jmp Suite5
Vrai5:
	push $0xFFFFFFFFFFFFFFFF					# True
Suite5:
	pop %rax							# The result of the comparison is at the stack top
	cmpq $0, %rax							# Comparison
	je IfElse4							# Jump into the else part if the condition was false
IfThen4:
push b
	pop %rdx							# The value to be displayed
	movq $FormatString1, %rsi					# "%llu\n"
movl $1, %edi
movl $0, %eax
call __printf_chk@PLT
	jmp IfEnd4							# The condition was true, no need to do the else part
IfElse4:
IfEnd4:
push b
push $1
	pop %rbx							# First operand
	pop %rax							# Second operand
	addq %rbx, %rax							# ADD
push %rax
pop b
jmp WhileLoop2
WhileEnd2:
push a
pop %rax
addq $1, %rax
push %rax
	pop a								# New value of variable for next check
jmp ForLoop1
ForEnd1:
	movq %rbp, %rsp							# Restore the position of the stack's top
	ret								# Return from main function
