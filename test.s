		# This code was produced by the CERI compiler
.data
.align 8
FormatString1:	.string "%llu\n"
a:	.quad 0
	.text								#The following lines contain the program
	.globl main							#The main function must be visible from outside
main:									#Main function body
	movq %rsp, %rbp							#Save the position of the stack's top
	push $4
pop a
WhileLoop1:
	push a
	push $8
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	jb Vrai2	# If below
	push $0		# False
	jmp Suite2
Vrai2:	push $0xFFFFFFFFFFFFFFFF		# True
Suite2:
pop %rax
	cmpq $0, %rax							#If the condition is no longer fulfilled, the loop is finished.
je WhileEnd1
	push a
	pop %rdx	# The value to be displayed
	movq $FormatString1, %rsi	# '%llu\n'
	movl $1, %edi
	movl $0, %eax
	call __printf_chk@PLT
	push a
	push $1
	pop %rbx
	pop %rax
	addq	%rbx, %rax	# ADD
	push %rax
pop a
jmp WhileLoop1
WhileEnd1:
	movq %rbp, %rsp							#Restore the position of the stack's top
	ret								#Return from main function
