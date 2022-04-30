		# This code was produced by the CERI compiler
.data
.align 8
FormatString1:	.string "%llu\n"
a:	.quad 0
	.text								#The following lines contain the program
	.globl main							#The main function must be visible from outside
main:									#Main function body
	movq %rsp, %rbp							#Save the position of the stack's top
	push $8
pop a
ForLoop1:
	push $15
pop %rax
push a
pop %rbx
	cmpq %rbx, %rax							#If we got beyond the stopping point, we stop
	jb ForEnd1							#TO
	push a
	pop %rdx	# The value to be displayed
	movq $FormatString1, %rsi	# '%llu\n'
	movl $1, %edi
	movl $0, %eax
	call __printf_chk@PLT
push a
pop %rax
addq $1, %rax
push %rax
	pop a								#New value of variable for next check
jmp ForLoop1
ForEnd1:
	movq %rbp, %rsp							#Restore the position of the stack's top
	ret								#Return from main function
