									# This code was produced by the CERI compiler
	.data
	.align 8
FormatInteger:	.string "%llu"						# To display 64-bit unsigned integers
FormatDouble: .string "%lf"						# To display 64-bit doubles
FormatChar: .string "%c"						# To display 8-bit chars
FormatTrue: .string "TRUE"						# To display the boolean TRUE
FormatFalse: .string "FALSE"						# To display the boolean FALSE
a:	.quad 0
b:	.double 0.0
	.text								# The following lines contain the program
	.globl main							# The main function must be visible from outside
main:									# Main function body
	movq %rsp, %rbp							# Save the position of the stack's top
	push $0xFFFFFFFFFFFFFFFF
	pop a
IfLoop1:
	push a
	pop %rax							# The result of the comparison is at the stack top
	cmpq $0, %rax							# Comparison
	je IfElse1							# Jump into the else part if the condition was false
IfThen1:
	subq $8, %rsp
	movl $3758096384, (%rsp)					# Conversion of 4.541240 (32-bit high part)
	movl $1074932282, 4(%rsp)					# Conversion of 4.541240 (32-bit low part)
	pop b
	jmp IfEnd1							# The condition was true, no need to do the else part
IfElse1:
	subq $8, %rsp
	movl $3758096384, (%rsp)					# Conversion of 6.666660 (32-bit high part)
	movl $1075489448, 4(%rsp)					# Conversion of 6.666660 (32-bit low part)
	pop b
IfEnd1:
	push b
	subq $8, %rsp
	movl $3758096384, (%rsp)					# Conversion of 4.124500 (32-bit high part)
	movl $1074823036, 4(%rsp)					# Conversion of 4.124500 (32-bit low part)
	fldl 8(%rsp)
	fldl (%rsp)
	faddp %st(0), %st(1)
	fstpl 8(%rsp)
	addq $8, %rsp
	pop b
	push b
	movsd (%rsp), %xmm0
	subq $16, %rsp
	movsd %xmm0, 8(%rsp)
	movq $FormatDouble, %rdi
	movq $1, %rax
	call printf
	nop
	addq $24, %rsp
	movq $0, %rax
	movb $'\n', %al
	push %rax							# Push a 64 bit version of '\n'
	pop %rsi
	movq $FormatChar, %rdi
	movl $0, %eax
	call printf@PLT
	movq %rbp, %rsp							# Restore the position of the stack's top
	ret								# Return from main function
