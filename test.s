									# This code was produced by the CERI compiler
	.data
	.align 8
FormatInteger:	.string "%llu"						# To display 64-bit unsigned integers
FormatDouble: .string "%lf"						# To display 64-bit doubles
FormatChar: .string "%c"						# To display 8-bit chars
FormatTrue: .string "TRUE"						# To display the boolean TRUE
FormatFalse: .string "FALSE"						# To display the boolean FALSE
a:	.quad 0
b:	.quad 0
c:	.byte 0
d:	.double 0.0
	.text								# The following lines contain the program
	.globl main							# The main function must be visible from outside
main:									# Main function body
	movq %rsp, %rbp							# Save the position of the stack's top
	subq $8, %rsp
	movl $0, (%rsp)							# Conversion of 7.000000 (32-bit high part)
	movl $1075576832, 4(%rsp)					# Conversion of 7.000000 (32-bit low part)
	pop d
	push d
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
