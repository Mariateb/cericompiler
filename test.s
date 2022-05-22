									# This code was produced by the CERI compiler
	.data
	.align 8
FormatInteger:	.string "%llu"						# To display 64-bit unsigned integers
FormatDouble: .string "%lf"						# To display 64-bit doubles
FormatChar: .string "%c"						# To display 8-bit chars
FormatTrue: .string "TRUE"						# To display the boolean TRUE
FormatFalse: .string "FALSE"						# To display the boolean FALSE
	.text								# The following lines contain the program
	.globl main							# The main function must be visible from outside
main:									# Main function body
	movq %rsp, %rbp							# Save the position of the stack's top
	push $0xFFFFFFFFFFFFFFFF
	pop %rdx
	movq $FormatTrue, %rdi
	call puts@PLT
	movq %rbp, %rsp							# Restore the position of the stack's top
	ret								# Return from main function
