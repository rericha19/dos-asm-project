; macro/util file for proj.asm

; ----------------------------------------------------------------------------------
; takes in number 0 - 99 and prints it
date_time_mac_help macro val
	mov ax, 0
	mov al, val						; dividend has to be in ax before div
	mov bl, 10			
	div bl							; divide, needs a register
	mov bl, '0'						
	add bl, al						; quotient is in al after division
	mov bh, '0'
	add bh, ah						; remainder is in ah after division
	pr_char bl
	pr_char bh
	
endm

; ----------------------------------------------------------------------------------
; in paging input, gets and prints current system date and time
print_curr_date_time macro
	pshutil
	print DAT_TIM
	
	; returns sys date:	cx - year, dh - month, dl - day
	mov ax, 2a00h					; get system date
	int 21h
	
	date_time_mac_help dl
	pr_char '.'
	date_time_mac_help dh
	pr_char '.'
	
	mov ax, cx						; divide year by 100
	mov bl, 100
	div bl
	
	mov dl, al						; after split use 2 macro calls
	mov dh, ah						; to print 2 and 2 digits
	
	date_time_mac_help dl
	date_time_mac_help dh
	pr_char ' '
	
	; returns sys time: ch - hour, cl - minute, dh - second, dl - 1/100 of a second
	mov ax, 2c00h					; get system time
	int 21h
	
	date_time_mac_help ch
	pr_char ':'
	date_time_mac_help cl
	pr_char ':'
	date_time_mac_help dh
	pr_char '.'
	date_time_mac_help dl
	
	print PAG_INF
	poputil
endm

; registers that dont need to be stored are not included in pshutil and poputil
; ----------------------------------------------------------------------------------
; replacement for pusha 
pshutil macro 
	push ax
	push bx
	push cx
	push dx
	
	push ds
	push si
endm

; ----------------------------------------------------------------------------------
; replacement for popa
poputil macro
	pop si
	pop ds

	pop dx
	pop cx
	pop bx
	pop ax
endm

; ----------------------------------------------------------------------------------	
; macro for printing strings
print macro T
	pshutil
	
	mov ax, @DATA
	mov ds, ax
	mov dx, offset T
	mov ah, 09h
	int 21h
	
	poputil
endm

; ----------------------------------------------------------------------------------	
; macro for printing a single character
pr_char macro char
	pshutil
	
	mov dl, char
	mov ah, 2h
	int 21h	
	
	poputil											; to prevent overwriting registersS
endm

; ----------------------------------------------------------------------------------	
; macro for printing line break
pr_line_break macro
	pr_char 0dh
	pr_char 0ah
endm

pr_new_line macro
	pr_char 0ah
endm

; ----------------------------------------------------------------------------------	
; macro for terminating the program
terminate_prog macro
	mov ax, 4c00h
	int 21h
endm

; ----------------------------------------------------------------------------------	
; macro for printing the help text
help_prnt macro
	pr_line_break
	print HELP_1
	print HELP_2
	print HELP_3
	print HELP_4
	print HELP_5
	print HELP_6
	print HELP_7
endm
 
