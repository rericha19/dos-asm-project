; program for finding lines in a txt file that contain substring provided 
; either via another txt file, or using keyboard input
; author: Samuel Řeřicha
; 2020/21 STU FIIT project 1, subject SPaASM
; sources: 	various asm source files, 
;			https://en.wikipedia.org/wiki/DOS_API#DOS_INT_21h_services
; 			http://spike.scu.edu.au/~barry/interrupts.html
;			https://www.en.w3ki.com/assembly_programming/assembly_cmps_instruction.html

include proj_m.inc

PAGING_LINES EQU 24

name proj1_rericha
.model small
.stack 100h
.data 	
	char_var	db 0
	buffer 		db 1 				; for reading from file, stores read data
	buf_ptr 	dw ?				; for reading from file, ptr to buffer
	filehandle  dw ?				; for reading from file, file handle
	file_seg	dw 0				; for reading from file, for current offset calculation
	p_index 	db -1				; index of path currently read into (from cmd line)
	arg_dsh 	db 1				; is current arg a dash arg (eg -h ?)
	
	; these 3 have to be like this because of how i use service 0ah in read_searched_from_k
	srched_s_ml	db 81				; max length of string read by 0ah when using keyboard input
	searched_l	db ?				; after 0ah call contains length of string read
	searched_s  db 60h dup('$')		; stores string to be looked for
	
	curr_line	db 81 dup(' ')		; array for storing currently read line
	lb_reach	db 0				; used for skipping \n
	print_cond	db 0				; variable, controls whether to print curr_line
	pagin_index db 0
	
	f_2_src_cnt db ?
	paged_p		db 0				; var for whether the print is paged
	print_h 	db 0				; var for whether to print help or not
	from_k  	db 0				; var for whether to use keyboard input
	path_0 		db 80h dup(0)		; array for storing the first path
	path_1 		db 80h dup(0)		; array for storing the second path
	
	; strings that make up the help print
	HELP_1 DB "Usage: <prog_name> [-h] [-k] [-p] FILE_TO_SEARCH [FILE_WITH_TEXT_TO_SEARCH]", 0dh, 0ah, '$'	
	HELP_2 DB "       arguments in [] brackets are optional", 0dh, 0ah, '$'
	HELP_3 DB "       -h: prints help text for the project (you're reading it now)", 0dh, 0ah, '$'
	HELP_4 DB "       -k: text to be searched is input using keyboard instead", 0dh, 0ah, '$'
	HELP_5 DB "       -p: paged, when screen is filled pause until N, ESC or ENTER is pressed", 0dh, 0ah, '$'
	HELP_6 DB "       FILE_TO_SEARCH_IN: path to file with text to search in, txtfile", 0dh, 0ah, '$'
	HELP_7 DB "       FILE_WITH_TEXT_TO_SEARCH: last path provided, not used if -k is used", 0dh, 0ah, '$'

	SEARCH		DB "Substring found in following lines: ", 0dh, 0ah, '$'
	SRC_S_INF	DB "String to be searched: ", 0dh, 0ah, '$'
	P_CNT_ERR 	DB "Unexpected path count (expecting 1)", 0dh, 0ah, '$'
	P_CNT_ERR2 	DB "Unexpected path count (expecting 2)", 0dh, 0ah, '$'
	LEN_ERR 	DB "Searched string is empty! Provide a non-empty string", 0dh, 0ah, '$'
	KEY_PRT 	DB "Input string to be searched:", 0dh, 0ah, '$'
	PAG_INF 	DB "   |   Press N, ESC or ENTER to continue  $"
	DAT_TIM		DB "Date and time: $"
	UNK_DSH 	DB "Unknown argument -$"
	WRD_DSH 	DB "Invalid argument format (things like -kp are not allowed, use -k -p)$"
	K_STR   	DB "Searched string will be read from the keyboard", 0dh, 0ah, '$'
	NO_ARG 		DB "No arguments provided!", 0dh, 0ah, '$'	; no arg warning string
	FILEERR 	DB "Couldnt open input file", 0dh, 0ah, '$'
	FILEERR2	DB "Couldnt open file with string", 0dh, 0ah, '$'
	
	;FILEBIG 	DB 0dh, 0ah, "File size limit (65535 B) reached, stopping", 0dh, 0ah, '$'
	;DBG 		DB "-----DEBUG PRINT------", 0dh, 0ah, '$'	; debug string i use
.code

; ----------------------------------------------------------------------------------
; checks whether to print the current line 
str_check proc
	pshutil							; save registers
	push di							
	push es
	
	mov ax, ds						; set es to ds
	mov es, ax
	
	mov cx, 0						; if current line is shorter than 
	mov cl, searched_l				; searched string dont compare
	cmp di, cx						; needed cuz cmps 
	jl str_check_end				; incorrectly returns match in those cases
	
	cmp di, 81						; hacky, prevents one extra char from being
	jnz no_adjustment				; considered
	dec di
	
	no_adjustment:
	mov cx, di						; loop from 0 to line_len - searched_l + 1
	sub cl, searched_l				
	inc cl
	mov dx, 0						; iterator
	
	seek_match:
		push cx						; store cx, cuz both loop and cmpsb use it
		
		mov si, offset curr_line	; move address of curr_line 
		add si, dx					; including offset of the current char
									; to ds:di, which is used in cmpsb
		mov di, offset searched_s	; move address of searched_s to es:di
		mov ch, 0					
		mov cl, searched_l			; set cx to length of substring to compare
		rep cmpsb 					; use cmpsb
		jz string_match				; zeroflag is 0 if strings matched
		
		inc dx
		pop cx						; restore cx for loop
	loop seek_match
	jmp str_check_end				; no match was found
	
	string_match:					; substring matched
		pop cx						; if jump out happened, pop cx in the loop didnt
		mov print_cond, 1			; turns on printing of current line
	
	str_check_end:
	pop es							; restore registers
	pop di
	poputil
	ret
str_check endp

; ----------------------------------------------------------------------------------

pagin_input proc
	pshutil
	print_curr_date_time
	
	wait_input:						; loop until matching input is done
		mov ax, 700h				; read from stdin, no echo
		int 21h
	
		cmp al, 'n'
		jz skip
		cmp al, 'N'
		jz skip
		cmp al, 0dh					; enter
		jz skip
		cmp al, 1bh					; escape
		jz skip
	jmp wait_input
	
	skip:
	pr_char ' '						; go to new line
	poputil
	ret
pagin_input endp

; ----------------------------------------------------------------------------------
; prints if condition (searched_s is a substring of curr_line) is met
; di has current line length
; searched_l has length of searched sting
cond_print proc
	pshutil
	mov ax, @data
	mov ds, ax
	mov print_cond, 0				; reset print condition
	
	call str_check				
	
	cmp print_cond, 0				; if print condition is off, dont print, exit
	jz cond_print_end
	mov cx, 80						; print is on, print curr_line
	mov dx, 0
	
	; the print is a bit sketchy but it works
	print_loop:
		mov ax, offset curr_line
		add ax, dx
		mov bx, ax
		mov ax, ds:[bx]
		pr_char al
		inc dx
	loop print_loop
	
	cmp paged_p, 0					; if paged print is off, just end
	jz cond_print_end
	
	inc pagin_index					; if paging index reaches limit
	cmp pagin_index, PAGING_LINES
	jnz cond_print_end
	
	mov pagin_index, 0				; reset paging index
	call pagin_input				; call function that solicits input before continuing
			
	cond_print_end:
	poputil
	ret
cond_print endp

; ----------------------------------------------------------------------------------
; procedure that handles searched file, its 'sent' characters of it one by one 
; using register dl
main_print_handler proc
	pshutil
	mov lb_reach, 0
	cmp di, 81						; if line length limit was reached, dont store
	jnz store
	jmp check_lb
	
	store:
		cmp dl, 0dh					; line breaks dont get stored
		jz lb_reached					
		mov ax, @DATA				; store current char into curr_line
		mov ds, ax					; ds still pointing to psp
		mov bx, offset curr_line		
		add bx, di					; write dl into di-th character of curr_line
		mov ds:[bx], dl
		inc di
	
	check_lb:	
		cmp dl, 0dh					; if line break was reached
		jz lb_reached
		jmp lb_not_reached
	
	; line break was reached
	lb_reached:
		call cond_print				; checks whether to print, prints if yes
		
		mov ax, ds					; stosb copies a value into an array
		mov es, ax					; starts from es:di
		mov di, offset curr_line
		mov ax, ' '					; ax holds the character to copy
		mov cx, 81 					; cx holds the count of characters to copy
		rep stosb					; filling curr_line with spaces 'erases' it
		mov di, 0					; reset di, I CAN WORK WITH DI CUZ pop/pshutil 
		mov lb_reach, 1				; doesnt pop/push di
		
	lb_not_reached:					; not reached, keep reading
		poputil
	ret
main_print_handler endp

; ----------------------------------------------------------------------------------
; procedure used to open txt files and reading it char by char, calling the main
; print handler
; also checks whether it can be opened and all that
; can only do 64kB files cuz 16b registers and i dont know how to work around that
; doesnt crash on them, just stops
read_file proc 
	pshutil
   
	mov si, 0						; si is used as offset in the currently read file		
	mov di, 0						; di is used as indexer for current line
	mov ax, @data					; in case ds still pointing to psp
	mov ds, ax

	lea dx, buffer					; buf_ptr contains pointer to buffer
	mov buf_ptr, dx
	mov ax, 3d00h 					; open the file (3d) for reading (00)
	lea dx, path_0 					; path to file to be opened, zerobyte terminated string
	int 21h 
	mov [filehandle], ax 			; store file handle
	jc couldnt_open					; if carry is set, an error occured
   
	read_loop:
		mov ax, 3f00h  				; read from file (3f)
		lea dx, buffer[0]			; where to store read data
		mov cx, 1 					; number of bytes to read
		mov bx, [filehandle] 		; file handle
		int 21h
		jc couldnt_open				; cf is set on error

		add si, 1					; ax contains number of bytes read
		mov cx, ax					; no bytes are read if EOF was reached
		jcxz end_of_file
   
		cmp si, 0					; si reached 0, means 64k bytes were read, new 'seg'
		jnz not_new_fseg
		inc file_seg

		not_new_fseg:
		mov ax, 3f00h 				; not sure why 2 reads are needed but here they are
		lea dx, buf_ptr 			
		mov cx, 1
		mov bx, [filehandle] 		
		int 21h
   
		mov dl, [buffer]			; the function is passed chars one by one								
		call main_print_handler		; main function for print logic
		cmp lb_reach, 1				; hacky, when lb (i mean \r, CR) was reached
		jnz temp				
		inc si						; inc si to skip \n (LF)
		
		temp:
		mov ax, 4200h				; seek (42), relative to start of file (00)
		mov bx, [filehandle]		; file handle
		mov cx, file_seg			; seek goes to cx:dx, 64k * cx + dx
		mov dx, si					
		int 21h
   
		;cmp buf_ptr, 0				
		;je end_of_file
	jmp read_loop
   
	;file_size_limit_reached:		; print warning that size limit got reached
	;	print FILEBIG				; unused
	;	jmp end_of_file
   
	couldnt_open:					; print error when file couldnt be opened
		print FILEERR
		terminate_prog
   
	end_of_file:
		call cond_print				; last line doesnt end with \r\n, need to call this
		mov bx, [filehandle]		; move file handle to bx
		mov ax, 3e00h 				; close file
		int 21h
		jmp endr
   
	endr:
	poputil
  
	ret
read_file endp


; ----------------------------------------------------------------------------------
; for storing file paths read from standard input
store_path proc
	pshutil

	mov ax, @DATA
	mov ds, ax
	
	; according to p_index, store currently read cmd line character
	; into currently handled path array
	cmp p_index, 0					
	jz path0
	cmp p_index, 1
	jz path1
	jmp path_unk
	
	; set bx, which is offset to currently handled path array
	path0:							
		mov bx, offset path_0
		jmp store_path_after
	path1:
		mov bx, offset path_1
		jmp store_path_after
	path_unk:
		jmp store_end
		
	; add character offset, dec by 1 because register si goes from 1 instead of 0
	store_path_after:
	add bx, si
	dec bx
	mov ds:[bx], dl
	
	store_end:
	poputil
	ret
store_path endp

; ----------------------------------------------------------------------------------	
; proc that handles part of cmd line input parsing behaviour
; character 1 of the currently read 'word'
char_handler_one proc
	cmp dl, '-'
	jz is_dash
	jmp isnt_dash
	
	is_dash:						; currently read 'word' is a dash arg
		mov arg_dsh, 1
		jmp char_handler_one_end
		
	isnt_dash:						; currently read 'word' isnt a dash arg
		mov arg_dsh, 0				; start storing path
		call store_path
		jmp char_handler_one_end
		
	char_handler_one_end:
	ret
char_handler_one endp

; ----------------------------------------------------------------------------------	
; proc that handles part of cmd line input parsing behaviour
; character 2 of the currently read 'word'
char_handler_two proc
	pshutil
	
	cmp arg_dsh, 0
	jz not_dash_par
	jmp dash_par
	
	not_dash_par:					; currently read cmd line arg is path
		call store_path				; keep storing that path
		jmp char_handler_two_end
		
	dash_par:						; handling dash arguments
		cmp dl, 'h'					
		jz is_help
		cmp dl, 'k'					
		jz is_rd_from_kboard
		cmp dl, 'p'
		jz is_paged
		jmp unk
		
		is_help:					; -h turns on help print
			mov print_h, 1
			jmp char_handler_two_end
		
		is_rd_from_kboard:			; -k means searched string is input using keyboard
			mov from_k, 1
			jmp char_handler_two_end
			
		is_paged:					; -p turns on paged print
			mov ax, @data
			mov ds, ax
			mov paged_p, 1
			jmp char_handler_two_end
		
		unk:						; unknown dash arg, print error, turn on help print
			print UNK_DSH			
			pr_char dl
			pr_line_break
			mov print_h, 1
			jmp char_handler_two_end
		
	char_handler_two_end:
	poputil
	ret
char_handler_two endp

; ----------------------------------------------------------------------------------	
; proc that handles part of cmd line parsing behaviour
; characters 3+ of the currently read 'word'
char_handler_other proc
	cmp arg_dsh, 1
	jz weird_dash
	jmp path_input
	
	weird_dash:						; only one letter dash args are allowed
		print WRD_DSH				; print error
		pr_line_break
		mov print_h, 1 				; print help, end
		jmp char_handler_other_end
	
	path_input:						; if cmd line arg currently parsed is path
		call store_path				; keep storing that path
		jmp char_handler_other_end
	
	char_handler_other_end:
	ret
char_handler_other endp

; ----------------------------------------------------------------------------------	
; procedure responsible for parsing cmd line arguments 
parse_cmd proc
    mov cl, ds:[0080h]  			; length of cmd line argument		
	jcxz no_args					; if the user specified no args


	mov bx, 80h
	loop_args:
		inc bx						
		mov dl, ds:[bx]				; read next char from cmd args
		;pr_char dl					; print the char
		
		cmp print_h, 1				; if help print got triggered, dont bother with the rest
		jz end_loop
		
		cmp dl, ' '					; spaces are used as argument separators	
		jz 	is_space 
		jmp isnt_space
		
		is_space:					; si is used for indexing in path arguments
			mov si, 0
			jmp char_zero	   
		isnt_space:
			inc si
			
		cmp si, 1
		jz 	char_one
		cmp si, 2
		jz  char_two
		jmp char_other
		
		char_zero:					 
			cmp arg_dsh, 1			; if previous arg wasnt dash arg
			jz after_char_handler_call				
			
			pshutil
			mov ax, @data
			mov ds, ax	
			inc p_index				; increment path index variable
			poputil
			
			jmp after_char_handler_call	
									; call corresponding handler function
		char_one:
			call char_handler_one
			jmp after_char_handler_call
		char_two:
			call char_handler_two
			jmp after_char_handler_call
		char_other:
			call char_handler_other
			jmp after_char_handler_call
						
		after_char_handler_call:		
	loop loop_args
	
	; this trickery has to be done cuz p_index could not get incremented
	; if cmd line args ended with a file path
	cmp arg_dsh, 1					; if previous arg wasnt dash arg
	jz end_loop				
	pshutil
	mov ax, @data
	mov ds, ax	
	inc p_index						; increment path index variable
	poputil
		
	end_loop:				
	ret
	
	no_args:
		print NO_ARG				; print noarg warning
		mov print_h, 1				; turn on help print
		ret
parse_cmd endp	
	
; ----------------------------------------------------------------------------------	
; procedure that prints help using the help_prnt macro
; and terminates
help_print_only proc
	help_prnt						; print help
	terminate_prog					; terminate
	ret
help_print_only endp

; ----------------------------------------------------------------------------------
; reads keyboard input into searched_s, also sets searched_l
read_searched_from_k proc
	pshutil
	print KEY_PRT					
	mov ax, @data
	mov ds, ax
	
	; for 0ah call ds:dx points to string, char 00 is set to max len before call
	; char 01 contains actual read length after the call
	; characters 2+ contain the actual string
	; variables are set up srched_s_ml, searched_l, searched_s
	; so when its called pointing to srched_s_ml (set to 81)
	; searched_l contains length of the string
	; and searched_s contains the string, as they should
	mov dx, offset srched_s_ml		
	mov ax, 0a00h
	int 21h
		
	pr_line_break
	pr_line_break
	poputil
	ret
read_searched_from_k endp

; ----------------------------------------------------------------------------------
; reads from file into searched_s, also sets searched_l
read_searched_from_f proc
	pshutil
	mov ax, @data
	mov ds, ax
	mov si, 0
	
	lea dx, searched_s				; buf_ptr contains pointer to buffer
	mov buf_ptr, dx
	mov ax, 3d00h 					; open the file (3d) for reading (00)
	lea dx, path_1 					; path to file to be opened, zerobyte terminated string
	int 21h 
	mov [filehandle], ax 			; store file handle
	jc couldnt_open2				; file couldnt be opened
	
	
	read_loop2:
		mov ax, 3f00h  				; read from file (3f)
		lea dx, searched_s[si]		; where to store read data
		mov cx, 1 					; number of bytes to read
		mov bx, [filehandle] 		; file handle
		int 21h
		jc couldnt_open2			; cf is set on error

		cmp searched_s[si], 0dh		; line break reached
		jz end_of_file2
		
		cmp si, 80					; line length reached
		jz end_of_file2
									; ax contains number of bytes read
		mov cx, ax					; no bytes are read if EOF was reached
		jcxz end_of_file2
		
		inc si	
   
		mov ax, 3f00h 				; not sure why 2 reads are needed but here they are
		lea dx, buf_ptr 			
		mov cx, 1
		mov bx, [filehandle] 		
		int 21h
   
		mov ax, 4200h				; seek (42), relative to start of file (00)
		mov bx, [filehandle]		; file handle
		mov cx, 0					; no need to care about 64kB+ files
		mov dx, si					
		int 21h
   
		;cmp buf_ptr, 0				
		;je end_of_file
	jmp read_loop2
   
	couldnt_open2:
		print FILEERR2				; print error string
		terminate_prog				; exit
		
	end_of_file2:
		mov bx, [filehandle]		; move file handle to bx
		mov ax, 3e00h 				; close file
		int 21h
	
	read_searched_from_f_end:
	mov ax, si
	mov searched_l, al
	print SRC_S_INF
	print searched_s
	pr_line_break
	pr_line_break
	poputil
	ret
read_searched_from_f endp

; ----------------------------------------------------------------------------------
; checks whether count of file paths provided is valid (when using keyboard input)
check_f_count_k proc
	pshutil
	mov ax, @data
	mov ds, ax
	
	cmp p_index, 1					; if file path count isnt 1
	jnz invalid_count				; jump to error print
	mov ax, 0
	mov al, p_index					; else copy p_index 
	mov f_2_src_cnt, al				; to f_2_src_cnt
	jmp check_f_count_k_end
	
	invalid_count:					
		print P_CNT_ERR				; print error
		help_prnt
		terminate_prog				; and terminate
	
	check_f_count_k_end:
	poputil
	ret
check_f_count_k endp

; ----------------------------------------------------------------------------------
; checks whether count of file paths provided is valid (when using input from file)
check_f_count_f proc
	pshutil
	mov ax, @data
	mov ds, ax
	
	cmp p_index, 2					; if file path count isnt 2
	jnz invalid_count2				; jump to error print
	mov ax, 0
	mov al, p_index
	dec al
	mov f_2_src_cnt, al				; else copy p_index to f_2_src_cnt
	jmp check_f_count_f_end

	invalid_count2:
		print P_CNT_ERR2			; print error and terminate
		help_prnt
		terminate_prog
	
	check_f_count_f_end:
	poputil
	ret
check_f_count_f endp

; ----------------------------------------------------------------------------------
; main loop
start:
	call parse_cmd					; parse cmd line arguments
		
	cmp print_h, 1					; if help print is on, only help print gets done
	jnz normal_execution
	call help_print_only			; also terminates
	
	normal_execution:				; normal program execution part
	cmp from_k, 1
	jz str_from_k
	jmp str_from_f
	
	str_from_k:
		call check_f_count_k
		call read_searched_from_k
		jmp after_reading_searched
	str_from_f:
		call check_f_count_f
		call read_searched_from_f
		jmp after_reading_searched
		
	after_reading_searched:	
		push ds						; ds still has psp in it cuz read_file expects it
		mov ax, @data
		mov ds, ax
		cmp searched_l, 0			; check whether searched string is empty
		pop ds
	jnz valid_len					; if its not, continue normally
		print LEN_ERR				; if it is, print error, terminate
		terminate_prog
	
	valid_len:
	print SEARCH
	call read_file
	terminate_prog
ends

end start

