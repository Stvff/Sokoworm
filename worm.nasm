; This is a custom ELF header for smaller executables. It is less memorysafe.
BITS 64
	org     0x08048000
elf_header:
	; e_ident
	db 0x7F, "ELF"
	db 2		; 64 bit
	db 1		; little endian
	db 1		; version
	db 0		; ABI: UNIX system V (3 is GNU/Linux, but I'm not using any GNU)
	db 0		; more ABI (also dynlinker ABI for glibc?)
	db 0, 0, 0, 0, 0, 0, 0 ; 7 zeroes to pad

	dw 2		; e_type: executable
	dw 0x3E		; e_machine: AMD X86-64
	dd 1		; e_version
	dq _start		; e_entry
	dq program_header - $$		; e_phoff: program header offset (should be 0x40)
	dq 0		; e_shoff: section header offset (we don't have this rn)
	dd 0		; e_flags
	dw 0x40		; e_ehsize: elf header size (should be 0x40 for 64 bit)
	dw 0x38		; e_phentsize: program header entry size (should be 0x38 for 64 bit)
	dw 1		; e_phnum: number of program header entries
	dw 0		; e_shentsize: section header stuff. we don't have it, so it's all zero
	dw 0		; e_shnum
	dw 0		; shstrndx

program_header:
	dd 1		; p_type: loadable segment (PT_LOAD)
	dd 0x7		; p_flags: executable (1) + writeable (2) + readable (4)
	dq 0		; p_offset (I think this is offset from _start)
	dq $$		; p_vaddr: $$ is that org number at the top, which is where it'll get loaded in
	dq $$		; p_paddr (I'm pretty sure this is ignored)
	dq filesize		; p_filesz
	dq filesize		; p_memsz
	dq 0x1000		; p_align: align to linux page size (4096 bytes)

_start:
	mov rax, greeting
	call print_string

	mov rax, IMG_SIZE
	call alloc
	mov r15, rax		; image data pointer is always in r15

	mov rax, UNDO_BUFFER_SIZE
	add rax, 8
	call alloc
	push rax				; input history
	mov qword[rax], 0		; initting history to zero

	mov rax, 2
	call alloc
	push rax			; input

	; stack:( +0 input, +8 history )
	game_loop:

	xor rcx, rcx
	clear_loop:
		mov byte[r15 + rcx], 110
		mov byte[r15 + rcx + 1], 150
		mov byte[r15 + rcx + 2], 190
		add rcx, 3
		cmp rcx, IMG_SIZE
		jl clear_loop

	mov byte[current_floor], 0
	floor_loop:
	xor r13, r13		; y offset
	xor r12, r12		; y coordinate of block
	per_row:
			xor r14, r14		; x value
			over_width:
				mov rax, r14	; put x into eax for division
				mov rdx, 0
				mov rcx, BLOCK
				div ecx			; divide by full block width, we really only care about the remainder
				mov r8, rax		; x coordinate of block

				sar rdx, 1		; y = x/2
				mov rcx, rdx	; upper limit of ground
				mov rbx, rdx	; lower limit of ground
				mov rax, rdx	; top of ground
				cmp rdx, QUAR_BLOCK	; in the case that we're over halfway, reverse direction
				jg second_half
				first_half:
					imul rcx, -1
					add rcx, QUAR_BLOCK
					add rbx, THQU_BLOCK
					add rax, QUAR_BLOCK
				jmp end_of_halves
				second_half:
					sub rcx, QUAR_BLOCK
					imul rbx, -1
					add rbx, ONQU_BLOCK
					imul rax, -1
					add rax, THQU_BLOCK
				end_of_halves:
	
				cmp rcx, r13
				jg end_of_drawing
				cmp rbx, r13
				jle end_of_drawing
					imul r11, r12, QUAR_BLOCK	; y offset from block
					add r11, r13		; y offset in block
					imul r11, WIDTH
					
					mov r10, r12
					and r10, 1
					cmp r10, 1
					jne skip_odd_alternation
						add r11, HALF_BLOCK
					skip_odd_alternation:
					add r11, r14		; x offset
					imul r11, 3			; byte width
					add r11, r15		; data offset so this is a pointer
				push rax
					mov rax, r8
					mov rcx, r12
					call get_current_block_info
				pop r8
				cmp r8, r13
				jg actual_drawing
					add rax, 3
				actual_drawing:
					cmp rcx, 0
					je end_of_drawing
					mov bl, [rax]
					mov byte[r11], bl
					mov bl, [rax + 1]
					mov byte[r11 + 1], bl
					mov bl, [rax + 2]
					mov byte[r11 + 2], bl
	
				end_of_drawing:
				inc r14
				cmp r14, WIDTH
				jl over_width
		inc r13
		cmp r13, BLOCK
		jl per_row
	xor r13, r13
	inc r12
	cmp r12, 29
	jl per_row

	inc byte[current_floor]
	cmp byte[current_floor], 2
	jl floor_loop
	mov byte[current_floor], 0

	mov rax, r15		; putting the data pointer back in rax
	call write_image

	mov rax, [rsp + 8]
	call push_gamestate

	mov rax, prompt
	call print_string
	mov rax, [rsp]
	call read_input
	mov rbx, -1
	mov rcx, 1				; direction x
	mov rdx, -1				; direction y
	cmp byte[rax], "e"		; topleft
		cmove rcx, rdx		; x-1, y-1
		je figure_out_movement
	cmp byte[rax], "r"		; topright
							; x+1, y-1
		je figure_out_movement
	cmp byte[rax], "d"		; botleft
		cmove rdx, rcx		; x-1, y+1
		cmove rcx, rbx
		je figure_out_movement
	cmp byte[rax], "f"		; botright
		cmove rdx, rcx		; x+1, y+1
		je figure_out_movement

	cmp byte[rax], "z"
	jne skip_undo
		mov rax, [rsp + 8]
		call pop_gamestate
		call pop_gamestate
		jmp game_loop
	skip_undo:
	cmp byte[rax], "q"
		je quit
	jmp game_loop

	figure_out_movement:
	;push rcx
	;push rdx
	;; stack:( +0 dir_y, +8 dir_x, +16 input, +24 history )

	; worm is always first blocks of a floor
	mov rax, [current_level]
	xor rbx, rbx
	mov bl, [rax]
	imul rbx, 3
	inc rbx
	add rax, rbx
	inc rax				; rax now points to the head of worm
	xor r8, r8
	xor r9, r9
	mov r8b, [rax]
	mov r9b, [rax+1]	; r8 xpos, r9 ypos of worm

	; first update xpos, based on the evenness of ypos, and negativeness of xdir
	mov r10, r9
	cmp rcx, -1		; if xdir == -1, flip r10
	jne skip_inversion_head_mvmt
		not r10
	skip_inversion_head_mvmt:
	and r10, 1
	imul r10, rcx
	add r8b, r10b
	; update ypos
	add r9b, dl

	; update worm body (but not yet head)
	push rax
	push word[rax]
	add rax, 3
	worm_body_update_loop:
		pop bx
		push word[rax]
		mov [rax], bx
		add rax, 3
		cmp byte[rax + 2], 5
		je worm_body_update_loop
	pop bx

	; check if those coords already have something
	mov r10, rcx			; r10 is now xdir
	mov r11, rdx			; r11 is now ydir
	mov rax, r8
	mov rcx, r9
	mov byte[current_floor], 1
	call get_current_block_info	; first check floor 1
	cmp rcx, 0					; air
	je check_floor_0
	cmp rcx, 2					; a hole
	je illegal_move
	cmp rcx, 4					; a block
	jne skip_block_loop
	block_loop:
		xor r12, r12
		xor r13, r13
		mov r14, rdx			; found_block_pointer
		mov r12b, [r14]			; xpos
		mov r13b, [r14 + 1]		; ypos

		; first update xpos, based on the evenness of ypos, and negativeness of xdir
		mov rax, r13
		cmp r10, -1		; if xdir == -1, flip rax
		jne skip_inversion_block_mvmt
			not rax
		skip_inversion_block_mvmt:
		and rax, 1
		imul rax, r10
		add r12b, al
		; update ypos
		add r13b, r11b

		; check floor 0
		mov rax, r12
		mov rcx, r13
		add rcx, 2
		mov byte[current_floor], 0
		call get_current_block_info
		; if below hole or air, fill that hole or air, set current blocktype to 0
		cmp rcx, 0
		je illegal_move
		cmp rcx, 2
		jne block_loop_floor_1
			mov byte[r14 + 2], 0
			mov byte[rdx + 2], 4

		block_loop_floor_1:
		mov rax, r12
		mov rcx, r13
		mov byte[current_floor], 1
		call get_current_block_info
		cmp rcx, 4					; a block
		jne block_loop_skip_block
			mov [r14], r12b			; xpos
			mov [r14 + 1], r13b		; ypos
			jmp block_loop
		block_loop_skip_block:
		cmp rcx, 5					; a worm
		je illegal_move
		cmp rcx, 6					; a wormhead
		je illegal_move

		mov [r14], r12b			; xpos
		mov [r14 + 1], r13b		; ypos
		jmp check_floor_0
	skip_block_loop:
	cmp rcx, 5					; a worm
	je illegal_move
	cmp rcx, 6					; a wormhead
	je illegal_move

	check_floor_0:
	mov rax, r8
	mov rcx, r9
	add rcx, 2
	mov byte[current_floor], 0
	call get_current_block_info	; after floor 1, check floor 0
	cmp rcx, 0					; air
	je illegal_move
	cmp rcx, 1					; ground
	je legal_move
	cmp rcx, 2					; a hole
	je illegal_move
	cmp rcx, 3					; a wormhole
	jne skip_wormhole_check
		mov rax, [current_level]
		xor rbx, rbx
		mov bl, [rax]
		imul rbx, 3
		inc rax
		add rbx, rax
		wormhole_check_loop:
			cmp byte[rax + 2], 2
			je legal_move
			add rax, 3
			cmp rax, rbx
			jl wormhole_check_loop
		pop rax						; cleaning up the worm head
		call current_level_length
		add [current_level], rax
		cmp qword[current_level], game_complete
		je game_completion

		mov rax, next_level_text
		call print_string
		mov rax, [rsp + 8]
		mov qword[rax], 0

		jmp game_loop
	skip_wormhole_check:
	cmp rcx, 4					; a block
	je legal_move

	legal_move:					; updating the head of the worm at last
	pop rax
	mov [rax], r8b
	mov [rax+1], r9b

	jmp game_loop
	
	game_completion:
	mov rax, game_completion_text
	call print_string

	quit:
	add rsp, 16			; user input buffer, input history buffer

	; The unix exit
	mov rax, 0			; success
	call exit

illegal_move:		; This is a subroutine, not a function
	pop rax				; cleanup the pointer to the wormhead
	mov rax, [rsp + 8]
	call pop_gamestate
	jmp game_loop


push_gamestate:		; (rax history_buffer -- rax history_buffer)
	push rax

	; check length of current level
	call current_level_length

	; allocate that size, copy over into it
	push rax
	call alloc					; rax now contains the newly allocated level buffer
	pop rcx						; rcx contains level_length
	xor rbx, rbx
	push_gamestate_copyloop:
		mov rdx, [current_level]
		mov dl, [rdx + rbx]
		mov byte[rax + rbx], dl
		inc rbx
		cmp rbx, rcx
		jl push_gamestate_copyloop

	; shift history buffer to the right
	push rax					; rax still contained the newly allocated level buffer
	mov rax, [rsp + 8]	; we're retrieving the history buffer
	mov rbx, UNDO_BUFFER_SIZE
	add rbx, rax
	push_gamestate_shiftloop:
		sub rbx, 8
		mov rcx, qword[rbx]
		mov qword[rbx + 8], rcx
		cmp rbx, rax
		jg push_gamestate_shiftloop

	; put remembered value on the history buffer
	pop rbx						; rbx now contains the newly allocated level buffer
	pop rax						; rax is the history_buffer again
	mov qword[rax], rbx
	ret

pop_gamestate:		; (rax history_buffer -- rax history_buffer)
	; check top for zero
	cmp qword[rax], 0
	je pop_gamestate_end

	push rax
	; check length of current level
	call current_level_length	; rax now has the length of current state

	mov rbx, [rsp]				; rbx now has history_buffer
	mov rbx, [rbx]				; rbx now has pointer to the previous state
	mov rcx, [current_level]	; rcx now has pointer to the current state
	; copy top of history into current_level
	xor r8, r8
	pop_gamestate_copyloop:
		mov dl, [rbx + r8]
		mov byte[rcx + r8], dl
		inc r8
		cmp r8, rax
		jl pop_gamestate_copyloop

	pop rax						; rax now has history_buffer
	; shift history back up
	xor rcx, rcx
	pop_gamestate_shiftloop:
		add rcx, 8
		mov rbx, [rax + rcx]
		mov qword[rax + rcx - 8], rbx
		cmp rcx, UNDO_BUFFER_SIZE
		jl pop_gamestate_shiftloop

	pop_gamestate_end:
	ret

current_level_length:	; ( -- rax level_length)
	mov rax, [current_level]	; rax has level pointer
	xor rdx, rdx
	mov dl, [rax]				; rdx has floor 1 block amount
	imul rdx, 3
	inc rdx						; *3 + 1 to get byte length of floor 1
	add rax, rdx				; rax now points to floor 2
	xor rdx, rdx
	mov dl, [rax]
	imul rdx, 3
	inc rdx
	add rax, rdx				; rax now points to 1 byte after floor 2
	sub rax, [current_level]	; rax now contains level length
	ret

get_current_block_info:	; (rax x_coord, rcx y_coord -- rax colour, rcx blocktype, rdx block_pointer)
	push rax
	push rcx
	mov rax, [current_level]
	cmp byte[current_floor], 0
	je first_floor_block_info
		xor rcx, rcx
		mov cl, [rax]
		imul rcx, 3
		add rax, rcx
		inc rax
	first_floor_block_info:
	xor rcx, rcx
	mov cl, [rax]
	imul rcx, 3
	inc rax
	add rcx, rax
	push rcx
	block_info_check_loop:
		xor rcx, rcx
		mov cl, [rax]
		cmp cl, [rsp + 16]
		jne continue_block_info_check_loop
		mov cl, [rax + 1]
		cmp cl, [rsp + 8]
		jne continue_block_info_check_loop
			mov cl, [rax + 2]
			mov rdx, rax
			mov rax, [colour_table + rcx*8]
			jmp end_of_info_check

		continue_block_info_check_loop:
		add rax, 3
		cmp rax, [rsp]
		jl block_info_check_loop

	mov rax, water_colour
	mov rcx, 0
	end_of_info_check:
	add rsp, 24
	ret

read_input:		; (rax buffer[] -- rax buffer[], rdx syscall_returned)
	push rax
	mov rdx, [rax - 8]	; the size is stored before the buffer
	mov rsi, rax		; the buffer pointer
	mov rdi, 0			; stdin
	mov rax, 0			; 0 is read
	syscall
	mov rax, rdx
	pop rax
	ret

print_string:	; (rax string[] -- rax syscall_returned)
	xor rdx, rdx
	mov dl, byte[rax-1]	; the length is stored before the buffer
	mov rsi, rax		; Putting the text
	mov rdi, 1			; stdout
	mov rax, 1			; 1 is write
	syscall
	ret

write_image:	; (rax data[] -- rax syscall_returned)
	push rax			; save data pointer

	mov rdx, 420		; 0o644: user 6 rw, group 4 r, other 4 r
	mov rsi, 0x0242		; 0x2 (read-write) | 0x40 (create) | 0x200 (truncate)
	mov rdi, image_name	; filename
	mov rax, 2			; 2 is open
	syscall

	push rax			; save file descriptor

	mov rdx, 15			; length of ppm header
	mov rsi, ppm_header	; the actual header
	mov rdi, [rsp]		; the file descriptor
	mov rax, 1			; 1 is write
	syscall

	mov rcx, [rsp + 8]
	mov rdx, [rcx - 8]	; the length is stored before the buffer
	mov rsi, rcx		; data pointer
	mov rdi, [rsp]		; the file descriptor
	mov rax, 1			; 1 is write
	syscall

	mov rdi, [rsp]		; the file descriptor
	mov rax, 3			; 3 is close
	syscall
	add rsp, 16
	ret

alloc:			; (rax byte_size -- rax data[])
	mov rbx, rax
	add rbx, 8

	mov r9, 0			; Offset, 0 is 0
	mov r8, -1			; File descriptor, -1 is map no file
	mov r10, 34			; Flags: 2 is private, 32 is anonymous
	mov rdx, 3			; Protection flags, 1 is read, 2 is write
	mov rsi, rbx		; The size
	mov rdi, 0			; The address, 0 because I have no preference
	mov rax, 9			; 9 is mmap
	syscall

	sub rbx, 8
	mov qword[rax], rbx	; saving the mmap-ed size (actual size - 8 because I don't count the size value)
	add rax, 8			; pointer to start of memory
	ret

;free:			; (rax data[] -- rax syscall_returned)
;	sub rax, 8			; Reset the pointer to the actual start of the buffer
;	add qword[rax], 8	; , and the actual length of it as well
;	mov rsi, [rax]		; the size
;	mov rdi, rax		; the pointer
;	mov rax, 11			; 11 us unmap
;	syscall
;	ret

exit:			; (rax error_code -- rax syscall_returned)
	mov rdi, rax
	mov rax, 60			; 60 is exit
	syscall
	ret

;sleep_ms:		; (rax milliseconds -- rax syscall_returned)
;	imul rax, 1000000	; ms to ns
;	push rax
;	push 0				; struct timespec duration

;	mov rsi, 0			; rem (null, so not used)
;	mov rdi, rsp		; duration
;	mov rax, 35			; 35 is nanosleep
;	syscall
;	add rsp, 16
;	ret

.data:
	greeting.count:
		db 255; length
	greeting:
		db "Welcome!", 10, "Open worm.ppm with xviewer, and put it next to this terminal", 10,
		db "Red is a hole, Black is a wormhole", 10
		db "The controls are:", 10
		db "e for going topleft", 10, "r for topright", 10, "d for bottomleft", 10, "f for bottomright", 10,
		db " those are based on keyboard layout", 10
		db "z for undo", 10, "q for quit", 10, "GL!", 10

		db 2
	prompt:
		db "> "

		db 12
	next_level_text:
		db "Next level!", 10

		db 37
	game_completion_text:
		db "Finished! Thanks for playing!", 10, "-Stvff", 10

	image_name:
		db "./worm.ppm", 0

	current_level: dq level_1
	current_floor: db 0

	level_1:
		db 9
		db 1, 13, 1
		db 2, 14, 1
		db 2, 15, 1
		db 3, 16, 1
		db 3, 17, 1
		db 4, 18, 1
		db 4, 19, 2
		db 5, 20, 1
		db 5, 21, 3
		db 3
		db 2, 12, 6
		db 1, 11, 5
		db 3, 15, 4
	level_2:
		db 9
		db 1, 13, 1
		db 2, 14, 1
		db 2, 15, 1
		db 3, 16, 1
		db 3, 17, 1
		db 4, 18, 2
		db 4, 19, 2
		db 5, 20, 1
		db 5, 21, 3
		db 4
		db 2, 12, 6
		db 1, 11, 5
		db 3, 14, 4
		db 3, 15, 4
	game_complete:

	colour_table:
		dq water_colour, ground_colour, hole_colour, wormhole_colour, block_colour, worm_colour, worm_head_colour

	water_colour:     db 110, 150, 190, 110, 150, 190
	ground_colour:    db 150, 130, 80, 130, 110, 80
	hole_colour:      db 100, 70, 40, 150, 60, 30
	wormhole_colour:  db 20, 20, 20, 0, 0, 0
	block_colour:     db 100, 100, 100, 70, 70, 70
	worm_colour:      db 220, 160, 160, 210, 150, 150
	worm_head_colour: db 237, 170, 170, 230, 160, 160
	;open_wormhole_colour: db 240, 240, 240, 235, 235, 235

	;ppm_header.count:
	;	dq 15
	ppm_header:
		db "P6", 10, "768 768", 10, "255", 10
	WIDTH equ 768
	HEIGHT equ 768
	BYTE_WIDTH equ WIDTH*3
	IMG_SIZE equ BYTE_WIDTH*HEIGHT

	BLOCK equ WIDTH/8
	HALF_BLOCK equ BLOCK/2
	QUAR_BLOCK equ BLOCK/4
	THQU_BLOCK equ 3*QUAR_BLOCK
	ONQU_BLOCK equ 5*QUAR_BLOCK
	
	UNDO_BUFFER_SIZE equ 2048

filesize equ $ - $$; This is for the custom ELF header.

; Syscall arguments:
;	call number: rax
;	arg1: rdi
; 	arg2: rsi
;	arg3: rdx
;	arg4: r10
;	arg5: r8
;	arg6: r9
;	return value: rax
