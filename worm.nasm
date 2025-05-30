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
	;mov rax, greeting
	;call print_string

	mov rax, IMG_SIZE
	call alloc

	;push rax
	;call read_input
	;pop rax

	xor rcx, rcx
	clear_loop:
		mov byte[rax + rcx], 110
		mov byte[rax + rcx + 1], 150
		mov byte[rax + rcx + 2], 190
		add rcx, 3
		cmp rcx, IMG_SIZE
		jl clear_loop
	mov r15, rax		; data pointer

	mov r9, 0
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

	inc byte[current_level + 8]
	cmp byte[current_level + 8], 2
	jl floor_loop

	mov rax, r15		; putting the data pointer back in rax
	push rax
		call write_image
		mov rax, 300
	pop rax
	call free

	; The unix exit
	mov rax, 0			; success
	call exit

read_input:		; (rax buffer[] -- rax syscall_returned)
	mov rdx, [rax - 8]	; the size is stored before the buffer
	mov rsi, rax		; the buffer pointer
	mov rdi, 0			; stdin
	mov rax, 0			; 0 is read
	syscall
	ret

print_string:	; (rax string[] -- rax syscall_returned)
	mov rdx, [rax - 8]	; the length is stored before the buffer
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

free:			; (rax data[] -- rax syscall_returned)
	sub rax, 8			; Reset the pointer to the actual start of the buffer
	add qword[rax], 8	; , and the actual length of it as well
	mov rsi, [rax]		; the size
	mov rdi, rax		; the pointer
	mov rax, 11			; 11 us unmap
	syscall
	ret

exit:			; (rax error_code -- rax syscall_returned)
	mov rdi, rax
	mov rax, 60			; 60 is exit
	syscall
	ret

sleep_ms:		; (rax milliseconds -- rax syscall_returned)
	imul rax, 1000000	; ms to ns
	push rax
	push 0				; struct timespec duration

	mov rsi, 0			; rem (null, so not used)
	mov rdi, rsp		; duration
	mov rax, 35			; 35 is nanosleep
	syscall
	add rsp, 16
	ret

get_current_block_info:	; (rax x_coord, rcx y_coord -- rax colour, rcx blocktype)
	push rax
	push rcx
	mov rax, [current_level]
	cmp byte[current_level + 8], 0
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

.data:
	greeting.count:
		dq 11; length
	greeting:
		db "Hey there!", 10
	image_name:
		db "./worm.ppm", 0

	current_level:
		dq level_1, 0
		db 0

	level_1:
		db 4
		db 3, 12, 1
		db 4, 12, 2
		db 4, 14, 1
		db 3, 14, 3
		db 6
		db 2, 10, 4
		db 1, 6, 5
		db 1, 7, 5
		db 2, 8, 5
		db 2, 9, 5
		db 3, 10, 5
	level_2:
		db 0
		db 0

	colour_table:
		dq water_colour, ground_colour, hole_colour, wormhole_colour, block_colour, worm_colour

	water_colour: db 100, 105, 110, 90, 95, 100
	ground_colour: db 150, 130, 80, 130, 110, 80
	hole_colour: db 220, 70, 40, 210, 60, 30
	wormhole_colour: db 20, 20, 20, 0, 0, 0
	block_colour: db 100, 100, 100, 70, 70, 70
	worm_colour: db 230, 170, 170, 220, 160, 160

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
