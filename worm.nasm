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

	mov rax, 11
	call alloc

	;push rax
	;call read_input
	;pop rax
	
	push rax
	mov rax, greeting
	mov rcx, bmp_name
	call write_whole_file
	pop rax
	
	call free

	mov rax, 10
	call sleep_ms

	; The unix exit
	mov rax, 0			; success
	call exit

read_input:		; (rax buffer[] -- rax syscall_returned)
	mov rdx, qword[rax - 8]	; the size is stored before the buffer
	mov rsi, rax		; the buffer pointer
	mov rdi, 0			; stdin
	mov rax, 0			; 0 is read
	syscall
	ret

print_string:	; (rax string[] -- rax syscall_returned)
	mov rdx, qword[rax - 8]	; the length is stored before the buffer
	mov rsi, rax		; Putting the text
	mov rdi, 1			; stdout
	mov rax, 1			; 1 is write
	syscall
	ret

write_whole_file:	; (rax data[], rcx filename_zero_terminated[] -- rax syscall_returned)
	push rax

	mov rdx, 420		; 0o644: user 6 rw, group 4 r, other 4 r
	mov rsi, 0x0242		; 0x2 (read-write) | 0x4 (create) | 0x200 (truncate)
	mov rdi, rcx		; filename
	mov rax, 2			; 2 is open
	syscall

	pop rcx
	push rax

	mov rdx, qword[rcx - 8]	; the length is stored before the buffer
	mov rsi, rcx		; data pointer
	mov rdi, rax		; the just-opened file
	mov rax, 1			; 1 is write
	syscall

	pop rdi				; the file descriptor that I pushed onto the stack earlier
	mov rax, 3			; 3 is close
	syscall
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
	mov rsi, qword[rax]	; the size
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

.data:
	greeting.count:
		dq 11; length
	greeting:
		db "Hey there!", 10
		
	bmp_name:
		db "./worm.bmp", 0

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
