;************************************************************************
;		 loader.asm	(C) Jim Lee
;
; Start the CPU: switch to 32-bit protected mode, jump into C.
; The BIOS loads this code from the first sector of the hard disk into
; memory at physical address 0x7c00 and starts executing in real mode
; with cs=0 ip=7c00.
;************************************************************************

;************************************************************************
; defines for the code go here
;************************************************************************

org 0x10000				; offset to 0, we will set segments later

; Tell NASM to generate 16-bit instructions so that this code works in real mode
bits 16

global _loader, loader

_loader:
loader:
	jmp main			; jump to main

;************************************************************************
;	OEM Parameter block / BIOS Parameter Block
;************************************************************************
%include "FAT12.inc"

;************************************************************************
;	Preprocessor directives
;************************************************************************
%include "stdio.inc"				; basic i/o routines
%include "loader.inc"

;************************************************************************
;	Data Section
;************************************************************************
notification_string: db "Preparing to load Operating System...", 0x0D, 0x0A, 0x00
KernelFileName	: db "LOADER  BIN"
FileFoundMsg	: db "File Found", 0x0D, 0x0A, 0x00
FileNotFoundMsg	: db "File Not Found", 0x0D, 0x0A, 0x00

;************************************************************************
;	Second Stage Loader Entry Point
;************************************************************************

main:
	cli				; disable interrupts

	mov ax, cs			; null segments
	mov ds, ax
	mov es, ax

	xor ax, ax
	mov ss, ax
	mov sp, 0x10000

	sti				; enable interrupts

	; print a notification message on the screen
	MSG notification_string

	call Func_Load_Root

	mov si, KernelFileName
	call Func_Find_File
	cmp ax, -1
	jne .FileFound

.FileNotFound:
	MSG FileNotFoundMsg
	mov ah, 0x00			; Function 0
	int 0x16			; invoke BIOS 0x16, Function 0: Await keypress
	int 0x19			; invoke BIOS 0x19: reboot computer

.FileFound:
	MSG FileFoundMsg

.LOOP:
	jmp .LOOP


