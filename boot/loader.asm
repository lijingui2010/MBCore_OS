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

org 0x7e00				; offset to 0, we will set segments later

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
%include "GDT.inc"				; GDT routines
%include "A20.inc"				; A20 enabling

;************************************************************************
;	Data Section
;************************************************************************
notification_string: db 0x0D, 0x0A, "Preparing to load Operating System...", 0x0D, 0x0A, 0x00
KernelFileName	: db "LOADER  BIN"
FileFoundMsg	: db "File Found", 0x0D, 0x0A, 0x00
FileNotFoundMsg	: db "File Not Found", 0x0D, 0x0A, 0x00
FailureMsg 	: db 0x0D, 0x0A, "*** FATAL: MISSING OR CURRUPT KERNEL.SYS. Press Any Key to Reboot", 0x0D, 0x0A, 0x0A, 0x00

;************************************************************************
;	Second Stage Loader Entry Point
;************************************************************************

main:
	cli				; disable interrupts

	;************************************************************************
	; Setup segments and stack
	;************************************************************************
	mov ax, 0			; null segments
	mov ds, ax
	mov es, ax

	xor ax, ax
	mov ss, ax
	mov sp, 0x7e00

	sti				; enable interrupts

	;************************************************************************
	; print a notification message on the screen
	;************************************************************************
	MSG notification_string

	;************************************************************************
	; Enable A20	
	;************************************************************************
	call Func_Enable_A20_KBCTRL

	;************************************************************************
	; Load GDT
	;************************************************************************
	call Func_Load_GDT

	;************************************************************************
	; Initialize filesystem: Load Root Directory Table
	;************************************************************************
	call Func_Load_Root

	;************************************************************************
	; Load Kernel
	;************************************************************************
	mov bx, KERNEL_PMODE_SEG	; BX:BP points to buffer to load to
	mov bp, KERNEL_PMODE_OFF
	mov si, KernelName		; File to load
	call Func_Load_File		; Load Kernel
	mov WORD [KernelSize], cx	; Save size of kernel
	cmp ax, 0			; Test for success
	je EnterPMode
	MSG FailureMsg
	mov ah, 0x00			; Function 0
	int 0x16			; invoke BIOS 0x16, Function 0: Await keypress
	int 0x19			; invoke BIOS 0x19: reboot computer
	
;************************************************************************
; Switch from real to protected mode
;************************************************************************
EnterPMode:
	MSG FileFoundMsg
	jmp KERNEL_PMODE_SEG:KERNEL_PMODE_OFF
	cli				; clear interrupts
	mov eax, cr0			;  set bit 0 in cr0 -- enter pmode
	or  eax, 1
	mov cr0, eax

.LOOP:
	jmp .LOOP


