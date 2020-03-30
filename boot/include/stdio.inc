;************************************************************************
;		 stdio.inc	(C) Jim Lee
;
;			-Input/Output routines
;************************************************************************

%ifndef __STDIO_INCLUDE__
%define __STDIO_INCLUDE__

;************************************************************************
;	 16 Bit Real Mode Routines
;************************************************************************
bits 16

;************************************************************************
; Print message string
;************************************************************************
%macro MSG 1
	mov si, %1
	call Func_RM_Print_MSG
%endmacro

;************************************************************************
; message: write the string pointed to by si
;   WARNING: trashes si, ax, and bx
;
; Use BIOS "int 10H Function 0Eh" to write character in teletype mode
;	ah = 0xe	al = character
;	bh = page	bl = foreground color (graphics modes)
;************************************************************************
Func_RM_Print_MSG:
	pusha					; save registers
	
	.PRINTLOOP:
		lodsb				; load next byte from string from SI to AL
		or	al, al			; Does AL=0? if end of string, jmp to Done
		jz	.PRINTDone		; Yep, null terminator found-bail out
		mov	bx, 0x0001		; Set page and foreground color
		mov	ah, 0x0e		; Nope-Print the character
		int	0x10			; invoke BIOS
		jmp	.PRINTLOOP		; Repeat until null terminator found
	.PRINTDone:
		popa				; restore registers
		ret				; we are done, so return

;************************************************************************
;	 32 Bit Protected Mode Routines
;************************************************************************
;bits 32

;%define	VIDMEM 0xB8000			; video memory

%endif 		; __STDIO_INCLUDE__
