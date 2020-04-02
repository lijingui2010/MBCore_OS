;************************************************************************
;		 A20.inc	(C) Jim Lee
;
;		Enable A20 address line
;************************************************************************

%ifndef __A20_INCLUDE__
%define __A20_INCLUDE__

bits 16

;************************************************************************
; Enables A20 line using the Keyboard Controller chip (8042 chip)
;************************************************************************
Func_Enable_A20_KBCTRL:
        cli
 	pusha

        call a20wait_input
        mov  al, 0xAD
        out  0x64, al
 
        call a20wait_input
        mov  al,0xD0
        out  0x64,al
 
        call a20wait_output
        in   al, 0x60
        push eax
 
        call a20wait_input
        mov  al, 0xD1
        out  0x64,al
 
        call a20wait_input
        pop  eax
        or   al, 2
        out  0x60, al
 
        call a20wait_input
        mov  al, 0xAE
        out  0x64,al
 
        call a20wait_input
	popa
        sti
        ret
 
a20wait_input:
        in   al, 0x64
        test al, 2
        jnz  a20wait_input
        ret
 
a20wait_output:
        in   al, 0x64
        test al, 1
        jz   a20wait_output
        ret

;************************************************************************
; Enables a20 line using the Fast A20 Gate
;************************************************************************
Func_Enable_A20_FastA20Gate:
	push ax
	in   al, 0x92
	test al, 2
	jnz after
	or  al, 2
	and al, 0xFE
	out 0x92, al
after:
	pop ax
	ret

;************************************************************************
; Enables A20 line using the BIOS
;************************************************************************
Func_Enable_A20_BIOS:
	pusha
	mov ax, 0x2401
	int 0x15
	popa
	ret

%endif 		; __A20_INCLUDE__
