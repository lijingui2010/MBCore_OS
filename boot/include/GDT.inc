;************************************************************************
;		 gdt.inc	(C) Jim Lee
;
;		Global Descriptor Table (GDT).
;************************************************************************

%ifndef __GDT_INCLUDE__
%define __GDT_INCLUDE__

bits 16

;************************************************************************
; This is the Global Descriptor Table
;
;  An entry, a "Segment Descriptor", looks like this:
;
; 31          24         19   16                 7           0
; ------------------------------------------------------------
; |             | |B| |A|       | |   |1|0|E|W|A|            |
; | BASE 31..24 |G|/|L|V| LIMIT |P|DPL|  TYPE   | BASE 23:16 |  4
; |             | |D| |L| 19..16| |   |1|1|C|R|A|            |
; ------------------------------------------------------------
; |                             |                            |
; |        BASE 15..0           |       LIMIT 15..0          |  0
; |                             |                            |
; ------------------------------------------------------------
;
;  Note the ordering of the data items is reversed from the above
;  description.
;************************************************************************

gdt:
	;************************************************************************
	; -- null segment descriptor --
	;************************************************************************
	null_seg_desc: 
		dw 0, 0
		db 0, 0, 0, 0

	;************************************************************************
	; -- code segment descriptor -- 
	; base = 0x00000000, limit = 0xFFFFF (4 KiB Granularity), present
	; type = 32 bit code execute/read, DPL = 0
	;************************************************************************
	code_seg_desc:
		dw 0xFFFF		; limit low
		dw 0x0000               ; base low
		db 0x00			; base middle
		db 0x9A			; access
		db 0xCF			; granularity
		db 0x00                	; base high

	;************************************************************************
	; -- data segment descriptor --
	; base = 0x00000000, limit 0xFFFFF (4 KiB Granularity), present
	; type = 32 bit data read/write, DPL = 0
	;************************************************************************
	data_seg_desc:
		dw 0xFFFF		; limit low
		dw 0x0000               ; base low
		db 0x00			; base middle
		db 0x92			; access
		db 0xCF			; granularity
		db 0x00                	; base high
gdt_end:

;************************************************************************
; this is the GDT descriptor
;************************************************************************
gdtdesc:
	dw gdt_end - gdt - 1		; limit (Size of GDT)
	dd gdt				; address (address of GDT)

;************************************************************************
; give the descriptor offsets names
;************************************************************************
%define NULL_DESC 0
%define CODE_DESC 0x8
%define DATA_DESC 0x10

;************************************************************************
; Func_Load_GDT
;	- Load our GDT
;************************************************************************
Func_Load_GDT:
	cli				; clear interrupts
	pusha				; save registers
	lgdt [gdtdesc]			; load GDT into GDTR
	sti				; enable interrupts
	popa				; restore registers
	ret

%endif 		; __GDT_INCLUDE__
