;************************************************************************
;		 loader.inc	(C) Jim Lee
;************************************************************************

%ifndef __LOADER_INCLUDE__
%define __LOADER_INCLUDE__

bits 16

; where the kernel is to be loaded to in protected mode
KERNEL_PMODE_SEG equ 0x1000
KERNEL_PMODE_OFF equ 0x0000

; where the kernel is to be loaded to in real mode
KERNEL_RMODE_BASE equ 0x3000

; kernel name (Must be 11 bytes)
KernelName     db "KERNEL  SYS"

; size of kernel image in bytes
KernelSize     dw 0

%endif 		; __LOADER_INCLUDE__
