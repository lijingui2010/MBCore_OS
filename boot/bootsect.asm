;************************************************************************
;		 bootsect.asm	(C) Jim Lee
; FAT12 bootsector  
;	- FAT12 compatible. 
;	- Loads a binary file from the floppy.
; Start the CPU: search and load stage 2 (loader.bin), jump into it.
; The BIOS loads this code from the first sector of the hard disk into
; memory at physical address 0x7c00 and starts executing in real mode
; with cs=0 ip=7c00.
;************************************************************************

;************************************************************************
; other notes:
;  this code is public domain, and you can use it for
;  anything you want. but if you do, please act polite and
;  give credit. :-)
;
;     16-bit mem map  (seg:off)
;  0x0000:0x0000 -> 0x0000:0x0500  BIOS stuff
;  0x0000:0x0500 -> 0x0000:0x2100  FAT12 RootDirectory
;  0x0000:0x2100 -> 0x0000:0x3300  FAT for FAT12
;  0x0000:0x3300 -> 0x0000:0x6c00  14,25kb free space
;  0x0000:0x6c00 -> 0x0000:0x7400  IDT, 256 descriptors
;  0x0000:0x7400 -> 0x0000:0x7c00  GDT, 256 descriptors
;  0x0000:0x7c00 -> 0x0000:0x7e00  bootsector
;  0x0000:0x7e00 <- 0x0000:0xffff  ~32,5kb stack for boot
;  0x1000:0x0000 -> 0x9000:0xffff  576kb kernel/free space
;  0xa000:0x0000 -> .............  VGA mem etc.
;************************************************************************

; Tell NASM to generate 16-bit instructions so that this code works in real mode
bits 16

;************************************************************************
;	Bootloader Entry Point
;************************************************************************
global _start, start

_start:
start:
	; start is loaded at 0x7c00 and is jumped to with CS:IP 0:0x7c00
	; Beginning of the sector is compatible with the FAT/HPFS BIOS parameter block.
	jmp after_BPB				; jump over OEM block

;************************************************************************
;	OEM Parameter block / BIOS Parameter Block
;************************************************************************
BS_OEMName          db  'MINEboot'      	; OEM Name, , must be 8 bytes
BPB_BytesPerSec     dw  512             	; bytes per section
BPB_SecPerClus      db  1               	; sectors per cluster
BPB_RsvdSecCnt      dw  1               	; reserved sector count (boot sector)
BPB_NumFATs         db  2               	; number of FAT tables
BPB_RootEntCnt      dw  224             	; max dir count of root dir
BPB_TotSec16        dw  2880            	; total number of sectors
BPB_Media           db  0xf0            	; drive type
BPB_FATSz16         dw  9               	; Sectors size of each FAT table
BPB_SecPerTrk       dw  18              	; sectors per track
BPB_NumHeads        dw  2               	; number of magnetic heads
BPB_HiddSec         dd  0               	; number of hidden sectors
BPB_TotSec32        dd  0               	; this value effects when BPB_TotSec16 is 0
BS_DrvNum           db  0               	; number of drives
BS_Reserved         db  0               	; Reserved
BS_BootSign	    db  29h             	; boot signature
BS_VolID            dd  0               	; volume ID
BS_VolLab           db  'MOS FLOPPY '   	; volume Label, Must be 11 bytes
BS_FileSysType      db  'FAT12   '      	; file system type, must be 8 bytes

;************************************************************************
; Print message string
;************************************************************************
%macro MSG 1
	push si
	mov si, %1
	call Func_Print_MSG
	pop si
%endmacro

;************************************************************************
;	Data Section
;************************************************************************
BootingMsg 	: db "Start Booting", 0x00
ReadedFailMsg	: db 0x0D, 0x0A, "Read Error!", 0x00
LoaderFileName	: db "LOADER  BIN"
LoadingMsg	: db 0x0D, 0x0A, "Loading LOADER.BIN", 0x00
LoadingProgress : db ".", 0x00
FileNotFoundMsg	: db 0x0D, 0x0A, "No LOADER", 0x00
LoadedSuccessMsg: db 0x0D, 0x0A, "Loaded!", 0x00

ROOT_OFFSET 	equ 0x0500
FAT_OFFSET 	equ 0x2100
LOADER_OFFSET 	equ 0x0000
LOADER_SEG 	equ 0x07e0

;**********************************************************************************
; Bellow is a FAT12 formatted disk:
;
; --------------------------------------------------------------------------------
; | BootSector | ExtraReservedSectors | FAT1 | FAT2 | RootDirectory | DataRegion |
; --------------------------------------------------------------------------------
;**********************************************************************************
dataSectorStart dw 0x0000			; Starting sector of Data Region
cluster		dw 0x0000			; The Cluster of the current Root directory entry

;************************************************************************
;	general setup
;************************************************************************
after_BPB:
	cli					; disable interrupts

	; Far jmp to the next instruction because some bogus BIOSes
	; jump to 07C0:0000 instead of 0000:7C00.
	jmp 0x0000:real_start

real_start:
	; Setup segments to insure they are 0.
	xor ax, ax
	mov ds, ax
	mov es, ax
	mov ss, ax

	; Setup the REAL stack
	mov sp, 0xFFFF

	sti					; restore interrupts
	
	;************************************************************************
	; clear screen
	; 	AH = 06h roll pages
	; 	AL = page num (0 to clear screen)
	; 	BH = color attributes
	; 	CL = left row, CH = left column
	; 	DL = right row, DL = right column
	;************************************************************************
	mov ax, 0x0600
	mov bx, 0x0700
	mov cx, 0x0000
	mov dx, 0x184F
	int 0x10

	;************************************************************************
	; set focus
	; 	AH = 02h set focus
	; 	DL = row
	; 	DH = column
	; 	BH = page num
	;************************************************************************
	mov ax, 0x0200
	mov bx, 0x0000
	mov dx, 0x0000
	int 0x10
	
	;************************************************************************
	; print a Booting message on the screen
	;************************************************************************
	MSG BootingMsg

;************************************************************************
; Load Root Directory Table to es:ROOT_OFFSET
;************************************************************************
LoadRoot:
	;************************************************************************************************************
	; Compute size of root directory and store in "cx"
	; The root directory is a table of 32 byte values that represent information reguarding file and directories
	;************************************************************************************************************
	mov ax, 0x0020				; 32 bytes directory entry
	mul WORD [BPB_RootEntCnt]		; ax * BPB_RootEntCnt: total bytes of The root directory
	div WORD [BPB_BytesPerSec]		; ax * BPB_RootEntCnt / BPB_BytesPerSec: total sectors of The root directory
	xchg ax, cx				; store total sectors of The root directory in cx

	;************************************************************************************************************
	; Compute location of root directory and store in "ax"
	;
	; Bellow is a FAT12 formatted disk: Root Directory is located directly after both FATs and reserved sectors. 
	; In other words, just add the FATs + reserved sectors, and you found the root direcory!
	;
	; --------------------------------------------------------------------------------
	; | BootSector | ExtraReservedSectors | FAT1 | FAT2 | RootDirectory | DataRegion |
	; --------------------------------------------------------------------------------
	;
	; BPB_NumFATs * BPB_FATSz16 + BPB_RsvdSecCnt: starting sector of root directory
	;*************************************************************************************************************
	mov al, BYTE [BPB_NumFATs]		; number of FAT tables
	mul WORD [BPB_FATSz16]			; number of FATs * sectors per FAT: get total number sectors of FATs
	add ax, WORD [BPB_RsvdSecCnt]		; add the reserved sectors: Now AX = starting sector of root directory

	;************************************************************************************************************
	; Compute location of Data Region and store in dataSectorStart
	;
	; location of root directory + size of root directory: starting sector of Data Region
	;************************************************************************************************************
	mov WORD [dataSectorStart], ax
	add WORD [dataSectorStart], cx

	;************************************************************************************************************
	; Read Root directory Sectors into memory address es:ROOT_OFFSET
	; AX = Starting sector (LBA), CX = Number of sectors to read, ES:BX = Buffer to read to
	;************************************************************************************************************
	mov bx, ROOT_OFFSET			; ES:BX = Buffer to read to
	call Func_Read_Sectors

;************************************************************************
; Load FAT to es:FAT_OFFSET
;************************************************************************
LoadFAT:
	;*****************************************************************
	; Compute size of FAT and store in "cx"
	;*****************************************************************
	xor ax, ax
	mov al, BYTE [BPB_NumFATs]		; number of FAT tables
	mul WORD [BPB_FATSz16]			; Sectors size of each FAT table
	xchg ax, cx				; store total sectors of The FAT in cx

	;********************************************************************************************************
	; Compute location of FAT and store in "ax"
	;
	; --------------------------------------------------------------------------------
	; | BootSector | ExtraReservedSectors | FAT1 | FAT2 | RootDirectory | DataRegion |
	; --------------------------------------------------------------------------------
	;********************************************************************************************************
	mov ax, WORD [BPB_RsvdSecCnt] 

	;************************************************************************************************************
	; Read Root directory Sectors into memory address es:FAT_OFFSET
	; AX = Starting sector (LBA), CX = Number of sectors to read, ES:BX = Buffer to read to
	;************************************************************************************************************
	mov bx, FAT_OFFSET			; ES:BX = Buffer to read to
	call Func_Read_Sectors


;************************************************************************
; Find Stage 2: Loader.bin
;************************************************************************
FindFile:
	; browse root directory for binary image
	mov dx, WORD [BPB_RootEntCnt]		; load loop counter: the number of entrys. If we reach 0, file doesnt exist
	mov bx, ROOT_OFFSET			; first Root directory entry was loaded here

	.FINDLOOP:
		cld				; clear direction flag
		mov si, LoaderFileName		; [DS:SI] point to the file name to find
		mov cx, 0x000B			; eleven character name
		mov di, bx			; [ES:DI] point to the current Root directory entry
		repe cmpsb			; Compare bytes [DS:SI] and [ES:DI], the number of bytes stores in CX
		je LoadFile			; Found the file and jump to Load the file
		add bx, 0x0020			; next Root directory entry was loaded here, each entry is 32 bytes
		dec dx
		jz .NOTFOUND
		jmp .FINDLOOP			; continue compare next entry
		
	; if loop the number of entrys (cx) = 0, means file is not found in root directory
	.NOTFOUND:
		MSG FileNotFoundMsg
		mov ah, 0x00			; Function 0
		int 0x16			; invoke BIOS 0x16, Function 0: Await keypress
		int 0x19			; invoke BIOS 0x19: reboot computer

;************************************************************************
; Load File to es:FAT_OFFSET
;************************************************************************
LoadFile:
	mov di, bx				; make sure [ES:DI] point to the current Root directory entry

	MSG LoadingMsg

	; Save cluster number from root directory entry
	; [ES:DI] point to the current Root directory entry, and Bytes 26-27 of each entry is First Cluster
	mov dx, [es:di + 0x001A]		; DX = The First Cluster of the current Root directory entry
	mov WORD [cluster], dx
	
	push WORD LOADER_SEG
	pop es					; ES:BX = Buffer to read to
	mov bx, LOADER_OFFSET
	push bx
	
	.LOADFILELOOP:
		;************************************************************************
		; convert current cluster to LBA:
		; LBA = (cluster - 2) * sectors per cluster
		;
		; AX = the Starting sector (LBA) to read
		;************************************************************************
		.CLUSTERLBA:				
			mov ax, WORD [cluster]
			sub ax, 0x0002			; zero base cluster number
			xor cx, cx
			mov cl, BYTE [BPB_SecPerClus]	; get sectors per cluster
			mul cx				; calculate (cluster - 2) * BPB_SecPerClus
			add ax, WORD [dataSectorStart]	; add base data sector, Now AX = the Starting sector to read

		xor cx, cx
        	mov cl, BYTE [BPB_SecPerClus]		; sectors to read
		pop bx					; ES:BX = Buffer to read to
        	call Func_Read_Sectors
		push bx

		; compute next cluster
		mov ax, WORD [cluster]	; identify current cluster
		mov cx, ax			; copy current cluster
		mov dx, ax			; copy current cluster
		shr dx, 0x0001			; divide by two, cluster / 2
		add cx, dx			; CX = (cluster / 2  + cluster) = cluster * 1.5
		mov bx, FAT_OFFSET		; location of FAT in memory
		add bx, cx
		mov dx, WORD [bx]		; read two bytes from FAT
		shr ax, 0x0001			; cluster / 2: test for odd or even cluster
		jc .ODD_CLUSTER
          
	.EVEN_CLUSTER:
		and dx, 0000111111111111b       ; take low twelve bits
		jmp .DONE
         
	.ODD_CLUSTER:
		shr dx, 0x0004                  ; take high twelve bits
          
	.DONE:
		mov WORD [cluster], dx
		cmp dx, 0x0FF0              	; test for end of file
		jb  .LOADFILELOOP		; dx < 0x0FF0, jmp to .LOADFILELOOP

	.SUCCESS:
		pop bx
		MSG LoadedSuccessMsg
	
LoadSuccess:
	jmp LOADER_SEG:LOADER_OFFSET		; jump to loaded file

;************************************************************************
; message: write the string pointed to by si
;   WARNING: trashes si, ax, and bx
;
; Use BIOS "int 10H Function 0Eh" to write character in teletype mode
;	ah = 0xe	al = character
;	bh = page	bl = foreground color (graphics modes)
;************************************************************************
Func_Print_MSG:
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
;	Func_Read_Sectors ()
; 		- Use BIOS "int 13H Function 02h" to Reads a series of sectors
;
; Parameter:
; 	AX = Starting sector (LBA)
; 	CX = Number of sectors to read
; 	ES:BX = Buffer to read to
; Return:
;	NULL
;************************************************************************

Func_Read_Sectors:
	.MAIN:
		mov di, 0x0005                  ; five retries for error

	.SECTORLOOP:
		pusha				; save registers

	;************************************************************************
	; convert starting sector to CHS:
	; Cylinder/Track = (LBA / SectorsPerTrack) / NumberOfHeads => ch
	; Head   	 = (LBA / SectorsPerTrack) % NumberOfHeads => dh
	; Sector   	 = (LBA % SectorsPerTrack) + 1		   => cl
	;************************************************************************
		.LBACHS:
			xor dx, dx			; prepare dx:ax for operation
        		div WORD [BPB_SecPerTrk]        ; LBA / SectorsPerTrack: AX stores the quotient, DX stores the remainder
        		inc dl				; DX = (LBA % SectorsPerTrack) + 1
			mov cl, dl			; cl = real Sector number
		
			xor dx, dx                      ; prepare dx:ax for operation
			div WORD [BPB_NumHeads]         ; calculate (LBA / SectorsPerTrack) / NumberOfHeads
			mov ch, al			; ch = real Cylinder/Track Num
			mov dh, dl			; dh = real Head Num
		
		mov ax, 0x0201			; Function 2, read 1 sector
		mov dl, BYTE [BS_DrvNum]	; drive Num
		int 0x13                        ; invoke BIOS
		jnc .SUCCESS                    ; test for read error

		xor ax, ax                      ; BIOS reset disk System
		mov dl, BYTE [BS_DrvNum]	; drive Num
          	int 0x13                        ; invoke BIOS
		popa				; restore registers
          	dec di                          ; decrement error counter
		jnz .SECTORLOOP                 ; attempt to read again
		jmp .Failed			; the read operation failed

	.SUCCESS:
		MSG LoadingProgress
		popa
		add bx, WORD [BPB_BytesPerSec]   ; queue next buffer
		inc ax                           ; queue next sector
		loop .MAIN                       ; read next sector, will calculate (cx = cx - 1) and judge whether cx = 0
		ret

	;************************************************************************
	; Show a message and wait for a key before reboot 
	;************************************************************************
	.Failed:
		MSG ReadedFailMsg
		mov ah, 0x00			; Function 0
		int 0x16			; invoke BIOS 0x16, Function 0: Await keypress
		int 0x19			; invoke BIOS 0x19: reboot computer
		ret
     
