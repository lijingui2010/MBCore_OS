;************************************************************************
;		 FAT12.inc	(C) Jim Lee
;
;		FAT12 filesystem for 3-1/2 floppies
;************************************************************************

%ifndef __FAT12_INCLUDE__
%define __FAT12_INCLUDE__

bits	16

%include "Floppy16.inc"				; Floppy drive interface routines

FAT_SEG		equ 0x0050
FAT_OFFSET 	equ 0x0500
ROOT_SEG 	equ 0x0210
ROOT_OFFSET 	equ 0x2100

;************************************************************************
;	Func_Load_Root ()
;		- Load the Root Directory Table to ROOT_SEG:0x0000
;
; Parameter:
; 	NULL
; Return:
;	NULL
;************************************************************************
Func_Load_Root:
	pusha
	push es
	
	xor cx, cx				; clear register
	xor dx, dx

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
	; Read Root directory Sectors into memory address ROOT_SEG:0x0000
	; AX = Starting sector (LBA), CX = Number of sectors to read, ES:BX = Buffer to read to
	;************************************************************************************************************
	push WORD ROOT_SEG
	pop es
	xor bx, bx
	call Func_Read_Sectors
	
	pop es					; restore registers and return
	popa
	ret


;************************************************************************
;	Func_Load_FAT ()
;		- Load the FAT table to FAT_SEG:0x0000
;
; Parameter:
; 	NULL
; Return:
;	NULL
;************************************************************************

Func_Load_FAT:
	pusha
	push es

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
	; Read FAT Sectors into memory address FAT_SEG:0x0000
	; AX = Starting sector (LBA), CX = Number of sectors to read, ES:BX = Buffer to read to
	;************************************************************************************************************
	push WORD FAT_SEG
	pop es
	xor bx, bx
	call Func_Read_Sectors

	pop es					; restore registers and return
	popa
	ret


;************************************************************************
; 	Func_Find_File ()
;		- Search for filename in root directory
;
; The root directory is a table of 32 byte values that represent information reguarding file and directories. 
; This 32 bytes value uses the format:
; 	Bytes 0-7 : DOS File name (Padded with spaces)
; 	Bytes 8-10 : DOS File extension (Padded with spaces)
;	Bytes 11 : File attributes. This is a bit pattern:
; 		Bit 0 : Read Only
;		Bit 1 : Hidden
;		Bit 2 : System
;		Bit 3 : Volume Label
;		Bit 4 : This is a subdirectory
;		Bit 5 : Archive
;		Bit 6 : Device (Internal use)
;		Bit 7 : Unused
;	Bytes 12 : Unused
;	Bytes 13 : Create time in ms
;	Bytes 14-15 : Created time, using the following format:
;		Bit 0-4 : Seconds (0-29)
;		Bit 5-10 : Minutes (0-59)
;		Bit 11-15 : Hours (0-23)
; 	Bytes 16-17 : Created year in the following format:
;		Bit 0-4 : Year (0=1980; 127=2107
;		Bit 5-8 : Month (1=January; 12=December)
;		Bit 9-15 : Hours (0-23)
;	Bytes 18-19 : Last access date (Uses same format as above)
;	Bytes 20-21 : EA Index (Used in OS/2 and NT, don't worry about it)
;	Bytes 22-23 : Last Modified time (See byte 14-15 for format)
;	Bytes 24-25 : Last modified date (See bytes 16-17 for format)
;	Bytes 26-27 : First Cluster
;	Bytes 28-31 : File Size
;
; Parameter:
; 	DS:SI = File name
; Return:
;	AX = The File's index Root directory entry. -1 if error
;************************************************************************

Func_Find_File:
	push cx					; store registers
	push dx
	push bx				
	push es

	mov bx, si				; copy filename for later

	; browse root directory for binary image
	mov dx, WORD [BPB_RootEntCnt]		; load loop counter: the number of entrys. If we reach 0, file doesnt exist
	push WORD ROOT_SEG
	pop es
	xor di, di				; [ES:DI]: first Root directory entry was loaded here

	.FINDLOOP:
		;************************************************************************
		; DOS filenames are limited to 11 bytes, Just need to compare the 11 bytes:
		; Bytes 0-7 : DOS File name (Padded with spaces)
; 		; Bytes 8-10 : DOS File extension (Padded with spaces)
		;************************************************************************
		cld				; clear direction flag
		mov cx, 0x000B			; eleven character name, compare count
		mov si, bx			; File name is in BX
		push di				; Stores current Root directory entry. if found, will pop into AX
		repe cmpsb			; Compare bytes [DS:SI] and [ES:DI], the number of comparisons stores in cx
		je .FOUND			; found the file and jmp to .FOUND
		pop di
		add di, 0x0020			; next Root directory entry was loaded here, each entry is 32 bytes
		dec dx				; if not found, reload loop counter
		jz .NOTFOUND
		jmp .FINDLOOP			; continue compare next entry

	; if loop counter (cx) = 0, means file is not found in root directory
	.NOTFOUND:
		pop es				; restore registers and return
		pop bx
		pop dx
		pop cx
		mov ax, -1			; set error code
		ret

	.FOUND:
		pop ax				; return value into AX contains entry of file, remember the "push di"
		pop es				; restore registers and return
		pop bx
		pop dx
		pop cx
		ret	


;************************************************************************
; 	Func_Load_File ()
;		- Load File
; Parameter:
; 	DS:SI = File to load
;	BX:BP = Buffer to load file to
; Return:
;	AX = -1 on error, 0 on success
;	CX = number of sectors to read
;************************************************************************

Func_Load_File:
	xor cx, cx				; size of file in sectors
	push cx

	.FIND_FILE:
		push bx
		push bp				; BX:BP points to buffer to read to; store it for later

		call Func_Find_File
		cmp ax, -1			; find our file. DS:SI contains our filename
		jne .LOAD_FILE_PRE		; find the file and jmp to .LOAD_FILE_PRE

		pop bp				; restore registers and return
		pop bx
		pop cx
		mov ax, -1			; not find the file and set error code
		ret

	.LOAD_FILE_PRE:
		mov di, ax			; make sure [ES:DI] point to the current Root directory entry

		; get the first cluster
		push WORD ROOT_SEG		; Root Directory Segment
		pop es
		mov dx, [es:di + 0x001A]	; ES:DI points to root directory, and Bytes 26-27 of each entry is First Cluster
		mov WORD [cluster], dx		; store file's first cluster in cluster
		pop bx				; convert bx:bp to es:bx, BX:BP points to buffer to read to
		pop es
		push bx				; store location for later again
		push es
		call Func_Load_FAT		; Load FAT table

	.LOAD_FILE:
		; load the cluster
		mov ax, WORD [cluster]		; cluster to read
		pop es				; ES:BX points to buffer to read to
		pop bx
		call Func_Cluster_TO_LBA	; Get the Starting sector (stores in AX) to read
		xor cx, cx
		mov cl, BYTE [BPB_SecPerClus]	; sectors to read
		call Func_Read_Sectors		; read the cluster

		pop cx
		inc cx				; add one more sector to counter
		push cx
		push bx				; store location for later again
		push es
		
		mov ax, FAT_SEG			; start reading from FAT, ES:BX points to FAT
		mov es, ax
		xor bx, bx

		;*****************************************************************************************
		; get next cluster
		;
		; Each 12 bit FAT cluster entry is displayed.
		; Notice all even number clusters accopy all of the first byte, but part of the second. 
		; And all odd clusters accopy a part of their first byte, but all of the second!
		; 
		; so what we need to do is to read a 2 bytes (word) value from the FAT (This is our cluster).
		; If the cluster is even, Mask out the top 4 bits, as it belongs to the next cluster.
		; If it is odd, shift it down 4 bits (to discard the bits used by the first cluster.) 
		;
                ;                  |                              |
   		; 01011101      00111010    01110101  00111101 00111101  00111010  0011110 00111100
   		; |                |               |              |             |             |
   		; |                |1st cluster    |              |-3rd cluster-|             |
   		; |-0 cluster  ----|               |2nd cluster---|             |4th cluster--|
		;
		;*****************************************************************************************
		mov ax, WORD [cluster]		; identify current cluster from FAT
		mov cx, ax			; copy current cluster
		mov dx, ax			; copy current cluster
		shr dx, 0x0001			; currentCluster / 2
		add cx, dx			; CX = (currentCluster / 2  + currentCluster) = currentCluster * 1.5
		
		mov bx, 0			; location of FAT in memory
		add bx, cx
		mov dx, WORD [es:bx]		; read two bytes from FAT

		shr ax, 0x0001			; currentCluster / 2: test for odd or even cluster
		jc .ODD_CLUSTER

	;*****************************************************************************************
	; Remember that each entry in the FAT is a 12 but value. If it represents
	; a cluster (0x002 through 0xFEF) then we only want to get those 12 bits
	; that represent the next cluster
	;*****************************************************************************************

	.EVEN_CLUSTER:
		and dx, 0000111111111111b	; take low 12 bits
		jmp .DONE

	.ODD_CLUSTER:
		shr dx, 0x0004			; take high 12 bits

	.DONE:
		mov WORD [cluster], dx
		cmp dx, 0x0FF0			; test for end of file marker 
		jb .LOAD_FILE			; dx < 0x0FF0, jmp to .LOAD_FILE

	.SUCCESS:
		pop es
		pop bx
		pop cx
		xor ax, ax
		ret
		

%endif		; __FAT12_INCLUDE__
