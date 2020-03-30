;************************************************************************
;		 Floppy16.inc	(C) Jim Lee
;
;		Floppy drive interface routines
;************************************************************************

%ifndef __FLOPPY_INCLUDE__
%define __FLOPPY_INCLUDE__

bits 16

;************************************************************************
;	OEM Parameter block / BIOS Parameter Block
;
; BPB Begins 3 bytes from start. Do a far jump, which is 3 bytes in size.
; If you use a short jump, add a "nop" after it to offset the 3rd byte.
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

;**************************************************************************************************
; Bellow data are used for Converting LBA (Logical Block Address) to CHS (Cylinder/Head/Sector)
;**************************************************************************************************
absoluteSector db 0x00
absoluteHead   db 0x00
absoluteTrack  db 0x00

;**********************************************************************************
; Bellow is a FAT12 formatted disk:
;
; --------------------------------------------------------------------------------
; | BootSector | ExtraReservedSectors | FAT1 | FAT2 | RootDirectory | DataRegion |
; --------------------------------------------------------------------------------
;**********************************************************************************
dataSectorStart dw 0x0000
cluster		dw 0x0000

;************************************************************************
; 	Func_Cluster_TO_LBA ()
; 		- Convert Cluster to LBA (Logical Block Address) 
;
; A cluster represents a series of contiguous sectors on disk
; The File Allocation Table (FAT) is a list of entrys that map to each of these clusters. 
; However, when the cluster number in the FAT table is converted to the number of linear sectors,
; it needs to be reduced by 2 (The first two items of the FAT table are reserved, 
; but the Data Region is stored from 0 in order to not waste space)
;
; LBA = (cluster - 2) * sectors per cluster;
; 
; Parameter:
;	AX = cluster to convert
; Return:
;	AX = Starting sector (LBA) to read
;************************************************************************

Func_Cluster_TO_LBA:
	push cx
	sub ax, 0x0002				; zero base cluster number
	xor cx, cx
	mov cl, BYTE [BPB_SecPerClus]		; get sectors per cluster
	mul cx					; calculate (cluster - 2) * BPB_SecPerClus
	add ax, WORD [dataSectorStart]		; add base data sector, Now AX = the Starting sector to read
	pop cx
	ret

;************************************************************************
;	Func_LBA_TO_CHS ()
; 		- Convert LBA (Logical Block Address) to CHS (Cylinder/Head/Sector)
;
; The LBA represents an indexed location on disk. The first block being 0,
; then 1, and so on. 
;
; absolute Cylinder/Track = (LBA / SectorsPerTrack) / NumberOfHeads
; absolute Head   	  = (LBA / SectorsPerTrack) % NumberOfHeads
; absolute Sector   	  = (LBA % SectorsPerTrack) + 1
;
; Parameter:
;	AX = LBA Address to convert
; Return:
;	absoluteTrack, absoluteHead, absoluteSector
;************************************************************************

Func_LBA_TO_CHS:
	xor dx, dx                            	; prepare dx:ax for operation
        div WORD [BPB_SecPerTrk]           	; LBA / SectorsPerTrack: AX stores the quotient, DX stores the remainder
        inc dl                                	; adjust for sector 0
        mov BYTE [absoluteSector], dl
        xor dx, dx                            	; prepare dx:ax for operation
        div WORD [BPB_NumHeads]			; calculate (LBA / SectorsPerTrack) / NumberOfHeads
        mov BYTE [absoluteHead], dl
        mov BYTE [absoluteTrack], al
        ret

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
		mov di, 0x0005                    	; five retries for error
	.SECTORLOOP:
		push ax
          	push bx
          	push cx
          	call Func_LBA_TO_CHS               	; convert starting sector to CHS
          	mov  ah, 0x02                      	; BIOS read sector
          	mov  al, 0x01                      	; read one sector
          	mov  ch, BYTE [absoluteTrack]      	; track
          	mov  cl, BYTE [absoluteSector]     	; sector
          	mov  dh, BYTE [absoluteHead]       	; head
          	mov  dl, BYTE [BS_DrvNum]          	; drive
          	int  0x13                          	; invoke BIOS
          	jnc  .SUCCESS                      	; test for read error
          	xor  ax, ax                        	; BIOS reset disk
          	int  0x13                          	; invoke BIOS
          	pop  cx
          	pop  bx
          	pop  ax
          	dec  di                            	; decrement error counter
          	jnz  .SECTORLOOP                   	; attempt to read again
          	jmp .FAILED				; the read operation failed

	.SUCCESS:
          	pop  cx
          	pop  bx
          	pop  ax
          	add  bx, WORD [BPB_BytesPerSec]   	; queue next buffer
          	inc  ax                            	; queue next sector
          	loop .MAIN                         	; read next sector, will calculate (cx = cx - 1) and judge whether cx = 0
          	ret

	.FAILED:
		mov ah, 0x00			; Function 0
		int 0x16			; invoke BIOS 0x16, Function 0: Await keypress
		int 0x19			; invoke BIOS 0x19: reboot computer
		ret

%endif		; __FLOPPY_INCLUDE__
