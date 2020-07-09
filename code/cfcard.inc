; vim:noet:sw=8:ts=8:sts=8:ai:syn=asm68k

; base address
CFCARD          equ     $900000

; task file register offsets
CF_DATA         equ     0       ; data register (16-bit read/write)
CF_ERROR        equ     3       ; error register (8-bit read)
CF_FEATURE      equ     3       ; feature register (8-bit write)
CF_COUNT        equ     5       ; sector count register (8-bit read/write)
CF_SECTOR       equ     7       ; sector number register (8-bit read/write)
CF_LBA0         equ     7       ;  also, LBA bits 7-0
CF_CYLL         equ     9       ; cylinder low register (8-bit read/write)
CF_LBA1         equ     9       ;  also, LBA bits 15-8
CF_CYLH         equ     11      ; cylinder high register (8-bit read/write)
CF_LBA2         equ     11      ;  also, LBA bits 23-16
CF_DRIVEHEAD    equ     13      ; drive select/head register (8-bit read/write)
CF_LBA3         equ     13      ;  also, LBA bits 27-24
CF_STATUS       equ     15      ; status register (8-bit read)
CF_COMMAND      equ     15      ; command register (8-bit write)

; task file register offsets for word access
CF_ERROR_W      equ     CF_ERROR-1
CF_FEATURE_W    equ     CF_FEATURE-1
CF_COUNT_W      equ     CF_COUNT-1
CF_SECTOR_W     equ     CF_SECTOR-1
CF_LBA0_W       equ     CF_LBA0-1
CF_CYLL_W       equ     CF_CYLL-1
CF_LBA1_W       equ     CF_LBA1-1
CF_CYLH_W       equ     CF_CYLH-1
CF_LBA2_W       equ     CF_LBA2-1
CF_DRIVEHEAD_W  equ     CF_DRIVEHEAD-1
CF_LBA3_W       equ     CF_LBA3-1
CF_STATUS_W     equ     CF_STATUS-1
CF_COMMAND_W    equ     CF_COMMAND-1

; commands
CFCMD_IDENTIFY  equ     $EC
CFCMD_RDSECTOR  equ     $20

; errors
; if bit 5 is clear, it's an error reported by the card
; if bit 5 is set, it's a filesystem error
FSERR_TIMEOUT   equ     32      ; no card detected
FSERR_WRONGTYPE equ     33      ; filesystem is not FAT16
FSERR_BPS       equ     34      ; bytes per sector is not 512
FSERR_NMOUNTED  equ     35      ; filesystem not mounted
FSERR_NOTFOUND  equ     36      ; file not found
FSERR_INVCLSTR  equ     37      ; invalid cluster number
FSERR_BADSECTOR equ     38      ; cluster contains a bad sector
FSERR_INVNAME   equ     39      ; invalid filename
FSERR_ISDIR     equ     40      ; file is a directory

; Size of a sector. Hardcoded.
; Drives with a different sector size are not supported.
SECTORSIZE      equ     512
; Size of the buffer required for directory listing
DIRBUFSIZE      equ     SECTORSIZE+8
; Size of a FAT16 directory entry
DIRENTLEN       equ     32

; FAT16 directory entry field offsets
FNAME           equ     0
FNAMELEN        equ     11
FATTRS          equ     11
FCLUSTER        equ     26
FSIZE           equ     28
