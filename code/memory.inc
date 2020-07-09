; vim:noet:sw=8:ts=8:sts=8:ai:syn=asm68k

ROMSIZE         equ     $010000 ;64KB
RAMSIZE         equ     $100000 ;1MB
RAMEND          equ     RAM+RAMSIZE
INITIAL_SP      equ     RAMEND
;-------------------------------------------------------------------------------
; Base addresses
ROM             equ     $000000
RAM             equ     $E00000
;-------------------------------------------------------------------------------
; Memory reserved for the system
SYSMEMSTART     equ     RAM
SYSMEMSIZE      equ     256
                rsset	SYSMEMSTART
OUTCH_VEC       rs.l    1
INCH_VEC        rs.l    1
HEXDIGITS       rs.l    1
SEPARATORS      rs.l    1
DATE_SEP        equ     SEPARATORS
TIME_SEP        equ     SEPARATORS+1
THOUSANDS_SEP   equ     SEPARATORS+2

; Mounted filesystem parameters
PARTSIZE        rs.l    1               ; size of the partition in sectors, 0 = not mounted (4 bytes)
BPBSECTOR       rs.l    1               ; sector number of the partition's BIOS Parameter Block (4 bytes)
; These parameters are in reverse order of those in the BPB,
; because they are little-endian quantities that need to be byte-swapped
FATSIZE         rs.w    1               ; size of a FAT in sectors (2 bytes)
MAXRDIRENTS     rs.w    1               ; maximum number of root directory entries (2 bytes)
FATCOPIES       rs.w    1               ; number of FATs (padded to 2 bytes)
RSVDSECTORS     rs.w    1               ; number of reserved sectors (2 bytes)
CLUSTERSIZE     rs.w    1               ; number of sectors per cluster (2 bytes)
RDIRSECTOR      rs.l    1               ; sector number of root directory table (4 bytes)
FATSECTOR       rs.l    1               ; sector number of FAT #1 (4 bytes)
DATASTART       rs.l    1               ; sector number of data region start (4 bytes)
VOLNAME         rs.b    12              ; volume label (11 bytes, one extra for padding)
FSVARSTART      equ     PARTSIZE
FSVARLEN        equ     VOLNAME-FSVARSTART+12

INPUTBUFLEN     equ     64
INPUTBUF        rs.b    INPUTBUFLEN+2   ; command input buffer

;-------------------------------------------------------------------------------
; Application programs can be loaded here
APPMEMSTART    equ     SYSMEMSTART+SYSMEMSIZE

