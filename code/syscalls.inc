; Calling conventions for system routines
;-------------------------------------------------------------------------------
; D0-D1 - integer arguments
; A0-A1 - pointer arguments
; Additional arguments passed on the stack.
;
; D0-D1 - integer return values
; A0-A1 - pointer return values
; D2-D7 - preserved
; A2-A7 - preserved
; SR    - preserved
;
; Registers D0-D1/A0-A1 not used for return values may have undefined contents
; upon return to the caller. (i.e. the callee may use them as scratch registers)
;
; The caller is responsible for removing arguments passed on the stack.
;
Exit            equ     0
WaitBtn         equ     1
OutChar         equ     2
OutStr          equ     3
OutFmt          equ     4
InChar          equ     5
PromptStr       equ     6
ReadSector      equ     7
ListDirectory   equ     8
FindFile        equ     9
ReadFile        equ     10
GetDateTime     equ     11
SetDateTime     equ     12
GetSysInfo      equ     13
; 14 - reserved for future use
Breakpoint      equ     15


; Syscall macro.
sys             macro
                trap    #\1
                endm

; format specifiers for OutFmt
FMT_BASE        equ     $E0
FMT_C           equ     FMT_BASE+0      ;pop word and print lsb as an ASCII character
FMT_C2          equ     FMT_BASE+1      ;pop word and print as two ASCII characters
FMT_C4          equ     FMT_BASE+2      ;pop longword and print as four ASCII characters
FMT_H8          equ     FMT_BASE+3      ;pop word and print lsb as two hex digits
FMT_H16         equ     FMT_BASE+4      ;pop word and print as four hex digits
FMT_H32         equ     FMT_BASE+5      ;pop longword and print as eight hex digits
FMT_S           equ     FMT_BASE+6      ;pop longword and print null-terminated ASCII string at address
FMT_U8          equ     FMT_BASE+7      ;pop word and print lsb as 1-3 decimal digits (unsigned)
FMT_U16         equ     FMT_BASE+8      ;pop word and print as 1-5 decimal digits (unsigned)
FMT_U32         equ     FMT_BASE+9      ;pop longword and print as 1-10 decimal digits (unsigned)
FMT_D8          equ     FMT_BASE+10     ;pop word and print lsb as 1-3 decimal digits (signed)
FMT_D16         equ     FMT_BASE+11     ;pop word and print as 1-5 decimal digits (signed)
FMT_D32         equ     FMT_BASE+12     ;pop longword and print as 1-10 decimal digits (signed)
FMT_Z8          equ     FMT_BASE+13     ;pop word and print lsb as 5 decimal digits with leading zeros (unsigned)
FMT_Z16         equ     FMT_BASE+14     ;pop word and print as 3 decimal digits with leading zeros (unsigned)
FMT_Z32         equ     FMT_BASE+15     ;pop longword and print as 10 decimal digits with leading zeros (unsigned)
FMT_SRFLAGS     equ     FMT_BASE+16     ;pop word and print as status register flags
FMT_FAULTFLAGS  equ     FMT_BASE+17     ;pop word and print as bus/address error fault flags
FMT_DATE        equ     FMT_BASE+18     ;pop longword and print as date (YYYY/MM/DD)
FMT_TIME        equ     FMT_BASE+19     ;pop longword and print as time (HH:MM:SS)
FMT_HEXDUMP     equ     FMT_BASE+20     ;pop longword length and longword address, print as hex dump
FMT_BUF         equ     FMT_BASE+21     ;pop longword length and longword address, print as ASCII
FMT_FNAME       equ     FMT_BASE+22     ;pop longword address and print 11 characters as a decoded filename
FMT_ERR         equ     FMT_BASE+23     ;pop word and print description of the error code in the lsb

; option flags for PromptStr
PRbNOECHO       equ     8
PRbNOCTRLCHARS  equ     9

PR_NOECHO       equ     (1<<PRbNOECHO)          ;do not echo characters received to output device
PR_NOCTRLCHARS  equ     (1<<PRbNOCTRLCHARS)     ;do not interpret control characters
