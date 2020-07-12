; vim:noet:sw=8:ts=8:sts=8:ai:syn=asm68k

        include "68000sbc.inc"

ROM_VER_MAJ     equ     $0000
ROM_VER_MIN     equ     $9910
ROM_DATE_YEAR   equ     $2020
ROM_DATE_MONTH  equ     $07
ROM_DATE_DAY    equ     $11

BAUD_DIV        equ     (((F_CPU*10)/(16*BAUD))+5)/10 ; compute one extra decimal place and round
BAUD_DIV_L      equ     (BAUD_DIV&$FF)
BAUD_DIV_U      equ     ((BAUD_DIV>>8)&$FF)

        org     ROM
                dc.l    INITIAL_SP
                dc.l    RESET
                dc.l    VEC_BUSFAULT
                dc.l    VEC_ADRERROR
                dc.l    VEC_ILLINSTR
                dc.l    VEC_DIVBY0
                dc.l    VEC_CHK
                dc.l    VEC_TRAPV
                dc.l    VEC_PRIVVIOL
                dc.l    VEC_TRACE
                dc.l    VEC_LINE1010
                dc.l    VEC_LINE1111
                dc.l    VEC_RESERVED    ;12
                dc.l    VEC_RESERVED    ;13
                dc.l    VEC_RESERVED    ;14
                dc.l    VEC_UNINIVEC
        rept    8
                dc.l    VEC_RESERVED    ;16-23
        endr
                dc.l    VEC_SPURIOUS
                dc.l    VEC_AUTOVEC1
                dc.l    VEC_AUTOVEC2
                dc.l    VEC_AUTOVEC3
                dc.l    VEC_AUTOVEC4
                dc.l    VEC_AUTOVEC5
                dc.l    VEC_AUTOVEC6
                dc.l    VEC_AUTOVEC7
        ; System calls (TRAP #n)
                dc.l    SYS_Exit                ; 0  - return to system
                dc.l    SYS_WaitBtn             ; 1  - wait for button press and release
                dc.l    SYS_OutChar             ; 2  - single character output
                dc.l    SYS_OutStr              ; 3  - string output
                dc.l    SYS_OutFmt              ; 4  - formatted string output
                dc.l    SYS_InChar              ; 5  - single character input
                dc.l    SYS_PromptStr           ; 6  - prompt for string from input device
                dc.l    SYS_ReadSector          ; 7  - read sector from CF card
                dc.l    SYS_ListDirectory       ; 8  - iterate through directory
                dc.l    SYS_FindFile            ; 9  - find named file in directory
                dc.l    SYS_ReadFile            ; 10 - read file from CF card into memory
                dc.l    SYS_GetDateTime         ; 11 - read time and date from real-time clock
                dc.l    SYS_SetDateTime         ; 12 - set real-time clock time and date
                dc.l    SYS_GetSysInfo          ; 13 - return pointer to system info structure
                dc.l    $FFFFFFFF               ; 14 - reserved
                dc.l    VEC_BREAKPT             ; 15 - break into debugger/monitor
;-------------------------------------------------------------------------------
RESET:
        ; uart init
                lea.l   UART,a1
                move.b  #%00001101,FCR(a1)      ; enable FIFO
                move.b  #%10000011,LCR(a1)      ; 8 data bits, no parity, 1 stop bit, DLAB=1
                move.b  #BAUD_DIV_L,DLL(a1)     ; set divisor latch low byte
                move.b  #BAUD_DIV_U,DLM(a1)     ; set divisor latch high byte
                bclr.b  #7,LCR(a1)              ; disable divisor latch
                clr.b   SCR(a1)                 ; clear the scratch register
                move.b  #(1<<MCR_COPI),MCR(a1)  ; SPI COPI idles low
        ; read the button state at launch
                move.b  MSR(a1),SCR(a1)         ; save the button state into the scratch register
; Welcome message
                led_on
                lea.l   str_startup,a0
                bl      _printstr
                move.l  rom_version,d0
                bl      _printhexl
                moveq   #' ',d0
                tx_char d0,a1
                moveq   #'(',d0
                tx_char d0,a1
                move.l  rom_date,d0
                bl      _printhexl
                lea.l   str_credits,a0
                bl      _printstr
;-------------------------------------------------------------------------------
; Power-on self-test
;-------------------------------------------------------------------------------
                lea.l   str_ramtest,a0  ;print startup message
                bl      _printstr
; test RAM
; unrolled for a bit more speed
        ; write test pattern
                lea.l   RAM,a0          ; start address
                lea.l   RAMEND,a3       ; end address
                move.l  #$A5C99C5A,d0   ; test word 1
                move.l  d0,d1
                not.l   d1              ; test word 2
.1:
        rept 8
                move.l  d0,(a0)+
                move.l  d1,(a0)+
        endr
                cmp.l   a0,a3
                bne     .1
        ; read back
                lea.l   RAM,a0          ; start address
                lea.l   RAMEND,a3       ; end address
.2:
        rept 8
                cmp.l   (a0)+,d0        ; read back longword and compare
                bne     testfail
                cmp.l   (a0)+,d1
                bne     testfail
        endr
                cmp.l   a0,a3
                bne     .2
testpass:
                lea.l   str_testpass,a0
                bl      _printstr
                bra     ready
testfail:
                lea.l   -4(a0),a2       ; back up to address of failure fail
                lea.l   str_testfail,a0 ; print message
                bl      _printstr
                move.l  a2,d0
                bl      _printhexl
; flash LED to indicate failure
lockup:         led_tgl
                move.l  #$8000,d0
.1:             dbra    d0,.1
                bra     lockup
;-------------------------------------------------------------------------------
str_startup:    asciz   "\n\n68K NANO - ROM VERSION "
str_credits:    asciz   ")\n(C) 2020 MATT SARNOFF (MSARNOFF.ORG)\n"
str_ramtest:    asciz   "TESTING RAM..."
str_testpass:   asciz   "PASSED\n"
str_testfail:   asciz   "FAILED AT "
                even
;-------------------------------------------------------------------------------
; print d0 as 8 hex digits to serial port with base address a1
; clobbers d1 and d2
; does not use the stack, suitable for use without RAM
_printhexl:     moveq   #7,d1
.loop:          rol.l   #4,d0           ; nibble to lower 4 bits
                move.w  d0,d2
                and.w   #%1111,d2       ; isolate nibble
                move.b  (hexdigits,pc,d2),d2    ; lookup character
                tx_wait a1              ; print digit
                move.b  d2,THR(a1)
                dbra    d1,.loop
                rl
hexdigits:      dc.b    "0123456789ABCDEF"
;-------------------------------------------------------------------------------
; print null-terminated string in a0 to serial port with base address a1
; clobbers d0
; does not use the stack, suitable for use without RAM
_printstr:      move.b  (a0)+,d0        ; get byte
                beq     .2              ; found the null?
.1:             btst.b  #5,LSR(a1)      ; wait until transmit holding register is empty
                beq     .1
                move.b  d0,THR(a1)      ; transmit byte
                bra     _printstr        ; do next character
.2:             rl


;-------------------------------------------------------------------------------
; System is ready, memory is usable
;-------------------------------------------------------------------------------
ready:
                move.l  #INITIAL_SP,sp  ; reset stack pointer
                move.l  #uart_outchar,OUTCH_VEC ; default i/o is serial port
                move.l  #uart_inchar,INCH_VEC
                move.l  #hexdigits_uc,HEXDIGITS
                move.l  #$2d3a2c00,SEPARATORS   ; hyphen, colon, comma as thousands separator
        ; print system info
                sys     GetSysInfo
                move.l  0(a0),-(sp)     ; clock speed
                move.l  8(a0),-(sp)     ; ROM size
                move.l  4(a0),-(sp)     ; RAM size
                lea.l   fmt_sysinfo,a0
                sys     OutFmt
                lea.l   12(sp),sp
        ; check for real-time clock
                bsr     printtime
        ; enable break interrupt to enter serial loader
                move.b  #%00000100,IER(a1)
                move.w  #$2000,sr       ; enable interrupts
        ; try to mount the filesystem on the CF card
                bsr     fs_mount
                beq     .foundcard
        ; error? print message
                bsr     fs_errorstr
                move.w  d0,-(sp)
                litstr  FMT_ERR,"\n"
                sys     OutFmt
                addq    #2,sp
                bra     idle
.printcardmsg:  sys     OutStr
                bra     idle
.foundcard:     litstr  "CARD DETECTED: ",FMT_U32," KB '",FMT_S,"'\n"
                pea     VOLNAME
                move.l  PARTSIZE,d0
                lsr.l   #1,d0           ; convert sectors to KB
                move.l  d0,-(sp)
                sys     OutFmt
                addq    #8,sp
        ; check the button state at boot (stored in the uart scratch register)
                btst.b  #MSR_BTN1,UART+SCR
                bne     .skipstartup
        ; see if there's a startup file to run
                moveq   #-1,d0          ; TODO fail if binary too big
                lea.l   APPMEMSTART,a1  ; destination
                litstr  "STARTUP.BIN"   ; startup filename
                sys     ReadFile
                tst.b   d0
                bne     .nostartup
        ; load suceeded, run startup file
                litstr  "RUNNING STARTUP.BIN\n"
                sys     OutStr
                bra     launchapp


.nostartup:     move.w  d0,-(sp)        ; error number
                litstr  "CANNOT LOAD STARTUP.BIN - ",FMT_ERR,"\n"
                sys     OutFmt
                addq    #2,sp
                bra     idle

.skipstartup    litstr  "BYPASSING STARTUP.BIN\n"
                sys     OutStr
idle:
                bra     startshell
;                 litstr  "WAITING FOR SERIAL DATA.\n"
;                 sys     OutStr
;         ; slowly fade the LED in and out to indicate we're ready
; animate_led:
; fadespeed       equ     6
;                 moveq   #0,d1  ; duty cycle
;                 moveq   #fadespeed,d2  ; number of periods with given duty cycle
;                 led_on
; .cycle:         move.l  #255,d0
;                 led_tgl
; .loop:          cmp.b   d0,d1   ; invert LED waveform when count == duty cycle value
;                 bne     .1
;                 led_tgl
; .1:             dbra    d0,.loop
;                 dbra    d2,.cycle
;         ; increment duty cycle
;                 addq    #1,d1
;         ; when duty cycle == 0, invert waveform (change fade direction)
;                 cmp.b   #0,d1
;                 bne     .2
;                 led_tgl
; .2:             moveq   #fadespeed,d2
;                 bra     .cycle

;===============================================================================
; Syscalls
;===============================================================================

; Exit - return to system
; Arguments:    none
SYS_Exit:
                bra     ready


; WaitBtn - wait for button press and release
; Arguments:    none
SYS_WaitBtn:
                lea.l   UART,a1
.waitpress:     btst    #MSR_BTN1,MSR(a1)       ; wait for button press
                beq     .waitpress
                moveq   #-1,d0
.debounce:      dbra    d0,.debounce            ; wait for debounce
.waitrelease:   btst    #MSR_BTN1,MSR(a1)       ; wait for button release
                bne     .waitrelease
                rte

; OutChar - write one byte to the current output device
; Arguments:    D0.B - character
SYS_OutChar:
                move.l  OUTCH_VEC,a1
                jsr     (a1)
                rte

; OutStr - write null-terminated string to the serial port
; Arguments:    A0.L - pointer to null-terminated string
SYS_OutStr:
                pushm   a2-a3
                move.l  a0,a2
                move.l  OUTCH_VEC,a3
.1:             move.b  (a2)+,d0        ; get byte
                beq     .2              ; found the null?
                jsr     (a3)
                bra     .1
.2:             popm    a2-a3
                rte

; OutFmt - write formatted string to the current output device
; Arguments:    A0.L - pointer to null-terminated format string
;               format arguments on stack (caller is responsible for cleanup)
SYS_OutFmt:
                pushm   d2/a2-a6
                lea.l   30(sp),a6       ; a6 points to argument list on stack
                move.l  a0,a2
                move.l  OUTCH_VEC,a3    ; pointer to output routine
                move.l  HEXDIGITS,a5    ; pointer tp digit table
fmtchar:        moveq   #0,d0           ; clear upper bytes
                move.b  (a2)+,d0        ; get byte
                add.w   d0,d0           ; multiply by 2 to get table offset
                move.w  fmt_jumptable(pc,d0.w),d1
                jmp     fmt_jumptable(pc,d1.w)

fmt_jumptable:  dc.w    .fmt_nullbyte-fmt_jumptable
        rept    FMT_BASE-1
                dc.w    .fmt_literalchar-fmt_jumptable
        endr
                dc.w    .fmt_char-fmt_jumptable
                dc.w    .fmt_char2-fmt_jumptable
                dc.w    .fmt_char4-fmt_jumptable
                dc.w    .fmt_hex8-fmt_jumptable
                dc.w    .fmt_hex16-fmt_jumptable
                dc.w    .fmt_hex32-fmt_jumptable
                dc.w    .fmt_str-fmt_jumptable
                dc.w    .fmt_u8-fmt_jumptable
                dc.w    .fmt_u16-fmt_jumptable
                dc.w    .fmt_u32-fmt_jumptable
                dc.w    .fmt_d8-fmt_jumptable
                dc.w    .fmt_d16-fmt_jumptable
                dc.w    .fmt_d32-fmt_jumptable
                dc.w    .fmt_z8-fmt_jumptable
                dc.w    .fmt_z16-fmt_jumptable
                dc.w    .fmt_z32-fmt_jumptable
                dc.w    .fmt_srbits-fmt_jumptable
                dc.w    .fmt_fltbits-fmt_jumptable
                dc.w    .fmt_date-fmt_jumptable
                dc.w    .fmt_time-fmt_jumptable
                dc.w    .fmt_hexdump-fmt_jumptable
                dc.w    .fmt_buf-fmt_jumptable
                dc.w    .fmt_fname-fmt_jumptable
                dc.w    .fmt_err-fmt_jumptable

.fmt_nullbyte:
                popm    d2/a2-a6
                rte
.fmt_literalchar:
                lsr.w   #1,d0           ; divide by 2 to get original char
                jsr     (a3)
                bra     fmtchar
.fmt_char:
                move.w  (a6)+,d0        ; get char from stack
                jsr     (a3)
                bra     fmtchar
.fmt_char2:
                move.w  (a6)+,d0        ; get chars from stack
.1:             ror.w   #8,d0           ; print msb first
                jsr     (a3)
                ror.w   #8,d0
                jsr     (a3)            ; print lsb
                bra     fmtchar
.fmt_char4:
                move.l  (a6)+,d0        ; get chars from stack
                swap    d0              ; print msw first
                ror.w   #8,d0           ; print msb of msw
                jsr     (a3)
                ror.w   #8,d0
                jsr     (a3)            ; print lsb of msw
                swap    d0
                bra     .1

.fmt_hex8:      move.w  (a6)+,d0        ; get byte from stack
                pea     fmtchar         ; tail call optimization
                bra     printhexbyte

.fmt_hex32:     move.w  (a6)+,d0        ; get longword from stack
                bsr     printhexword
                ; fall through
.fmt_hex16:     move.w  (a6)+,d0        ; get word from stack
                pea     fmtchar
                bra     printhexword

.fmt_date:      move.w  (a6)+,d0        ; get years
                bclr.l  #15,d0          ; make sure "time not set" bit is clear
                lea.l   DATE_SEP,a4
                bsr     printhexword    ; print years
                move.b  (a4),d0         ; print date separator
                jsr     (a3)
                move.b  (a6)+,d0        ; get month
                bsr     printhexbyte    ; print month
                move.b  (a4),d0         ; print date separator
                jsr     (a3)
                move.b  (a6)+,d0        ; get date
                pea     fmtchar
                bra     printhexbyte    ; print date

.fmt_time:      move.w  (a6)+,d0        ; get weekday/hour
                lea.l   TIME_SEP,a4
                bsr     printhexbyte    ; only print lsb (hour)
                move.b  (a4),d0         ; print time separator
                jsr     (a3)
                move.b  (a6)+,d0        ; get minute
                bsr     printhexbyte
                move.b  (a4),d0         ; print time separator
                jsr     (a3)
                move.b  (a6)+,d0        ; get second
                pea     fmtchar
                bra     printhexbyte

.fmt_hexdump:   move.l  (a6)+,a4        ; get string address
                moveq   #0,d2           ; initialize byte count to 0
.hexdumploop:   cmp.l   (a6),d2
                beq     .dumpend
                moveq   #$0F,d0         ; lower 4 bits of count equal 0?
                and.b   d2,d0
                bne     .midline
                moveq   #$0a,d0         ; print newline
                jsr     (a3)
                move.l  d2,d0           ; print byte offset as 8 hex digits
                swap    d0
                bsr     printhexword    ; upper word
                move.w  d2,d0           ; lower word
                bsr     printhexword
                moveq   #':',d0         ; print colon
                jsr     (a3)
                moveq   #' ',d0         ; print space
                jsr     (a3)

.midline:       move.b  (a4)+,d0
                bsr     printhexbyte    ; print byte
                moveq   #' ',d0         ; print space
                jsr     (a3)
                addq.l  #1,d2           ; increment byte count
                bra     .hexdumploop
.dumpend:
        ; pad out the final line
                neg.w   d2
                and.w   #$000F,d2
                beq     .nopad
                subq    #1,d2
.padloop:       moveq   #' ',d0         ; three spaces
                jsr     (a3)
                jsr     (a3)
                jsr     (a3)
                dbra    d2,.padloop
.nopad:         addq    #4,a6           ; remove count
                moveq   #$0a,d0         ; print final newline
                pea     fmtchar
                jmp     (a3)

.fmt_buf:       move.l  (a6)+,a4        ; get buffer address
                move.l  (a6)+,d2        ; get number of bytes to print
                beq     fmtchar         ; do nothing if zero
.bufloop:       move.b  (a4)+,d0        ; get byte
                jsr     (a3)
                subq.l  #1,d2
                bne     .bufloop
                bra     fmtchar

.fmt_str:       move.l  (a6)+,a4        ; get string address from stack
.strloop:       move.b  (a4)+,d0        ; get byte
                beq     fmtchar         ; found the null? done
                jsr     (a3)
                bra     .strloop

.fmt_fname:     move.l  (a6)+,a0        ; get string address
                lea.l   -14(sp),sp      ; allocate stack space for filename (only need 13 bytes, but sp must stay word-aligned)
                move.l  sp,a1
                bsr     fname_decode
                move.l  a1,a4           ; decoded string address
.fnameloop:     move.b  (a4)+,d0        ; get byte
                beq     .fnamedone      ; found the null? done
                jsr     (a3)
                bra     .fnameloop
.fnamedone:     lea.l   14(sp),sp       ; remove temporary string from stack
                bra     fmtchar

.fmt_err:       move.w  (a6)+,d0        ; get error number from stack
                bsr     fs_errorstr
                beq     .othererr
                move.l  a0,a4           ; valid error msg? print it
                bra     .strloop
        ; no error message? just print error number as hex
.othererr:      pea     fmtchar         ; tail call optimization
                bra     printhexbyte

.fmt_d8:        move.w  (a6)+,d2        ; get word from stack
                ext.w   d2              ; check sign bit
                bpl     .dec8           ; positive? just use the unsigned routine
                moveq   #'-',d0         ; print leading minus
                jsr     (a3)
                neg.b   d2              ; negate and jump to unsigned routine
                and.l   #$FF,d2         ; make sure upper bytes are clear
                bra     .dec8

.fmt_u8:        move.w  (a6)+,d2        ; get word from stack
                andi.w  #$00FF,d2       ; clear upper byte of word
                bra     .dec8

.fmt_z8:        move.w  (a6)+,d2        ; get word from stack
                andi.w  #$00FF,d2       ; clear upper byte of word
                bra     .hundreds

.fmt_z16:       move.w  (a6)+,d2        ; get word from stack
                bra     .tthousands

.fmt_u32:       move.l  (a6)+,d2        ; get longword from stack
.dec32:         cmp.l   #$FFFF,d2
                bls     .dec16          ; if < 65536, use 16-bit routine
                cmp.l   #1000000000,d2
                bcc     .billions
                cmp.l   #100000000,d2
                bcc     .hmillions
                cmp.l   #10000000,d2
                bcc     .tmillions
                cmp.l   #1000000,d2
                bcc     .millions
                cmp.l   #100000,d2
                bcc     .hthousands
                bra     .tthousands_l   ; between 65536 and 99999 inclusive

.fmt_d16:       move.w  (a6)+,d2        ; get word from stack
                bpl     .dec16          ; positive? just use the unsigned routine
                moveq   #'-',d0         ; print leading minus
                jsr     (a3)
                neg.w   d2              ; negate and jump to unsigned routine
                bra     .dec16

.fmt_u16:       move.w  (a6)+,d2        ; get word from stack
.dec16:         cmp.w   #10000,d2
                bcc     .tthousands
                cmp.w   #1000,d2
                bcc     .thousands
.dec8:          cmp.w   #100,d2
                bcc     .hundreds
                cmp.w   #10,d2
                bcc     .tens
                bra     .ones

.fmt_d32:       move.l  (a6)+,d2        ; get longword from stack
                bpl     .dec32          ; positive? just use the unsigned routine
                moveq   #'-',d0         ; print leading minus
                jsr     (a3)
                neg.l   d2              ; negate and jump to unsigned routine
                bra     .dec32

.fmt_z32:       move.l  (a6)+,d2        ; get longword from stack
                cmp.l   #$FFFF,d2
                bhi     .billions       ; if < 65536, print leading zeros and fall through to 16-bit routine
                moveq   #'0',d0
                jsr     (a3)
                jsr     (a3)
                jsr     (a3)
                jsr     (a3)
.tthousands:    move.l  #10000,d1
                moveq   #'/',d0
.loop10000:     addq    #1,d0
                sub.w   d1,d2
                bcc     .loop10000
                add.w   d1,d2
                jsr     (a3)
.thousands:     move.w  #1000,d1
                moveq   #'/',d0
.loop1000:      addq    #1,d0
                sub.w   d1,d2
                bcc     .loop1000
                add.w   d1,d2
                jsr     (a3)

                move.b  THOUSANDS_SEP,d0
                beq     .hundreds
                jsr     (a3)

.hundreds:      moveq   #100,d1
                moveq   #'/',d0
.loop100:       addq    #1,d0
                sub.w   d1,d2
                bcc     .loop100
                add.w   d1,d2
                jsr     (a3)
.tens:          moveq   #10,d1
                moveq   #'/',d0
.loop10:        addq    #1,d0
                sub.w   d1,d2
                bcc     .loop10
                add.w   d1,d2
                jsr     (a3)
.ones:          moveq   #'0',d0
                add.b   d2,d0
                jsr     (a3)
                bra     fmtchar
.billions:      move.l  #1000000000,d1
                moveq   #'/',d0
.loop1e9:       addq    #1,d0
                sub.l   d1,d2
                bcc     .loop1e9
                add.l   d1,d2
                jsr     (a3)

                move.b  THOUSANDS_SEP,d0
                beq     .hmillions
                jsr     (a3)

.hmillions:     move.l  #100000000,d1
                moveq   #'/',d0
.loop1e8:       addq    #1,d0
                sub.l   d1,d2
                bcc     .loop1e8
                add.l   d1,d2
                jsr     (a3)
.tmillions:     move.l  #10000000,d1
                moveq   #'/',d0
.loop1e7:       addq    #1,d0
                sub.l   d1,d2
                bcc     .loop1e7
                add.l   d1,d2
                jsr     (a3)

.millions:      move.l  #1000000,d1
                moveq   #'/',d0
.loop1e6:       addq    #1,d0
                sub.l   d1,d2
                bcc     .loop1e6
                add.l   d1,d2
                jsr     (a3)

                move.b  THOUSANDS_SEP,d0
                beq     .hthousands
                jsr     (a3)

.hthousands:    move.l  #100000,d1
                moveq   #'/',d0
.loop1e5:       addq    #1,d0
                sub.l   d1,d2
                bcc     .loop1e5
                add.l   d1,d2
                jsr     (a3)
.tthousands_l:  move.l  #10000,d1
                moveq   #'/',d0
.loop1e4:       addq    #1,d0
                sub.l   d1,d2
                bcc     .loop1e4
                add.l   d1,d2
                jsr     (a3)
                bra     .thousands

.fmt_srbits:    move.w  (a6)+,d2        ; get word from stack
                lea.l   .srflagchars,a4
                btst.l  #15,d2          ; trace flag
                bsr     printflag
                btst.l  #13,d2          ; supervisor flag
                bsr     printflag
                move.w  d2,d0
                lsr.w   #8,d0           ; get interrupt priority level
                and.b   #7,d0           ; isolate 3 bits
                add.b   #'0',d0         ; convert to ASCII digit
                jsr     (a3)
                btst.l  #4,d2           ; extend flag
                bsr     printflag
                btst.l  #3,d2           ; negative flag
                bsr     printflag
                btst.l  #2,d2           ; zero flag
                bsr     printflag
                btst.l  #1,d2           ; overflow flag
                bsr     printflag
                btst.l  #0,d2           ; carry flag
                bsr     printflag
                bra     fmtchar

.fmt_fltbits:   move.w  (a6)+,d2        ; get word from stack
                lea.l   .fltflagchars,a4
                btst.l  #4,d2           ; read/write flag
                bsr     printflag
                btst.l  #3,d2           ; instruction/not-instruction flag
                bsr     printflag
                and.b   #7,d2           ; isolate 3 bits of function code
                moveq   #'0',d0
                add.b   d2,d0           ; convert function code to ASCII digit
                jsr     (a3)
                bra     fmtchar

.srflagchars:   dc.b    "-T-S-X-N-Z-V-C"
.fltflagchars:  dc.b    "WRIN"
                even

printflag:      beq     .flag0
.flag1:         move.w  (a4)+,d0
                jmp     (a3)
.flag0:         move.b  (a4)+,d0
                addq    #1,a4
                jmp     (a3)


printhexbyte:   andi.l  #$00FF,d0       ; isolate lower byte
                ror.l   #8,d0           ; and move it to the upper byte
_lsb:           rol.l   #4,d0           ; get upper nibble
                move.b  (a5,d0.w),d0    ; digit lookup in table
                jsr     (a3)
                clr.w   d0
                rol.l   #4,d0           ; get lower nibble of upper byte
                move.b  (a5,d0.w),d0    ; digit lookup in table
                jmp     (a3)


printhexword:   swap    d0              ; move to upper word
                clr.w   d0              ; clear lower word
                rol.l   #4,d0           ; get upper nibble of upper byte
                move.b  (a5,d0.w),d0    ; digit lookup in table
                jsr     (a3)
                clr.w   d0
                rol.l   #4,d0           ; get lower nibble of upper byte
                move.b  (a5,d0.w),d0    ; digit lookup in table
                jsr     (a3)
                clr.w   d0
                bra     _lsb

; InChar - read one character from current input device
; Arguments:    none
; Returns:      character in low byte of D0
; Blocks until a character is received.
SYS_InChar:     move.l  INCH_VEC,a1
                jsr     (a1)
                rte


; PromptStr - prompt for string from input device
; Arguments:    A0.L - destination buffer
;               D0.L - maximum length
;               D1.W - delimiter character and flags
;                       bit 9:    if set, do NOT interpret control characters
;                       bit 8:    if set, do NOT echo received characters to the output device
;                                 if clear, received characters are echoed to the output device
;                       bits 7-0: delimiter character
; Returns:      A0.L - unmodified (points to start of string)
;               (A0) - null-terminated string
;               D0.L - number of characters received (excluding null byte)
; The string is null-terminated. The buffer should have enough space for maxlen+1 bytes.
; Control characters supported:
; ^C - return immediately with an empty string
; ^H - backspace one character
; ^? (DEL) - baskspace one character
; ^M (CR) - translated to ^J (LF)
SYS_PromptStr:  pushm   a0/a2-a3/d2
                move.l  a0,a2           ; pointer within buffer
                lea.l   (a0,d0),a3      ; pointer to end of buffer
                move.w  d1,d2           ; delimiter char and flags

.prompt:        move.l  INCH_VEC,a1     ; get character
                jsr     (a1)
.tstchar:       cmp.b   d0,d2
                beq     .founddelim     ; if it's the delimiter character, we're done
                btst.l  #PRbNOCTRLCHARS,d2      ; check for control chars?
                bne     .noctrlchar
        ; check if it's a control character
                cmp.b   #$08,d0         ; backspace
                beq     .backspace
                cmp.b   #$7f,d0         ; delete
                beq     .backspace
                cmp.b   #$03,d0         ; ctrl-c
                beq     .abort
                cmp.b   #$0d,d0         ; CR -> LF
                bne     .noctrlchar
                moveq   #$0a,d0
                bra     .tstchar
.noctrlchar:    cmp.l   a2,a3           ; is there room in the string?
                beq     .prompt         ; if not, ignore the character
        ; there is room in the string and the character is not a delimiter
                move.b  d0,(a2)+        ; store in buffer
        ; echo it?
                btst.l  #PRbNOECHO,d2
                bne     .prompt
                sys     OutChar
                bra     .prompt

.founddelim:    move.b  #0,(a2)         ; null-terminate the string
                move.l  a2,d0           ; save the address of the null byte
                popm    a0/a2-a3/d2     ; a0 now contains pointer to start of buffer
                sub.l   a0,d0           ; d0 contains length of string in buffer
                rte

.backspace:     cmp.l   4(sp),a2        ; can't backspace if we're at the start of the string
                beq     .prompt
                subq    #1,a2
        ; if echo is enabled, print backspace/space/backspace
                btst.l  #PRbNOECHO,d2
                bne     .prompt
                lea.l   .backspacestr,a0
                sys     OutStr
                bra     .prompt

.abort:         popm    a0/a2-a3/d2
                move.b  #0,(a0)         ; return the empty string
                clr.b   d0
                rte

.backspacestr:  dc.b    $08,$20,$08,0


; ReadSector - read one sector from CF card
; Arguments:    D0.L - sector number (LBA)
;               D1.L - size of destination memory buffer (pass any value >= 512 to load entire sector)
;               A0.L - address of destination buffer
; Returns:      D0.B - error code, or 0 if successful
;               D1.L - decremented by number of bytes read
;               A0.L - advanced
; note: A0 must be word-aligned
SYS_ReadSector: move.l  d1,-(sp)
                lea.l   CFCARD,a1
                rol.w   #8,d0           ; byte-swap the block address
                swap    d0
                rol.w   #8,d0
                or.b    #$E0,d0         ; set LBA bits
                movep.l d0,CF_LBA0(a1)  ; write LBA to registers
                move.b  #1,CF_COUNT(a1) ; read one sector
                moveq   #CFCMD_RDSECTOR,d0
                bsr     _cfcard_sendcmd
                bne     .done
                bsr     _cfcard_waitfordata
                bne     .done
        ; cap d1 at 512
                move.l  #SECTORSIZE,d0
                cmp.l   d0,d1
                bls     .1
                move.l  d0,d1
        ; convert bytes to words
.1:             sub.l   d1,(sp)         ; subtract actual number of bytes to read from buffer size on stack
                lsr.w   #1,d1           ; X flag will be set if there's a residual byte left
                beq     .2
                bra     .3
.readloop:      move.w  CF_DATA(a1),d0
                ror.w   #8,d0           ; words need to be byte-swapped
                move.w  d0,(a0)+
.3:             dbra    d1,.readloop
        ; handle the residual byte if X is set
.2:             roxr.b  #1,d1           ; shift into N flag
                bpl     .flush
                move.w  CF_DATA(a1),d0
                move.b  d0,(a0)+
        ; consume the rest of the card's data buffer if there's still data left
.flush:         tst.w   CF_DATA(a1)
                btst.b  #3,CF_STATUS(a1)
                bne     .flush
                moveq   #0,d0           ; no error
.done:          move.l  (sp)+,d1        ; bring remaining buffer capacity back to d1
                rte


; ListDirectory - traverse the root directory on the CF card
; Arguments:    A0.L - pointer to buffer at least DIRBUFSIZE bytes
;               D0.L - should be nonzero on first call; zero on subsequent calls
; Returns:      A0.L - preserved
;               A1.L - pointer to 32-byte directory entry within the buffer
;               D0.W - if 0, there are still entries to iterate over
;                      if < 0, iteration is complete
;                      if > 0, an error occurred
;               (A0) - advanced
; note: A0 must be word-aligned
SYS_ListDirectory:
                tst.l   d0
                beq     .1
        ; D0 nonzero? if so, initializer directory buffer
                bsr     fs_rdirlist
                bne     .error
        ; load first entry
.1:             bsr     fs_dirnext
.error:         rte


; FindFile - return the directory entry for the file with the given name
; Arguments:    A0.L - pointer to null-terminated 8.3 filename
;               A1.L - 32-byte buffer to copy the directory entry to
; Returns:      D0.B - error code (0 on success, nonzero on error)
;               A1.L - unmodified
SYS_FindFile:
                bsr     fs_findfile
                rte


; ReadFile - read file from the root directory of the CF card into memory
; Arguments:    A0.L - pointer to null-terminated 8.3 filename
;               A1.L - memory address where the file should be loaded into
;               D0.L - maximum file size in bytes
; Returns:      D0.B - error code (0 on success, nonzero on error)
;               D1.L - actual file size in bytes
SYS_ReadFile:   bsr     fs_loadfile
                rte


; GetDateTime - read the current date and time from the real-time clock
; Arguments:    none
; Returns:      D0.L - date
;                       bits 31-16: year (four BCD digits)
;                       bits 15-8:  month (two BCD digits)
;                       bits 7-0:   date (two BCD digits)
;               D1.L - weekday and time
;                       bits 26-24: weekday (one BCD digit)
;                       bits 23-16: hour (two BCD digits)
;                       bits 15-8:  minute (two BCD digits)
;                       bits 7-0:   second (two BCD digits)
; On error, D0 and D1 both equal 0.
; Since 00000000 is never a valid date, checking for date valididity only requires
; testing d0.
SYS_GetDateTime:
                link    a6,#-8          ; push a6 and allocate two longwords
                bsr     spi_startxfer   ; assert chip select
                moveq   #0,d0
                bsr     spi_shiftbyte   ; send register number $00
                bsr     spi_shiftbyte   ; read in byte 0 (seconds)
                move.b  d0,-(a6)
                bmi     .rtc_error      ; bit 7 should be 0 -- if it is, RTC not detected
                bsr     spi_shiftbyte   ; read in byte 1 (minutes)
                move.b  d0,-(a6)
                bsr     spi_shiftbyte   ; read in byte 2 (hours)
                move.b  d0,-(a6)
                bsr     spi_shiftbyte   ; read in byte 3 (weekday)
                move.b  d0,-(a6)
                bsr     spi_shiftbyte   ; read in byte 4 (date)
                move.b  d0,-(a6)
                bsr     spi_shiftbyte   ; read in byte 5 (month/century)
                tst.b   d0              ; check century bit
                bmi     .twenty_second_century
        ; 21st century (century=0)
                move.b  d0,-(a6)        ; store byte 5
                bsr     spi_shiftbyte   ; read in byte 6 (year)
                move.b  d0,-(a6)
                move.b  #$20,-(a6)      ; upper 2 year digits
                bra     .1
.twenty_second_century:
                bclr.l  #7,d0           ; clear century bit
                move.b  d0,-(a6)        ; store byte 5
                bsr     spi_shiftbyte   ; read in byte 6 (year)
                move.b  d0,-(a6)
                move.b  #$21,-(a6)      ; upper 2 year digits
.1:             bsr     spi_endxfer     ; deassert chip select

                bsr     spi_startxfer   ; assert chip select
                moveq   #$0F,d0         ; register $0F: control/status
                bsr     spi_shiftbyte   ; send register number $0F
                bsr     spi_shiftbyte   ; read data byte
                tst.b   d0
                bpl     .nopwrloss
                bset.b  #7,(a6)         ; set power-loss bit
.nopwrloss:     bsr     spi_endxfer
                popm    d0-d1/a6        ; pop date/time into return registers, restore a6
                rte
.rtc_error:     bsr     spi_endxfer     ; deassert chip select
                moveq   #0,d0           ; return 0
                moveq   #0,d1
                addq    #8,sp
                pop     a6
                rte


; SetDateTime - set the real-time clock's current date and time
; Arguments:    D0.L - date
;                       bits 31-16: year (four BCD digits)
;                       bits 15-8:  month (two BCD digits)
;                       bits 7-0:   date (two BCD digits)
;               D1.L - weekday and time
;                       bits 26-24: weekday (one BCD digit)
;                       bits 23-16: hour (two BCD digits)
;                       bits 15-8:  minute (two BCD digits)
;                       bits 7-0:   second (two BCD digit)
; No validation of the date (bounds-checking fields, ensuring valid BCD) is performed.
SYS_SetDateTime:
                link    a6,#0
                movem.l d0-d1,-(sp)
                bsr     spi_startxfer   ; assert chip select
                move.l  #$80,d0
                bsr     spi_shiftbyte   ; send register number $89
                move.b  -(a6),d0
                bsr     spi_shiftbyte   ; set byte 0 (seconds)
                move.b  -(a6),d0
                bsr     spi_shiftbyte   ; set byte 1 (minutes)
                move.b  -(a6),d0
                bsr     spi_shiftbyte   ; set byte 2 (hours)
                move.b  -(a6),d0
                bsr     spi_shiftbyte   ; set byte 3 (weekday)
                move.b  -(a6),d0
                bsr     spi_shiftbyte   ; set byte 4 (date)
                move.b  -(a6),d0        ; get month
                btst.b  #0,-2(a6)       ; check lsb of century
                beq     .1
                bset.l  #7,d0           ; if it's 1, set century bit
.1:             bsr     spi_shiftbyte   ; set byte 5 (month/century)
                move.b  -(a6),d0
                bsr     spi_shiftbyte   ; set byte 6 (year)
                bsr     spi_endxfer     ; deassert chip select

                bsr     spi_startxfer   ; assert chip select
                move.l  #$8F,d0         ; send register number $89
                bsr     spi_shiftbyte
                moveq   #$40,d0         ; clear OSF flag
                bsr     spi_shiftbyte   ; send data byte
                bsr     spi_endxfer     ; deassert chip select

                addq    #8,sp
                pop     a6
                rte


; GetSysInfo - get pointer to system information structure
; Arguments:    none
; Returns:      A0.L - pointer to system information structure:
;                0(A0).L - CPU clock speed in Hz
;                4(A0).L - RAM size in bytes
;                8(A0).L - ROM size in bytes
;               12(A0).L - ROM date
;               16(A0).W - ROM major revision
;               18(A0).W - ROM minor revision
SYS_GetSysInfo: lea.l   sysinfo,a0
                rte


;-------------------------------------------------------------------------------
; CompactFlash card
;-------------------------------------------------------------------------------
cfcard_init:    moveq   #-1,d0
.1:             cmp.b   #$50,CFCARD+CF_STATUS
                beq     .found
                dbeq    d0,.1
.notfound:      bclr.b  #7,UART+SCR
                bra     .2
.found:         bset.b  #7,UART+SCR
.2:             rts


; Send a commnd to the CF card.
; Arguments:    command byte in D0
;               CF card base address in A1
; Returns:      error code in low byte of D0: zero if command succeeded
cfcard_sendcmd:
                lea.l   CFCARD,a1
_cfcard_sendcmd:
                move.b  d0,CF_COMMAND(a1)
                moveq   #-1,d0                  ; initialize timeout counter
.busy:          btst.b  #7,CF_STATUS(a1)        ; wait until card is not busy (bit 7 clear)
                beq     .notbusy
                dbra    d0,.busy
.timeout:       moveq   #FSERR_TIMEOUT,d0
                rts

.notbusy:       moveq   #-1,d0                  ; initialize timeout counter
.notready:      btst.b  #4,CF_STATUS(a1)        ; wait until card is ready (bit 4 set)
                bne     .ready
                dbra    d0,.notready
.rdytimeout:    btst.b  #0,CF_STATUS(a1)        ; an error code set?
                bne     .err
                bra     .timeout

.ready:         btst.b  #0,CF_STATUS(a1)        ; check error register
                bne     .err
                moveq   #0,d0                   ; no error? return 0 in D0
                rts
.err:           moveq   #0,d0
                move.b  CF_ERROR(a1),d0         ; return error status in D0
                rts


; Wait until the card is ready for a data transfer.
; Arguments:    CF card base address in A1
; Returns:      error code in low byte of D0: zero if command succeeded
cfcard_waitfordata:
                lea.l   CFCARD,a1
_cfcard_waitfordata:
                moveq   #-1,d0                  ; initialize timeout counter
.nodata:        btst.b  #3,CF_STATUS(a1)        ; wait until data is present (bit 3 set)
                bne     .gotdata
                dbra    d0,.nodata
.datatimeout:   btst.b  #0,CF_STATUS(a1)        ; check error register
                bne     .err
                moveq   #FSERR_TIMEOUT,d0
                rts
.gotdata:       btst.b  #0,CF_STATUS(a1)        ; check error register
                bne     .err
                moveq   #0,d0                   ; no error? return 0 in D0
                rts
.err:           moveq   #0,d0
                move.b  CF_ERROR(a1),d0         ; return error status in D0
                rts



;-------------------------------------------------------------------------------
; FAT16 filesystem
;-------------------------------------------------------------------------------
fs_mount:
        ; clear filesystem info
                move.w  #(FSVARLEN/2)-1,d0
                lea.l   FSVARSTART,a0
.1:             clr.w   (a0)+
                dbra    d0,.1
        ; read the master boot record
                link    a6,#-SECTORSIZE ; allocate a buffer on the stack for the sector
                moveq   #0,d0           ; read sector 0
                move.l  sp,a0
                moveq   #-1,d1          ; read entire sector
                sys     ReadSector
                tst     d0
                bne     .error
        ; ignore the first 446 bytes and get to the first partition table entry
                lea.l   $1BE(sp),a0
        ; check partition type to make sure it's FAT16
                move.b  4(a0),d0        ; get partition type
                cmp.b   #$04,d0         ; FAT16 <32MB
                beq     .isfat16
                cmp.b   #$06,d0         ; FAT16 >=32MB
                beq     .isfat16
                cmp.b   #$0B,d0         ; FAT16 or FAT32
                beq     .isfat16
; partition is not FAT16
.notfat16:      moveq   #FSERR_WRONGTYPE,d0
                bra     .error
.isfat16:
        ; Partition size and LBA of first sector (BIOS Parameter Block)
        ; Both need to be byteswapped
                lea.l   16(a0),a0
                lea.l   PARTSIZE,a1
                move.b  -(a0),(a1)+
                move.b  -(a0),(a1)+
                move.b  -(a0),(a1)+
                move.b  -(a0),(a1)+
                move.b  -(a0),(a1)+
                move.b  -(a0),(a1)+
                move.b  -(a0),(a1)+
                move.b  -(a0),(a1)+
        ; now load the BIOS Parameter Block from the first sector
                move.l  BPBSECTOR,d0
                move.l  sp,a0
                moveq   #-1,d1          ; read entire sector
                sys     ReadSector
                tst     d0
                bne     .error
        ; read backwards, since the values need to be byte-swapped
                lea.l   24(sp),a0
                lea.l   FATSIZE,a1
                move.b  -(a0),(a1)+     ; Logical sectors per FAT
                move.b  -(a0),(a1)+
                subq    #3,a0
                move.b  -(a0),(a1)+     ; Max root directory entries
                move.b  -(a0),(a1)+
                clr.b   (a1)+           ; Number of FATs
                move.b  -(a0),(a1)+
                move.b  -(a0),(a1)+     ; Number of reserved logical sectors
                move.b  -(a0),(a1)+
                clr.b   (a1)+           ; Sectors per cluster
                move.b  -(a0),(a1)+
        ; don't support drives when bytes per sector is not 512
                move.b  -(a0),d0
                lsl.w   #8,d0
                move.b  -(a0),d0
                cmp.w   #SECTORSIZE,d0
                bne     .not512bps
        ; if max root dir entries is 0, this is a FAT32 partition that we can't read
                tst.w   MAXRDIRENTS
                beq     .notfat16
        ; copy volume name
                lea.l   32(a0),a0
                lea.l   VOLNAME,a1
                moveq   #10,d0          ; 11 characters in volume name
.namecopy:      move.b  (a0)+,(a1)+
                dbra    d0,.namecopy
        ; compute FAT sector = BPBSECTOR + RSVDSECTORS
                moveq   #0,d0
                move.w  RSVDSECTORS,d0
                add.l   BPBSECTOR,d0
                move.l  d0,FATSECTOR
        ; compute root directory sector = FATSECTOR + FATSIZE*FATCOPIES
                move.w  FATCOPIES,d1
                mulu.w  FATSIZE,d1
                add.l   d1,d0
                move.l  d0,RDIRSECTOR
        ; calculate size of root directory entry in sectors
        ; (directory entries are 32 bytes and a sector is 512 bytes)
        ; = (MAXRDIRENTS * 32) / 512 = MAXRDIRENTS / 16 = MAXRDIRENTS >> 4
                move.w  MAXRDIRENTS,d1
                lsr.w   #4,d1
                ext.l   d1
        ; calculate first data sector = RDIRSECTOR + (MAXDIRENTS >> 4)
                add.l   d1,d0
                move.l  d0,DATASTART
                
                moveq   #0,d0
.2:             unlk    a6
                rts
.not512bps:     moveq   #FSERR_BPS,d0
.error:         clr.l   PARTSIZE        ; clear PARTSIZE to indicate drive is not mounted
                tst     d0
                bra     .2


; Return a string describing an error code.
; Arguments:    D0 - error code
; Returns:      A0 - pointer to null-terminated string
;               D0 - unmodified
fs_errorstr:    moveq   #0,d1
                move.b  d0,d1
                lsl.w   #2,d1           ; multiply by 4 to get table offset
                lea.l   fserrtable,a0
                move.l  (a0,d1.w),a0    ; set Z flag if null ptr
                rts


; Begin traversal of the root directory.
; Arguments:    A0.L - pointer to uninitialized buffer at least DIRBUFSIZE bytes
; Returns:      D0.B - error code, or 0 if successful
;               (A0) - initialized directory buffer
; note: A0 must be word-aligned
fs_rdirlist:    tst.l   PARTSIZE
                beq     .notmounted
                move.w  MAXRDIRENTS,(a0)        ; initialize entry counter
                move.l  RDIRSECTOR,4(a0)        ; initialize sector number
                moveq   #0,d0
                rts
.notmounted:    moveq   #FSERR_NMOUNTED,d0
                rts


; Return the next entry in the directory being traversed.
; Arguments:    A0.L - pointer to directory buffer
; Returns:      A1.L - pointer to 32-byte directory entry within the buffer
;               D0.W - if 0, there are still entries to iterate over
;                      if < 0, iteration is complete
;                      if > 0, an error occurred
;               (A0) - advanced
; note: A0 must be word-aligned
fs_dirnext:     tst.w   (a0)            ; done with iteration?
                beq     .iterdone
        ; Each sector (512 bytes) holds 16 directory entries.
        ; Need to load a new sector when lower 4 bits of count are 0.
                moveq   #$0F,d0
                and.w   (a0),d0
                bne     .noload
        ; Load the sector.
                push    a0
                move.l  4(a0),d0        ; sector number to d0
                lea.l   8(a0),a0        ; buffer address in a0
                moveq   #-1,d1          ; read entire sector
                sys     ReadSector
                pop     a0
                tst.b   d0              ; check error code
                bne     .done
        ; Advance sector number.
                addq.l  #1,4(a0)
.noload:
        ; Negate lower 4 bits of count to get entry number within sector.
                neg.b   d0
                and.b   #$0F,d0
        ; Multiply by 32 to get offset within sector.
                lsl.w   #5,d0
        ; Load effective address of directory entry into A1
                lea.l   8(a0,d0.w),a1
                subq.w  #1,(a0)         ; decrement entry count
                beq     .iterdone       ; last entry? return -1
        ; If the first character of the filename is $00 or $E5, skip this entry
                tst.b   FNAME(a1)
                beq     fs_dirnext
                cmp.b   #$E5,FNAME(a1)
                beq     fs_dirnext
        ; If the attribute byte is $0F, it's a long filename entry, skip it
                cmp.b   #$0F,FATTRS(a1)
                beq     fs_dirnext
        ; If it's a volume label or hidden file, skip it
                btst.b  #1,FATTRS(a1)
                bne     fs_dirnext
                btst.b  #3,FATTRS(a1)
                bne     fs_dirnext
        ; It's a valid entry. Byteswap the cluster number and file size.
                move.w  FCLUSTER(a1),d0      ; cluster number (2 bytes)
                ror.w   #8,d0
                move.w  d0,FCLUSTER(a1)
                move.l  $1c(a1),d0      ; file size (4 bytes)
                ror.w   #8,d0
                swap    d0
                ror.w   #8,d0
                move.l  d0,$1c(a1)
                moveq   #0,d0           ; return 0 to indicate there's more
                rts
.iterdone:      moveq   #-1,d0
.done:          rts


; Return the directory entry for the file with the given name.
; Arguments:    A0.L - pointer to null-terminated 8.3 filename
;               A1.L - 32-byte buffer to copy the directory entry to
; Returns:      D0.B - error code (0 on success, nonzero on error)
;               A1.L - unmodified
; All 11 bytes of the filename must match byte-for-byte.
; At least DIRBUFSIZE bytes of stack space are required for the temporary buffer.
fs_findfile:    pushm   a2-a3
                move.l  a1,a3
                link    a6,#-(DIRBUFSIZE+12)    ; 12 extra bytes for filename conversion (only need 11 but sp must stay word-aligned)
        ; convert a0 to 11-byte encoded filename
                lea.l   DIRBUFSIZE(sp),a1
                bsr     fname_encode
                bne     .ret
                move.l  a1,a2                   ; a2 points to 11-byte encoded filename
                move.l  sp,a0
        ; initiate listing
                bsr     fs_rdirlist
                bne     .ret
        ; iterate over directory entries until we find the file
.diriter:       move.l  sp,a0           ; directory buffer is at top of stack
                bsr     fs_dirnext
                bmi     .notfound       ; bail if we hit the end of the directory
                bne     .ret            ; bail if there's an error
        ; a1 points to the directory entry. now compare filenames
        ; all 11 bytes of name must be byte-for-byte equal
                move.l  a2,a0
                moveq   #FNAMELEN-1,d0
.namecmp:       cmpm.b  (a0)+,(a1)+
                bne     .diriter        ; skip if names don't match
                dbra    d0,.namecmp
        ; names match! copy the directory entry into the caller's buffer
                lea.l   -FNAMELEN(a1),a1        ; back up to start of directory entry
                moveq   #DIRENTLEN-1,d0
.direntcmp:     move.b  (a1)+,(a3)+
                dbra    d0,.direntcmp
                lea.l   -DIRENTLEN(a3),a1       ; restore a1
                moveq   #0,d0
                bra     .ret
.notfound:      moveq   #FSERR_NOTFOUND,d0
.ret:           unlk    a6
                popm    a2-a3
                rts


; Load one cluster from CF card.
; Arguments:    D0.W - cluster number
;               D1.L - size of destination memory buffer
;               A0.L - destination memory buffer
; Returns:      A0.L - points one byte past the last byte written
;               D0.B - error code (0 on success, nonzero on error)
;               D1.W - bytes left in destination buffer
; note: A0 must be word-aligned
fs_loadcluster: pushm   d2-d3
        ; check cluster number
                cmp.w   #$FFF7,d0       ; indicates that cluster contains a bad sector
                beq     .badsector
                cmp.w   #$FFF0,d0       ; $FFF0-$FFFF are invalid
                bcc     .invclstr
                subq    #2,d0           ; $0001 and $0002 are invalid
                bmi     .invclstr
        ; convert cluster number to sector number
                move.w  CLUSTERSIZE,d2
                mulu    d2,d0           ; multiply by number of sectors per cluster
                add.l   DATASTART,d0    ; add to first sector of data region
        ; initialize counter with number of sectors per cluster
                move.l  d0,d3           ; save cluster number
                subq    #1,d2           ; subtract 1 for dbra
        ; read sector
.readloop:      sys     ReadSector
                tst.b   d0
                bne     .error
        ; stop if the buffer is full
                tst.l   d1
                beq     .bufferfull
        ; otherwise, read next sector in cluster
                addq.l  #1,d3
                move.l  d3,d0
                dbra    d2,.readloop
        ; all done
.bufferfull:    moveq   #0,d0
.error:         popm    d2-d3
                rts
.badsector:     moveq   #FSERR_BADSECTOR,d0
                bra     .error
.invclstr:      moveq   #FSERR_INVCLSTR,d0
                bra     .error



; Starting at the given cluster, load entire file from CF card.
; Arguments:    D0.W - cluster number
;               D1.L - size of destination memory buffer
;               A0.L - destination memory buffer
; Returns:      D0.B - error code (0 on success, nonzero on error)
;               A0.L - advanced
; note: A0 must be word-aligned
fs_loadfileat:
                move.w  d0,-(sp)
                bsr     fs_loadcluster
                bne     .error
                tst.l   d1      ; stop if buffer has been filled
                beq     .done
        ; look up next cluster in FAT
                move.w  (sp)+,d0
                bsr     fs_nextcluster
                bmi     .error  ; msb set if there was an error
        ; if cluster number >= $FFF8, reached the end
        ; otherwise, read next cluster
                cmp.w   #$FFF8,d0
                bcs     fs_loadfileat
        ; all done
                moveq   #0,d0
                rts
.done:          moveq   #0,d0
.error:         addq    #2,sp
                rts


; Given a cluster number, finds the next cluster in the chain.
; Arguments:    D0.W - cluster number
; Returns:      D0.L - next cluster number, or error code
;                       if MSB of D0.L is 1, error code in D0.B
;                       if MSB of D0.L is 0, cluster number in D0.W
; A0 and D1 are preserved.
; Sectors are 512 bytes, so there are 256 FAT entries per cluster.
; Thus, the high byte of D0.W indicates the sector offset to fetch.
fs_nextcluster: pushm   d1-d2/a0
                move.w  d0,d2
                move.l  FATSECTOR,d1    ; get first sector of FAT
                moveq   #0,d0
                move.w  d2,d0           ; isolate lsb of cluster number
                lsr.w   #8,d0
                add.l   d1,d0           ; add sector offset to FAT offset
        ; read the appropriate sector of the FAT
                link    a6,#-SECTORSIZE
                move.l  sp,a0
                moveq   #-1,d1
                sys     ReadSector
                tst     d0
                bne     .error
        ; bring lsb of cluster number back to d0
                moveq   #0,d0
                move.b  d2,d0
        ; table lookup
                add.w   d0,d0           ; multiply table index by 2 to get byte offset
                move.w  (sp,d0.w),d0    ; get next cluster number from FAT
                ror.w   #8,d0           ; needs to be byteswapped
.ret:           unlk    a6
                popm    d1-d2/a0
                tst.l   d0
                rts
.error:         bset.l  #31,d0
                bra     .ret


; Load the full contents of the named file from the CF card into memory.
; Arguments:    D0.L - size of destination memory buffer (-1 to load entire file)
;               A0.L - pointer to null-terminated 8.3 filename
;               A1.L - destination memory buffer
; Returns:      D0.B - error code (0 on success, nonzero on error)
;               D1.L - number of bytes read
;               A0.L - pointer to beginning of destination memory buffer (orig. value of A1)
; Will return an error if the named file is a directory.
fs_loadfile:    pushm   d0/a1           ; save buffer size and address
                link    a6,#-DIRENTLEN  ; allocate buffer for directory entry
                move.l  sp,a1
                bsr     fs_findfile
                bne     .finderror
                btst.b  #4,FATTRS(a1)   ; is it a directory? can't load it.
                bne     .isdir
                move.w  FCLUSTER(a1),d0 ; get cluster number
                move.l  FSIZE(a1),d1    ; get file size
                beq     .emptyfile
                unlk    a6              ; discard directory entry
                cmp.l   (sp),d1         ; take minimum of buffer size and file size
                bcc     .1
                move.l  d1,(sp)
.1:             pop     d1              ; pop buffer size
                move.l  (sp),a0         ; get buffer address, but keep on stack
                bsr     fs_loadfileat   ; load the file!
                sub.l   (sp),a0         ; compute number of bytes read
                move.l  a0,d1           ; and move to return value in d1
                pop     a0              ; bring back original buffer address
                tst.b   d0
                rts
.finderror:     unlk    a6
                addq    #8,sp
                rts
.isdir:         moveq   #FSERR_ISDIR,d0
                bra     .finderror
        ; return early if the file size is 0. an empty file has a cluster number
        ; of zero, which will make fs_loadfileat report an invalid cluster.
        ; instead, return successfully.
.emptyfile:     unlk    a6
                popm    d0/a1
                moveq   #0,d0
                rts


; Convert a "human-readable" filename to the 11-byte encoded filename string
; used in FAT16 directory entries. (e.g. "myfile.txt" -> "MYFILE  TXT").
; - lowercase ASCII characters converted to uppercase
; - filename padded to 8 characters and extension padded to 3 characters with spaces
; - period separating filename and extension is stripped out
; - aborts if input filename contains invalid characters
; Arguments:    A0.L - pointer to null-terminated input filename
;               A1.L - buffer where 11-byte encoded filename will be written to
; Returns:      D0.B - error code, or 0 if filename is valid
;               (A1) - if successful, populated with encoded filename
; The buffers pointed to by A0 and A1 should NOT overlap.
fname_encode:   pushm   a0-a1
        ; start by filling the destination with spaces
                moveq   #FNAMELEN-1,d0
                moveq   #$20,d1
.1:             move.b  d1,(a1)+
                dbra    d0,.1
                lea.l   -FNAMELEN(a1),a1
        ; the empty string isn't a valid filename
                tst.b   (a0)
                beq     .invalid
        ; populate filename
                moveq   #0,d1
.filename:      moveq   #7,d0
.fnameloop:     move.b  (a0)+,d1
                beq     .done           ; stop if we hit the null terminator
                cmp.b   #'.',d1         ; if it's a period, jump to the extension
                beq     .extension
                move.b  (validchartable,pc,d1.w),d1     ; case-convert and check for validity
                beq     .invalid        ; if table entry is 0, character isn't allowed
                move.b  d1,(a1)+        ; character is valid, store it
                dbra    d0,.fnameloop
        ; all 8 chars of filename have been populated--if next char in the
        ; input string is not 0 or '.', the filename is invalid
                tst.b   (a0)
                beq     .done
                cmp.b   #'.',(a0)+
                bne     .invalid
        ; populate extension
.extension:     move.l  4(sp),a1        ; grab pointer to start of dest buffer
                addq    #8,a1           ; advance 8 bytes to start of extension
                moveq   #2,d0
.extloop:       move.b  (a0)+,d1
                beq     .done           ; stop if we hit the null terminator
                move.b  (validchartable,pc,d1.w),d1     ; case-convert and check for validity
                beq     .invalid        ; if table entry is 0, character isn't allowed
                move.b  d1,(a1)+        ; character is valid, store it
                dbra    d0,.extloop
        ; all 3 chars of extension have been populated--if next char in the
        ; input string is not 0, the filename is invalid
                tst.b   (a0)
                bne     .invalid
        ; filename is valid!
.done:          moveq   #0,d0
.ret:           popm    a0-a1
                rts
.invalid:       moveq   #FSERR_INVNAME,d0
                bra     .ret

validchartable:
;                       x0  x1  x2  x3  x4  x5  x6  x7  x8  x9  xA  xB  xC  xD  xE  xF
                dc.b    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
                dc.b    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
                dc.b    0,  '!',0,  '#','$','%','&',$27,'(',')',0,  0,  0,  '-',0,  0
                dc.b    '0','1','2','3','4','5','6','7','8','9',0,  0,  0,  0,  0,  0
                dc.b    '@','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O'
                dc.b    'P','Q','R','S','T','U','V','W','X','Y','Z',0,  0,  0,  '^','_'
                dc.b    '`','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O'
                dc.b    'P','Q','R','S','T','U','V','W','X','Y','Z','{',0,  '}','~',0
                dc.b    $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8A,$8B,$8C,$8D,$8E,$8F
                dc.b    $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9A,$9B,$9C,$9D,$9E,$9F
                dc.b    $A0,$A1,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$AB,$AC,$AD,$AE,$AF
                dc.b    $B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7,$B8,$B9,$BA,$BB,$BC,$BD,$BE,$BF
                dc.b    $C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF
                dc.b    $D0,$D1,$D2,$D3,$D4,$D5,$D6,$D7,$D8,$D9,$DA,$DB,$DC,$DD,$DE,$DF
                dc.b    $E0,$E1,$E2,$E3,$E4,0,  $E6,$E7,$E8,$E9,$EA,$EB,$EC,$ED,$EE,$EF
                dc.b    $F0,$F1,$F2,$F3,$F4,$F5,$F6,$F7,$F8,$F9,$FA,$FB,$FC,$FD,$FE,$FF



; Convert a FAT16-encoded 11-byte filename to a "human-readable" representation.
; - space-padding in filename and extension removed, period added
; - null-terminator added
; Arguments:    A0.L - pointer to 11-byte encoded filename
;               A1.L - buffer where output filename will be written
; Returns:      (A1) - populated with decoded filename
; The buffers pointed to by A0 and A1 should NOT overlap.
fname_decode:   pushm   a0-a1
        ; skip trailing spaces in the filename portion
                addq    #8,a0
                moveq   #$20,d1
                moveq   #7,d0
.1:             cmp.b   -(a0),d1
                bne     .2
                dbra    d0,.1
        ; filename portion was blank
                bra     .extension
.2:     ; filename portion was not blank
                move.l  (sp),a0         ; bring back pointer to start of filename
.3:             move.b  (a0)+,(a1)+     ; copy filename to output buffer
                dbra    d0,.3
.extension:
        ; check the file extension, if it's not blank, add a period
                move.l  (sp),a0         ; bring back pointer to start of filename
                lea.l   FNAMELEN(a0),a0 ; advance to end of extension
                moveq   #$20,d1
                moveq   #2,d0
.4:             cmp.b   -(a0),d1
                bne     .5
                dbra    d0,.4
        ; extension was blank, don't add a period
                bra     .done
        ; add the separating period
.5:             move.b  #'.',(a1)+
                move.l  (sp),a0         ; bring back pointer to start of filename
                addq    #8,a0           ; advance to start of extension
.6:             move.b  (a0)+,(a1)+     ; copy extension to output buffer
                dbra    d0,.6
        ; null-terminate
.done:          move.b  #0,(a1)+
                popm    a0-a1
                rts



;-------------------------------------------------------------------------------
; Bit-banged SPI interface.
;-------------------------------------------------------------------------------

; Start a SPI transfer by asserting chip select.
; Arguments:    none
; Returns:      none
spi_startxfer:  lea.l   UART+MCR,a1
                bclr.b  #MCR_CLK,(a1)   ; clock starts high
                bset.b  #MCR_nSS,(a1)   ; chip select asserted
                rts

; End a SPI transfer by de-asserting chip select.
; Arguments:    none
; Returns:      none
spi_endxfer:    lea.l   UART+MCR,a1
                bclr.b  #MCR_nSS,(a1)   ; chip select deasserted
                rts

; Perform a 1-byte SPI data transfer.
; Arguments:    d0.b - data byte to shift out
; Returns:      d0.b - data byte shifted in
spi_shiftbyte:  push    d2
                lea.l   UART+MCR,a1
                move.b  (a1),d2         ; use d2 as a temporary for MCR write operations
                lsr.b   #1,d2           ; delete bit 0 so we can shift our COPI bit in
                not.b   d0              ; invert data byte to match MCR's inverted polarity
                moveq   #7,d1           ; eight bits
.bitloop:       roxl.b  #1,d0                   ; get data bit to send
                roxl.b  #1,d2                   ; and rotate it into the COPI bit
                addq.b  #(1<<MCR_CLK),d2        ; clock falling edge
                move.b  d2,(a1)
                subq.b  #(1<<MCR_CLK),d2        ; clock rising edge
                move.b  d2,(a1)
                lsr.b   #1,d2           ; delete old COPI bit value
                swap    d2
                move.b  4(a1),d2        ; read CIPO bit from MSR (2 words past MCR)
                lsl.b   #1,d2           ; rotate CIPO bit into X flag
                swap    d2
                dbra    d1,.bitloop
                roxl.b  #1,d0           ; rotate in the last bit received
                not.b   d0              ; invert CIPO bits
                pop     d2
                rts

printtime:      sys     GetDateTime
                tst.l   d0
                bmi     .timenotset
                beq     .no_rtc
                lea.l   fmt_date,a0
                movem.l d0-d1,-(sp)
                sys     OutFmt
                addq    #8,sp
                rts

.no_rtc:        lea.l   str_noclock,a0
                sys     OutStr
                rts
.timenotset:    lea.l   str_timenotset,a0
                sys     OutStr
                rts

;-------------------------------------------------------------------------------
; Serial I/O
;-------------------------------------------------------------------------------
uart_outchar:   lea.l   UART,a1
                tx_char d0,a1
                rts

uart_inchar:    lea.l   UART,a1
.1:             btst.b  #0,LSR(a1)      ; char received?
                beq     .1
                move.b  RHR(a1),d0      ; receive character
                rts

;-------------------------------------------------------------------------------
; Serial loader. Entered by UART interrupt
;-------------------------------------------------------------------------------
; Enter serial loader
VEC_AUTOVEC1:
                lea.l   UART,a1
                btst.b  #4,LSR(a1)      ; was this a break interrupt?
                bne     .1              ; if not, just return
                rte
; wait for the break condition to deassert
.1:             btst.b  #4,LSR(a1)
                bne     .1
; the break stuffs a zero byte into the fifo, remove it
                move.b  RHR(a1),d0
; acknowledge with 'U'
                tx_wait a1
                move.b  #'U',THR(a1)
; now we're in the loader
                lea.l   APPMEMSTART,a0  ; starting RAM address
                moveq   #0,d1           ; byte counter
; receive characters in a loop and write them to RAM
.byteloop:      move.b  LSR(a1),d0      ; wait for a character or a break from host
                btst.l  #4,d0           ; break received?
                bne     .done           ; done with transfer
                btst.l  #0,d0           ; character received?
                beq     .byteloop
                move.b  RHR(a1),(a0)+   ; receive byte and write to RAM
                addq.b  #1,d1           ; increment byte count
                bne     .byteloop
                bchg.b  #MCR_LED,MCR(a1) ; flash LED every 256 bytes (each time byte count lsb is zero)
                bra     .byteloop
.done:
; wait for the break condition to deassert
.2:             btst.b  #4,LSR(a1)
                bne     .2
; the break stuffs a zero byte into the fifo, remove it
                move.b  RHR(a1),d0
; turn off LED and jump to application
launchapp:      bclr.b  #MCR_LED,MCR(a1)
                moveq   #0,d0           ; zero all registers
                move.l  d0,d1
                move.l  d0,d2
                move.l  d0,d3
                move.l  d0,d4
                move.l  d0,d5
                move.l  d0,d6
                move.l  d0,d7
                move.l  d0,a0
                move.l  d0,a1
                move.l  d0,a2
                move.l  d0,a3
                move.l  d0,a4
                move.l  d0,a5
                move.l  d0,a6
                move.l  #INITIAL_SP,sp  ; reset stack pointer
                move.w  #$2000,sr       ; enable interrupts
                jmp     APPMEMSTART     ; begin execution from RAM



; Arg 0: exception name (string)
; Arg 1: exception group (0, 1, or 2. see Table 5-3 "Exception Grouping and Priority")
vecstub         macro
                movem.l d0-d7/a0-a7,-(sp)       ; stack all the registers
                lea.l   (.str,pc),a0
                bra     crash
        .str:   dc.w    $0A2A
                asciz   \1
                even
                endm

g0stub          macro
                movem.l d0-d7/a0-a7,-(sp)       ; stack all the registers
                lea.l   (.str,pc),a0
                bra     grp0_crash
        .str:   dc.w    $0A2A
                asciz   \1
                even
                endm

VEC_BUSFAULT:   g0stub  "BUS FAULT"
VEC_ADRERROR:   g0stub  "ADDRESS ERROR"
VEC_ILLINSTR:   vecstub "ILLEGAL INSTRUCTION"
VEC_DIVBY0:     vecstub "ZERO DIVIDE"
VEC_CHK:        vecstub "CHK"
VEC_TRAPV:      vecstub "TRAPV"
VEC_PRIVVIOL:   vecstub "PRIVILEGE VIOLATION"
VEC_TRACE:      vecstub "TRACE"
VEC_LINE1010:   vecstub "LINE 1010 EMULATOR"
VEC_LINE1111:   vecstub "LINE 1111 EMULATOR"
VEC_RESERVED:   vecstub "RESERVED VECTOR"
VEC_UNINIVEC:   vecstub "UNINITIALIZED INTERRUPT VECTOR"
VEC_SPURIOUS:   vecstub "SPURIOUS INTERRUPT"
VEC_AUTOVEC2:   vecstub "AUTOVECTOR 2"
VEC_AUTOVEC3:   vecstub "AUTOVECTOR 3"
VEC_AUTOVEC4:   vecstub "AUTOVECTOR 4"
VEC_AUTOVEC5:   vecstub "AUTOVECTOR 5"
VEC_AUTOVEC6:   vecstub "AUTOVECTOR 6"
VEC_AUTOVEC7:   vecstub "AUTOVECTOR 7"
VEC_BREAKPT:    vecstub "BREAKPOINT"

;-------------------------------------------------------------------------------
; Dump registers and hang.
; a0 - pointer to null-terminated exception name string
; d7 - number of bytes to unstack when resuming
;-------------------------------------------------------------------------------
crash:          sys     OutStr
                lea.l   fmt_regs,a0
                moveq   #64,d7          ; need to unstack 16 longwords
                bra     dumpregs

grp0_crash:     sys     OutStr
                lea.l   fmt_grp0regs,a0
                moveq   #72,d7          ; need to unstack 16 longwords and 4 words from the group 0 exception frame

dumpregs:       sys     OutFmt          ; print D regs, A regs, and exception frame
                move.l  usp,a0          ; print user stack pointer
                move.l  a0,-(sp)
                lea.l   fmt_usp,a0
                sys     OutFmt
                addq    #4,sp
                bra     prompt

prompt:         lea.l   debugprompt,a0
                sys     OutStr
.waitchar:      btst.b  #0,UART+LSR
                bne     .gotchar
                led_tgl
                spin    $8000
                bra     .waitchar
.gotchar:       move.b  UART+RHR,d0
                sys     OutChar
                cmp.b   #'A',d0         ; a: abort
                beq     ready
                cmp.b   #'a',d0
                beq     ready
                cmp.b   #'C',d0         ; c: continue
                beq     resume
                cmp.b   #'c',d0         ; c: continue
                beq     resume
                cmp.b   #'S',d0
                beq     resume_traceon
                cmp.b   #'s',d0
                bne     prompt
resume_traceon:
                add.l   d7,sp
                or.w    #$8000,(sp) ; set Trace flag
                rte                     ; attempt to resume
; attempt to resume
resume:         add.l   d7,sp
                and.w   #$7FFF,(sp)        ; make sure Trace flag is clear
                rte

hexdigits_uc:   dc.b    "0123456789ABCDEF"
hexdigits_lc:   dc.b    "0123456789abcdef"
debugprompt:    dc.b    "\n[A]BORT/[C]ONTINUE/[S]TEP? ",0
fmt_regs:       dc.b    "\nD0=",FMT_H32,"  D1=",FMT_H32,"  D2=",FMT_H32,"  D3=",FMT_H32,"\n"
                dc.b    "D4=",FMT_H32,"  D5=",FMT_H32,"  D6=",FMT_H32,"  D7=",FMT_H32,"\n"
                dc.b    "A0=",FMT_H32,"  A1=",FMT_H32,"  A2=",FMT_H32,"  A3=",FMT_H32,"\n"
                dc.b    "A4=",FMT_H32,"  A5=",FMT_H32,"  A6=",FMT_H32,"  A7=",FMT_H32,"\n"
                dc.b    "SR=",FMT_SRFLAGS,"  PC=",FMT_H32,"              ", 0

fmt_grp0regs:   dc.b    "\nD0=",FMT_H32,"  D1=",FMT_H32,"  D2=",FMT_H32,"  D3=",FMT_H32,"\n"
                dc.b    "D4=",FMT_H32,"  D5=",FMT_H32,"  D6=",FMT_H32,"  D7=",FMT_H32,"\n"
                dc.b    "A0=",FMT_H32,"  A1=",FMT_H32,"  A2=",FMT_H32,"  A3=",FMT_H32,"\n"
                dc.b    "A4=",FMT_H32,"  A5=",FMT_H32,"  A6=",FMT_H32,"  A7=",FMT_H32,"\n"
                dc.b    "FLAGS=",FMT_FAULTFLAGS,"  ADDR=",FMT_H32,"  IR=",FMT_H16,"\n"
                dc.b    "SR=",FMT_SRFLAGS,"  PC=",FMT_H32,"              ", 0
fmt_usp:        dc.b    "USP=",FMT_H32,"\n",0

fmt_sysinfo:    dc.b    "\nRAM: ",FMT_U32," BYTES\nROM: ",FMT_U32," BYTES\nCPU: ",FMT_U32," HZ\n",0
fmt_date:       dc.b    "TIME IS ",FMT_DATE," ",FMT_TIME,"\n",0
str_noclock:    asciz   "NO REAL-TIME CLOCK DETECTED\n"
str_timenotset: asciz   "DATE/TIME NOT SET\n"
                even

; CF card and filesystem error strings
fserr_noerror:  asciz   "NO ERROR"
fserr_amnf:     asciz   "CARD ERROR: ADDRESS MARK NOT FOUND"            ; CF error register bit 0 set
fserr_abrt:     asciz   "CARD ERROR: COMMAND ABORTED"                   ; CF error register bit 2 set
fserr_idnf:     asciz   "CARD ERROR: INVALID SECTOR ID"                 ; CF error register bit 4 set
fserr_unc:      asciz   "CARD ERROR: UNCORRECTABLE ERROR DETECTED"      ; CF error register bit 6 set
fserr_bbk:      asciz   "CARD ERROR: BAD BLOCK DETECTED"                ; CF error register bit 7 set
fserr_nocard:   asciz   "NO CARD DETECTED"
fserr_notfat16: asciz   "INVALID CARD FORMAT (NOT FAT16)"
fserr_not512:   asciz   "INVALID CARD FORMAT (LARGE SECTORS NOT SUPPORTED)"
fserr_nmounted: asciz   "CARD NOT MOUNTED"
fserr_notfound: asciz   "FILE NOT FOUND"
fserr_invclstr: asciz   "INVALID CLUSTER NUMBER"
fserr_badsectr: asciz   "BAD SECTOR"
fserr_invname:  asciz   "INVALID FILENAME"
fserr_isdir:    asciz   "IS A DIRECTORY"
fserr_other:    asciz   "CARD ERROR: OTHER"

fserrtable:     dc.l    fserr_noerror           ; 0
                dc.l    fserr_amnf              ; 1
                dc.l    fserr_other             ; 2
                dc.l    fserr_other             ; 3
                dc.l    fserr_abrt              ; 4
                dcb.l   16-((*-fserrtable)/4),fserr_other
                dc.l    fserr_idnf              ; 16
                dcb.l   FSERR_TIMEOUT-((*-fserrtable)/4)
                dc.l    fserr_nocard            ; 32 (FSERR_TIMEOUT)
                dc.l    fserr_notfat16          ; 33 (FSERR_WRONGTYPE)
                dc.l    fserr_not512            ; 34 (FSERR_BPS)
                dc.l    fserr_nmounted          ; 35 (FSERR_NMOUNTED)
                dc.l    fserr_notfound          ; 36 (FSERR_NOTFOUND)
                dc.l    fserr_invclstr          ; 37 (FSERR_INVCLSTR)
                dc.l    fserr_badsectr          ; 38 (FSERR_BADSECTOR)
                dc.l    fserr_invname           ; 39 (FSERR_INVNAME)
                dc.l    fserr_isdir             ; 40 (FSERR_ISDIR)
                dcb.l   64-((*-fserrtable)/4),fserr_other
                dc.l    fserr_unc               ; 64
                dcb.l   128-((*-fserrtable)/4),fserr_other
                dc.l    fserr_bbk               ; 128
                dcb.l   256-((*-fserrtable)/4),fserr_other



;-------------------------------------------------------------------------------
; Shell (command interpreter)
;-------------------------------------------------------------------------------
loadaddr        equ     APPMEMSTART
loadlen         equ     RAMEND-loadaddr-256     ; leave 256 bytes for the stack
startshell:     litstr  "\nTYPE ? [ENTER] FOR HELP."
                sys     OutStr
shell:          litstr  "\n> "
                sys     OutStr
                lea.l   INPUTBUF,a0
                moveq   #INPUTBUFLEN,d0
                moveq   #$0a,d1
                sys     PromptStr
                tst.l   d0
                beq     shell
        ; does it begin with a question mark? if so, print help
                move.b  (a0)+,d0
                cmp.b   #'?',d0
                beq     help
        ; otherwise, does it begin with a period? if so, it's an internal command
                cmp.b   #'.',d0
                bne     runfile
        ; interpret as a command
                move.b  (a0)+,d0        ; get command char
        ; skip whitespace, advance a0 to start of argument
.1:             cmp.b   #$20,(a0)+
                beq     .1
                subq    #1,a0           ; back up to first char of argument
                lea.l   commands,a1
                ; D
                cmp.b   (a1)+,d0
                beq     debug
                cmp.b   (a1)+,d0
                beq     debug
                ; H
                cmp.b   (a1)+,d0
                beq     hexdumpfile
                cmp.b   (a1)+,d0
                beq     hexdumpfile
                ; I
                cmp.b   (a1)+,d0
                beq     cardinfo
                cmp.b   (a1)+,d0
                beq     cardinfo
                ; L
                cmp.b   (a1)+,d0
                beq     listfiles
                cmp.b   (a1)+,d0
                beq     listfiles
                ; P
                cmp.b   (a1)+,d0
                beq     printfile
                cmp.b   (a1)+,d0
                beq     printfile
                ; T
                cmp.b   (a1)+,d0
                beq     time
                cmp.b   (a1)+,d0
                beq     time

        ; attempt to load and run named file
runfile:        subq    #1,a0           ; back up to first character
                lea.l   loadaddr,a1
                move.l  #loadlen,d0
                sys     ReadFile
                tst.b   d0
                bne     error
                litstr  "\nRUNNING.\n"
                sys     OutStr
                bra     launchapp

printfile:      lea.l   fmt_printfile,a2
printfile_:     lea.l   loadaddr,a1
                move.l  #loadlen,d0
                sys     ReadFile
                tst.b   d0
                bne     error
                move.l  d1,-(sp)        ; number of bytes read
                move.l  a0,-(sp)        ; start of buffer
                move.l  a2,a0
                sys     OutFmt
                lea.l   8(sp),sp
                bra     shell

hexdumpfile:    lea.l   fmt_hexdump,a2
                bra     printfile_

error:          litstr  "\n",FMT_ERR
                move.w  d0,-(sp)
                sys     OutFmt
                addq    #2,sp
                bra     shell

debug:          brk
                bra     shell

commands:       dc.b    "DdHhIiLlPpTt"


listfiles:      moveq   #$0a,d0
                sys     OutChar
                link    a6,#-DIRBUFSIZE
                move.l  sp,a0
                moveq   #-1,d0                  ; indicates this is the first iteration
.list:          sys     ListDirectory
                tst.w   d0
                bmi     .listdone
                bne     .listerror
                move.l  a0,a2
                btst.b  #4,FATTRS(a1)
                bne     .isdir
                litstr  FMT_FNAME," - ",FMT_U32," BYTE(S)\n"
                move.l  FSIZE(a1),-(sp)         ; size in bytes
                move.l  a1,-(sp)                ; filename
                sys     OutFmt
                addq    #8,sp
                bra     .1
.isdir:         litstr  FMT_FNAME," - DIRECTORY\n"
                move.l  a1,-(sp)                ; filename
                sys     OutFmt
                addq    #4,sp
.1:             move.l  a2,a0
                moveq   #0,d0                   ; indicates subsequent iteration
                bra     .list
.listdone:      unlk    a6
                bra     shell
.listerror:     unlk    a6
                bra     error

time:           tst.b   (a0)            ; no argument? just print time
                beq     showtime
        ; assuming input is formatted correctly, pack into D0 and D1
                movep.l 0(a0),d0
                and.l   #$0F0F0F0F,d0
                lsl.l   #4,d0
                movep.l 1(a0),d1
                and.l   #$0F0F0F0F,d1
                or.l    d1,d0
                movep.l 8(a0),d1
                and.l   #$0F0F0F0F,d1
                lsl.l   #4,d1
                movep.l 9(a0),d2
                and.l   #$0F0F0F0F,d2
                or.l    d2,d1
                sys     SetDateTime

showtime:       sys     GetDateTime
                tst.l   d0
                bmi     .timenotset
                beq     .no_rtc
                litstr  "\n",FMT_DATE," ",FMT_TIME
                movem.l d0-d1,-(sp)
                sys     OutFmt
                addq    #8,sp
                bra     shell
.no_rtc:        litstr  "\nNO REAL-TIME CLOCK DETECTED"
                sys     OutStr
                bra     shell
.timenotset:    litstr  "\nTIME NOT SET"
                sys     OutStr
                bra     shell

help:           lea.l   helpstr,a0
                sys     OutStr
                bra     shell

cardinfo:       tst.l   PARTSIZE
                beq     .nocard
                link    a6,#0
                pea     VOLNAME
                move.w  MAXRDIRENTS,-(sp)
                move.w  CLUSTERSIZE,-(sp)
                move.w  FATSIZE,-(sp)
                move.w  RSVDSECTORS,-(sp)
                move.w  FATCOPIES,-(sp)
                move.l  DATASTART,-(sp)
                move.l  RDIRSECTOR,-(sp)
                move.l  FATSECTOR,-(sp)
                move.l  BPBSECTOR,-(sp)
                move.l  PARTSIZE,-(sp)
                lea.l   fmt_cardinfo,a0
                sys     OutFmt
                unlk    a6
                bra     shell
.nocard:        litstr  "\nNO CARD INSERTED OR NO VALID FILESYSTEM ON CARD"
                sys     OutStr
                bra     shell

fmt_hexdump:    dc.b    "\n",FMT_HEXDUMP,"\n",0
fmt_printfile:  dc.b    "\n",FMT_BUF,0

helpstr:        dc.b    "\nCOMMANDS:\n"
                dc.b    "?          PRINT THIS HELP MESSAGE\n"
                dc.b    "<FILE>     RUN <FILE>\n"
                dc.b    ".L         LIST FILES\n"
                dc.b    ".I         PRINT CARD INFO\n"
                dc.b    ".P <FILE>  PRINT CONTENTS OF <FILE>\n"
                dc.b    ".H <FILE>  HEXDUMP CONTENTS OF <FILE>\n"
                dc.b    ".T         PRINT DATE AND TIME\n"
                dc.b    ".T <DATE>  SET DATE AND TIME\n"
                dc.b    "             (<DATE> FORMAT IS YYYYMMDDWWhhmmss)\n"
                dc.b    0


fmt_cardinfo:   dc.b    "\nPARTITION SIZE:                 ",FMT_U32," SECTORS\n"
                dc.b    "BIOS PARAMETER BLOCK AT SECTOR  ",FMT_U32,"\n"
                dc.b    "FILE ALLOCATION TABLE AT SECTOR ",FMT_U32,"\n"
                dc.b    "ROOT DIRECTORY AT SECTOR        ",FMT_U32,"\n"
                dc.b    "START OF DATA REGION AT SECTOR  ",FMT_U32,"\n"
                dc.b    "COPIES OF FAT:                  ",FMT_U16,"\n"
                dc.b    "RESERVED SECTORS:               ",FMT_U16,"\n"
                dc.b    "FILE ALLOCATION TABLE SIZE:     ",FMT_U16," SECTORS\n"
                dc.b    "CLUSTER SIZE:                   ",FMT_U16," SECTORS\n"
                dc.b    "MAX ROOT DIRECTORY ENTRIES:     ",FMT_U16,"\n"
                dc.b    "VOLUME NAME:                    '",FMT_S,"'\n"
                dc.b    0
                even


; pad out the ROM image
        printt  "Total ROM size:"
        printv  *
                dcb.b   ROMSIZE-20-*,$FF
; ROM image ends with system info and ROM and version number
sysinfo:
clockspeed:     dc.l    F_CPU
ramsize:        dc.l    RAMSIZE
romsize:        dc.l    ROMSIZE
rom_date:       dc.w    ROM_DATE_YEAR
                dc.b    ROM_DATE_MONTH
                dc.b    ROM_DATE_DAY
rom_version:    dc.w    ROM_VER_MAJ
                dc.w    ROM_VER_MIN
