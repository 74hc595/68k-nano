; vim:noet:sw=8:ts=8:sts=8:ai:syn=asm68k
;
; Naive implementation of FizzBuzz. (uses DIVU instruction)

        include "68000app.inc"

start:          sys     WaitBtn

                moveq   #1,d3           ; counter
.loop:
                moveq   #0,d2           ; flag
        ; compute modulo 3
                move.l  d3,d0
                divu.w  #3,d0
                swap    d0              ; ignore quotient, get remainder
                tst.w   d0
                bne     .1              ; don't print "Fizz" if remainder is nonzero
                litstr  "Fizz"
                sys     OutStr
                addq.w  #1,d2           ; set flag
.1:     ; compute modulo 5
                move.l  d3,d0
                divu.w  #5,d0
                swap    d0              ; ignore quotient, get remainder
                tst.w   d0
                bne     .2              ; don't print "Buzz" if remainder is nonzero
                litstr  "Buzz"
                sys     OutStr
                addq    #1,d2           ; set flag
.2:     ; if flag not set, print value as decimal
                tst.w   d2
                bne     .3
                move.w  d3,-(sp)
                litstr  FMT_U16,0
                sys     OutFmt
                addq    #2,sp
.3:             moveq   #$0a,d0         ; print newline
                sys     OutChar
                addq    #1,d3
                led_tgl
                move.l  #$40000,d0
                bsr     delay
                bra     .loop


; delay by number of loop iterations in d0 (32-bit)
delay:          subq.l  #1,d0
                bne     delay
                rts
