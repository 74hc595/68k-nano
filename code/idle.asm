; vim:noet:sw=8:ts=8:sts=8:ai:syn=asm68k
        include "68000app.inc"
        ; slowly fade the LED in and out to indicate we're ready
animate_led:
fadespeed       equ     6
                moveq   #0,d1  ; duty cycle
                moveq   #fadespeed,d2  ; number of periods with given duty cycle
                led_on
.cycle:         move.l  #255,d0
                led_tgl
.loop:          cmp.b   d0,d1   ; invert LED waveform when count == duty cycle value
                bne     .1
                led_tgl
.1:             dbra    d0,.loop
                dbra    d2,.cycle
        ; increment duty cycle
                addq    #1,d1
        ; when duty cycle == 0, invert waveform (change fade direction)
                cmp.b   #0,d1
                bne     .2
                led_tgl
.2:             moveq   #fadespeed,d2
                bra     .cycle
