; vim:noet:sw=8:ts=8:sts=8:ai:syn=asm68k
        include "68000app.inc"
start:          sys     WaitBtn
                led_tgl
                lea.l   str,a0
                sys     OutStr
                bra     start
str:            dc.b    "hello world!!\n",0
