; vim:noet:sw=8:ts=8:sts=8:ai:syn=asm68k

; base address
UART            equ $A00000

; register offsets
RHR             equ 0   ; receive holding register (read)
THR             equ 0   ; transmit holding register (write)
IER             equ 2   ; interrupt enable register
ISR             equ 4   ; interrupt status register (read)
FCR             equ 4   ; FIFO control register (write)
LCR             equ 6   ; line control register
MCR             equ 8   ; modem control register
LSR             equ 10  ; line status register
MSR             equ 12  ; modem status register
SPR             equ 14  ; scratchpad register (reserved for system use)
DLL             equ 0   ; divisor latch LSB
DLM             equ 2   ; divisor latch MSB
; aliases for register names (used by different manufacturers)
RBR             equ RHR ; receive buffer register
IIR             equ ISR ; interrupt identification register
SCR             equ SPR ; scratch register

; MCR register bits (output lines)
; A bit value of 1 drives the output low
MCR_COPI        equ 0   ; ~DTR -- bit 0 allows for easier shifting
MCR_LED         equ 1   ; ~RTS
MCR_CLK         equ 2   ; ~OUT1
MCR_nSS         equ 3   ; ~OUT2

; MSR register bits (input lines)
MSR_BTN1        equ 6   ; ~RI
MSR_BTN1_PRESS  equ 2   ; TERI
MSR_SQW         equ 5   ; ~DSR
MSR_SQW_CHG     equ 2   ; deltaDSR
MSR_CIPO        equ 7   ; ~DSR -- bit 7 allows for easier shifting

; Wait for the UART to become ready to transmit a character.
; Macro argument 1: register containing the UART base address
tx_wait         macro
.t\@!:          btst.b  #5,LSR(\1)
                beq     .t\@@
                endm

; Transmit a character on the serial port.
; Macro argument 1: effective address of value to transmit
; Macro argument 2: register containing the UART base address
tx_char         macro
                tx_wait \2
                move.b  \1,THR(\2)
                endm

led_on          macro
                bset.b  #MCR_LED,UART+MCR
                endm

led_off         macro
                bclr.b  #MCR_LED,UART+MCR
                endm

led_tgl         macro
                bchg.b  #MCR_LED,UART+MCR
                endm