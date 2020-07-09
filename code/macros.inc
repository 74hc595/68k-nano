; breakpoint (68000 does not have the BKPT instruction)
brk             macro
                trap    #15
                endm

; set overflow flag
sev             macro
                or      #%00000010,ccr
                endm

; clear overflow flag
clv             macro
                and     #%11111101,ccr
                endm

; convenience alias for saving registers on stack
pushm           macro
        if NARG>1
        fail    multiple args passed to pushm--did you use a comma instead of a slash?
        endif
                movem.l \1,-(sp)
                endm
push            macro
        if NARG>1
        fail    multiple args passed to push--use pushm if you want to push multiple registers
        endif
                move.l  \1,-(sp)
                endm

; convenience alias for restoring registers from stack
popm            macro
        if NARG>1
        fail    multiple args passed to popm--did you use a comma instead of a slash?
        endif
                movem.l (sp)+,\1
                endm
pop             macro
        if NARG>1
        fail    multiple args passed to pop--use popm if you want to pop multiple registers
        endif
                move.l  (sp)+,\1
                endm

; Branch to subroutine and link, storing return address in A6.
; (An optimization for calling leaf functions, a la Arm)
bl              macro
                lea     (.ret\@,pc),a6
                bra     \1
.ret\@:
                endm

; Spin for a given number of iterations. (0-65535)
; (clobbers d0)
spin            macro
                move.l  #\1,d0
.spin\@:        dbra    d0,.spin\@
                endm

; Return from subroutine to address in A6.
rl              macro
                jmp     (a6)
                endm

; Declare null-terminated string.
asciz           macro
                rept    \#
                dc.b    \+
                endr
                dc.b    0
                even
                endm

; Declare null-terminated string inline, place its address in A0, and jump
; over it.
litstr          macro
                lea.l   (.str\@,pc),a0
                bra     .next\@
.str\@:
                rept    \#
                dc.b    \+
                endr
                dc.b    0
                even
.next\@:
                endm

; Status register bit offsets
SR_C            equ     0       ; carry flag
SR_V            equ     1       ; overflow flag
SR_Z            equ     2       ; zero flag
SR_N            equ     3       ; negative flag
SR_X            equ     4       ; extend flag
SR_I            equ     8       ; interrupt mask
SR_S            equ     13      ; supervisor mode flag
SR_T            equ     15      ; trace flag
; Condition code register (lower byte of status register) bit offsets
CCR_C           equ     SR_C
CCR_V           equ     SR_V
CCR_Z           equ     SR_Z
CCR_N           equ     SR_N
CCR_X           equ     SR_X

LOUD_NOISES     macro
                move.l  #$d0d0dd00,d0
                move.l  #$d1d1dd11,d1
                move.l  #$d2d2dd22,d2
                move.l  #$d3d3dd33,d3
                move.l  #$d4d4dd44,d4
                move.l  #$d5d5dd55,d5
                move.l  #$d6d6dd66,d6
                move.l  #$d7d7dd77,d7
                move.l  #$a0a0aa00,a0
                move.l  #$a1a1aa11,a1
                move.l  #$a2a2aa22,a2
                move.l  #$a3a3aa33,a3
                move.l  #$a4a4aa44,a4
                move.l  #$a5a5aa55,a5
                move.l  #$a6a6aa66,a6
                endm