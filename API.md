## Application programming

Programs are loaded into RAM starting at the address `APPMEMSTART` (currently
defined as `$E00100`, 256 bytes past the start of RAM). Execution also begins at
this location.

Application programs are executed in supervisor mode. The stack pointer `A7` is
initialized to the end of RAM (`$00F00000`) and registers `D0-D7/A0-A6` are
initialized to `$00000000`.

The interrupt priority level is set to `0`, i.e. interrupts are enabled at
program start.

The UART is enabled and set to 57600 baud, 8 data bits, no parity, 1 stop bit.
The line status interrupt is enabled. A break condition on the serial input
terminates execution of the application and enters the serial loader (see above).

The `TRAP #n` instruction is used to perform system calls. (see below)


### System calls

Application programs can use basic input/output routines in the ROM using the
`TRAP` instruction. (This limits the number of syscalls to 16. Future ROM revisions
may use [A-traps](https://en.wikipedia.org/wiki/Macintosh_Toolbox).)


#### Calling convention

- `D0-D1` - integer arguments.
- `A0-A1` - pointer arguments.
- Additional arguments passed on the stack.
- `D0-D1` - integer return values.
- `A0-A1` - pointer return values.
- `D2-D7/A2-A7/SR` - preserved.
- Registers `D0-D1/A0-A1` not used for return values may have undefined contents
  upon return to the caller. (i.e. the callee may use them as scratch registers)
- The caller is responsible for removing arguments passed on the stack.

The `sys` macro can be used as a wrapper around the `TRAP` instruction.

A common convention for some system calls is to return 0 in `D0` on success, or
a nonzero error code in `D0` on failure. Note that since system calls preserve
condition codes, an explicit `tst d0` is required before branching to an error
handler.

#### `TRAP #0` - `Exit` - return to system

- **Arguments:**
  - none
- **Returns:**
  - none
- **Function:**
  - Terminates the application and returns to the system. This will relaunch
    `STARTUP.BIN` (if present on card) or return to the command shell.

#### `TRAP #1` - `WaitBtn` - wait for button to be pressed

- **Arguments:**
  - none
- **Returns:**
  - none
- **Function:**
  - Pauses execution until the "ENTER" pushbutton on the board is pressed and
    released.


#### `TRAP #2` - `OutChar` - write one character to output device

- **Arguments:**
  - `D0.B` - character to write
- **Returns:**
  - `D0.B` - unmodified
- **Function:**
  - Write one character to the current output device. (The default output device
    is the serial port; see "Input/output customization" below for more info.)
- **Example:**
```
; write a newline character to the output device
        moveq   #$0a,d0
        sys     OutChar
```


#### `TRAP #3` - `OutStr` - write null-terminated string to output device
- **Arguments:**
  - `A0.L` - pointer to null-terminated string
- **Returns:**
  - none
- **Function:**
  - Write a null-terminated string to the current output device.
- **Example:**
```
; write "hello world!" to the output device, followed by a newline
        lea.l   hello,a0
        sys     OutStr
hello:  dc.b    "hello world!\n",0
```


#### `TRAP #4` - `OutFmt` - write formatted string to output device
- **Arguments:**
  - `A0.L` - pointer to null-terminated format string
  - format arguments on stack (caller is responsible for cleanup)
- **Returns:**
  - none
- **Function:**
  - Write a formatted string to the current output device. Format arguments are
    taken from the stack, with the most-recently-pushed quantity treated as the
    first format argument.
  - Unlike C's `printf`, format specifiers are single bytes in the range `$E0-$FF`.
    See `syscalls.inc` for the full list.
  - Since the 68000 stack must always stay word-aligned, byte quantities must
    be pushed on the stack as words.
- **Example:**
```
; writes "567 xyz 89ABCDEF" to the output device, followed by a newline
        move.l  #2309737967,-(sp) ; second argument, printed as 32-bit hex
        move.w  #$237,-(sp)       ; first argument, printed as 16-bit decimal
        lea.l   fmtstr,a0
        sys     OutFmt
        addq    #6,sp
fmtstr: dc.b    FMT_U16," xyz ",FMT_H32,"\n",0
```


#### `TRAP #5` - `InChar` - read one character from input device
- **Arguments:**
  - none
- **Returns:**
  - `D0.B` - received character
- **Function:**
  - Waits for a character to become available from the current input device and
    returns it. (The default input device is the serial port; see "Input/Output
    handlers" below for more info.)
- **Example:**
```
; wait for a character to be typed, then print its hex value surrounded by brackets
        sys     InChar
        move.w  d0,-(sp)
        lea.l   fmtstr,a0
        sys     OutFmt
        addq    #2,sp
fmtstr: dc.b    "[",FMT_H8,"]",0
```


#### `TRAP #6` - `PromptStr` - prompt for string from input device
- **Arguments:**
  - `A0.L` - pointer to destination buffer
  - `D0.L` - maximum allowed length of string
  - `D1.W` - delimiter character and option flags
    - **bits 0-7:** delimiter (terminate prompt when this character is received)
    - **bit 8:** if set, received characters are _not_ echoed to the output device
    - **bit 9:** if set, control characters like Backspace and Ctrl-C are _not_ interpreted
- **Returns:**
  - `A0.L` - unmodified (points to start of string)
  - `(A0)` - user-supplied string with a terminated null byte
  - `D0.L` - number of characters received (excluding null byte)
- **Function:**
  - Prompts for a string from the input device. Returns when the delimiter character
    specified by `D1.B` is received, or (if bit 9 of `D1` is clear) when Ctrl-C (`$03`)
    is received.
  - The string is null-terminated. The buffer pointed to by `A0` should have sufficient
    space for `D0`+1 bytes.
  - If bit 9 of `D1` is clear, the following control characters are interpreted:
    - **Ctrl-C** (`$03`): return immediately with an empty string
    - **Ctrl-H** (`$08`, **DEL** (`$7F`): delete most-recently-entered character
      from the input buffer and (if bit 8 of `D1` is clear) output a backspace
      escape sequence
    - **Ctrl-M** (`CR`, `$0D`): translated to Ctrl-J (`LF`, `$0A`)
- **Example:**
```
; ask for the user's name
        lea.l   name,a0
        moveq   #32,d0    ; max length is 32 bytes (one less than buffer size)
        moveq   #$0a,d1   ; stop when a newline character is received
        sys     PromptStr
; print it
        move.l  a0,-(sp)
        lea.l   fmtstr,a0
        sys     OutFmt
        addq    #4,sp
fmtstr: dc.b    "Hello, ",FMT_S,"!\n",0
name:   dcb.b   33
```

#### `TRAP #7` - `ReadSector` - read one 512-byte sector from the CompactFlash card
- **Arguments:**
  - `D0.L` - sector number (LBA)
  - `D1.L` - number of bytes to read (pass any value >= 512 to load entire sector)
  - `A0.L` - address of destination buffer (must be word-aligned)
- **Returns:**
  - `D0.B` - error code, or 0 if successful
  - `D1.L` - decremented by the number of bytes read
  - `A0.L` - points one byte past the last byte written to the destination buffer
- **Function:**
  - Reads the numbered sector from the CompactFlash card into RAM. An error code
    is returned in `D0` if an error occurs (e.g. no card inserted or invalid sector number)
  - To facilitate reading of multiple sectors into the same buffer, `A0` is advanced
    to point one byte past the last byte written, and `D1` is decremented by the
    actual number of bytes read.
- **Example:**
```
; read the master boot record
        moveq   #0,d0     ; read sector number 0
        moveq   #-1,d1    ; read whole sector
        lea.l   buf,a0
        sys     ReadSector
; print it as a hexdump
        move.l  #512,-(sp) ; number of bytes to dump
        pea     buf        ; start of buffer
        lea.l   fmtstr,a0
        sys     OutFmt
        addq    #8,sp
fmtstr: dc.b    FMT_HEXDUMP,"\n",0
buf:    dcb.b   512
```

#### `TRAP #8` - `ListDirectory` - traverse the root directory of the CompactFlash card
- **Arguments:**
  - `A0.L` - pointer to buffer of at least `DIRBUFSIZE` bytes (must be word-aligned)
  - `D0.L` - should be nonzero on first call; zero on subsequent calls
- **Returns:**
  - `A0.L` - preserved
  - `A1.L` - pointer to 32-byte directory entry within the buffer
  - `D0.W` - status
    - if `D0.W = 0`, there are still entries to iterate over
    - if `D0.W < 0`, iteration is complete
    - if `D0.W > 0`, an error occurred (error code in lower 8 bits)
- **Function:**
  - Iterates over the entries in the root directory of the CompactFlash card.
  - On the first call, the user should pass a nonzero value in `D0`, and should
    set `D0` to zero on subsequent calls.
  - `A0` should be a pointer to a temporary buffer. This can be in a fixed location
    or on the stack. Its contents should not be modified by application code during
    directory traversal.
  - After each call, `A1` points to a 32-byte [FAT16 directory entry](https://en.wikipedia.org/wiki/Design_of_the_FAT_file_system#Directory_entry).
  - Hidden files, deleted files, volume labels, and empty entries are skipped over.
- **Example:**
```
; set up buffer on stack
        link    a6,#-DIRBUFSIZE
        move.l  sp,a0
        moveq   #-1,d0  ; d0 should be nonzero on first call
; loop over entries
.loop:  sys     ListDirectory
        tst.w   d0
        bmi     .done
        bne     .error
        ; do something with the directory entry here
        moveq   #0,d0   ; d0 should be zero on subsequent calls
        bra     .loop
.done:  ; all done
.error: ; handle error
        unlk    a6      ; deallocate temporary buffer
```

#### `TRAP #9` - `FindFile` - look up the named file in the root directory of the CompactFlash card
- **Arguments:**
  - `A0.L` - pointer null-terminated 8.3 filename
  - `A1.L` - pointer to 32-byte buffer where the directory entry will be copied to
- **Returns:**
  - `D0.B` - status code (0 on success, nonzero on error)
  - `A1.L` - preserved
- **Function:**
  - Traverse the root directory until a file with the given name is found.
    Its directory entry is then copied into the 32-byte buffer pointed to by `A1`.
  - The filename should be specified as a DOS-style 8.3 filename (e.g. `FOO.TXT`)
    and is case-insensitive. Long filenames are not considered.
  - Hidden files are ignored. (this is a probably a bug)
- **Example:**
```
; print the size of the file named TEST.TXT
        lea.l   fname,a0
        lea.l   buf,a1
        sys     FindFile
        move.l  FSIZE(a1),-(sp) ; get file size from directory entry
        lea.l   fmtstr,a0
        sys     OutFmt          ; print file size
        addq    #4,sp
fname:  dc.b    "TEST.TXT",0
fmtstr: dc.b    "FILE SIZE: ",FMT_U32," BYTES",0
buf:    dcb.b   32
```


#### `TRAP #10` - `ReadFile` - read the contents of the named file in the root directory of the CompactFlash card into memory
- **Arguments:**
  - `A0.L` - pointer null-terminated 8.3 filename (case-insensitive)
  - `A1.L` - address where the file should be loaded into
  - `D0.L` - maximum number of bytes to load (passing -1 loads entire file)
- **Returns:**
  - `D0.B` - status code (0 on success, nonzero on error)
  - `A0.L` - pointer to beginning of destination memory buffer (orig. value of `A1`)
  - `A1.L` - actual number of bytes loaded
- **Function:**
  - Loads the full contents of the named file into memory.
  - The filename should be specified as a DOS-style 8.3 filename (e.g. `FOO.TXT`)
    and is case-insensitive. Long filenames are not considered.
  - Hidden files are ignored. (this is a probably a bug)
  - Does not return an error if the file is larger than the maximum number of
    bytes specified in `D0`. (this would be a nice enhancement to make it easier
    to detect cases where a file is too big)
- **Example:**
```
; print the first 32K of a file named TEST.TXT
        lea.l   fname,a0
        lea.l   buf,a1
        move.l  #32768,d0
        sys     ReadFile
        move.l  d1,-(sp)  ; number of bytes read
        move.l  a0,-(sp)  ; start of buffer
        lea.l   fmtstr,a0
        sys     OutFmt
        addq    #8,sp
fname:  dc.b    "TEST.TXT",0
fmtstr: dc.b    FMT_BUF,"\n",0
buf:    dcb.b   32768
```


#### `TRAP #11` - `GetDateTime` - read current date and time from the real-time clock
- **Arguments:**
  - none
- **Returns:**
  - `D0.L` - date
    - **bits 31-16:** year (four BCD digits)
    - **bits 15-8:** month (two BCD digits)
    - **bits 7-0:** date (two BCD digits)
  - `D1.L` - weekday and time
    - **bits 26-24:** weekday (one BCD digit)
    - **bits 23-16:** hour (two BCD digits)
    - **bits 15-8:** minute (two BCD digits)
    - **bits 7-0:** second (two BCD digits)
- **Function:**
  - If no real-time clock is present, `D0` and `D1` will both equal 0.
  - If the time is not set (e.g. there has been a power failure), bit 31 of `D0`
    will be set.
- **Example:**
```
; print the date and time
        sys     GetDateTime
        tst.l   d0
        bmi     .time_not_set
        beq     .no_rtc
        lea.l   fmtstr,a0
        movem.l d0-d1,-(sp)
        sys     OutFmt
        addq    #8,sp
fmtstr: dc.b    "DATE IS ",FMT_DATE," AND TIME IS ",FMT_TIME,"\n",0
```

#### `TRAP #12` - `SetDateTime` - set date and time
- **Arguments:**
  - none
- **Returns:**
  - `D0.L` - date
    - **bits 31-16:** year (four BCD digits)
    - **bits 15-8:** month (two BCD digits)
    - **bits 7-0:** date (two BCD digits)
  - `D1.L` - weekday and time
    - **bits 26-24:** weekday (one BCD digit)
    - **bits 23-16:** hour (two BCD digits)
    - **bits 15-8:** minute (two BCD digits)
    - **bits 7-0:** second (two BCD digits)
- **Function:**
  - No validation of the date (bounds-checking fields, ensuring valid BCD) is performed.
- **Example:**
```
; set date and time to Monday, July 6, 2020, 12:34:56
        move.l  #$01123456,d0 ; msb of 01 = Monday
        move.l  #$20200706,d1
        sys     SetDateTime
```

#### `TRAP #13` - `GetSysInfo` - get pointer to system information structure
- **Arguments:**
  - none
- **Returns:**
  - `A0.L` - pointer to system information structure in ROM
    - `0(A0).L` - CPU clock speed in Hz
    - `4(A0).L` - RAM size in bytes
    - `8(A0).L` - ROM size in bytes
    - `12(A0).L` - ROM date
    - `16(A0).W` - ROM major revision
    - `18(A0).W` - ROM minor revision
- **Function:**
  - Allows application code to retrieve system information.


#### `TRAP #14` - reserved


#### `TRAP #15` - `Breakpoint` - enter the debugger
- **Arguments:**
  - none
- **Returns:**
  - none
- **Function:**
  - Prints a register dump and enters the debugger. 


#### Input/output customization

The longword-sized RAM locations `OUTCH_VEC` and `INCH_VEC` contain addressed of
routines used for character output and input. These can be customized to allow
input and output to be redirected.

At boot, the vectors are initialized to routines that write to and read from
the serial port.

`OUTCH_VEC` should contain the address of a function that accepts the byte to
be written in `D0`. It should preserve `A2-A7/D2-D7`.

`INCH_VEC` should contain the address of a function that waits for a byte to
become available and then returns it in `D0`. it should preserve `A2-A7/D2-D7`.

Formatted output using the `OutFmt` system call can also be customized:
- `HEXDIGITS` contains the address of a 16-character string containing the
  character set to use when printing hexadecimal numbers
- `DATE_SEP` contains the byte used as a separator character between components
  when the `FMT_DATE` specifier is used. At boot, it defaults to hyphen `-`.
- `TIME_SEP` contains the byte used as a separator character between components
  when the `FMT_TIME` specifier is used. At boot, it defaults to colon `:`.
- `THOUSANDS_SEP` contains the byte used as a thousands grouping separator when
  printing signed and unsigned decimal values. At boot, it defaults to comma `,`.
  To suppress thousands separators, set it to ASCII NUL. (zero byte)
