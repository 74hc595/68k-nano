# 68k nano

A minimal single-board computer based on the venerable ["Texas Cockroach"](https://youtu.be/UaHtGf4aRLs?t=2074)
Motorola 68000 16/32-bit microprocessor.

![Board image](/images/board.jpg)

## Features

- 68HC000 processor running at 12MHz
- 1MB RAM
- 64KB ROM
- 16550 UART providing a 5V FTDI serial port
- 44-pin IDE connector for CompactFlash card adapter
- Connector for SparkFun DS3234 real-time clock
- Only two 74HC glue logic chips required

This might be capable of running uClinux, but I haven't tried it!


## Hardware

The 68k nano hardware is simple and straightforward. It can be built on a
breadboard (though there may be stability issues at higher clock speeds).


### Memory map

Due to the minimal address decoding circuitry, accessing certain memory regions
will cause multiple devices to be selected. This should be avoided.

```
$000000-0xFFFF     ROM (repeats 16 times)
$100000-1FFFFF  X  Forbidden (multiple devices selected)
$200000-2FFFFF     ROM (mirror of $000000-0FFFFF)
$300000-$7FFFF  X  Forbidden (multiple devices selected)
$800000-8FFFFF     Open bus (available for expansion)
$900000-9xxxxF     CompactFlash card
$A00000-AxxxxF     UART
$B00000-BFFFFF  X  Forbidden (multiple devices selected)
$C00000-CFFFFF     RAM
$D00000-DFFFFF  X  Forbidden (multiple devices selected)
$E00000-EFFFFF     RAM (mirror of $C00000-CFFFFF)
$F00000-FFFFFF  X  Forbidden (multiple devices selected)
```

This is summarized with the following equations:

```
/ROMSEL  = /A23
/RAMSEL  =  A22
/UARTSEL =  A23 * /A22 * A21
/CARDSEL =  A20
```


### I/O

The 16550's modem control lines are used as a bit-banged SPI interface. A 7-pin
header is provided for [SparkFun's "DeadOn" DS3234 real-time clock board](https://www.sparkfun.com/products/10160).
Access to SPI is therefore achieved via the 16550's Modem Control Register and Line
Status Register.

A 2mm pitch 44-pin IDE connector is provided for storage devices. This is
intended for CompactFlash card adapters (operating in 16-bit True IDE mode) but
may also work with parallel ATA hard disks. (not tested)


### Interrupts

All interrupt sources are routed through the 16550. All 16550 interrupts invoke
the level 1 autovector (vector offset `$064`) and it is up to the software to
determine the source of the interrupt.


### Building it

`bom.txt` contains a bill of materials. As of this writing (July 2020), all
components are available from major distributors like [Mouser](https://mouser.com)
and [Jameco](https://jameco.com). If purchasing 68000s from eBay, note that [vintage
ICs are a common target of counterfeiters](https://medium.com/supplyframe-hardware/the-underground-parts-store-identifying-counterfeit-computer-chips-3020adbb01f7).
For this project I used [Toshiba TMP68HC000P-12 chips from Jameco](https://www.jameco.com/shop/ProductDisplay?catalogId=10001&langId=-1&storeId=10001&productId=2288039) and
they work just fine.

All parts are through-hole for that extra vintage feel. Note that some of the
decoupling capacitors (C4, C5, C6, C8, and C10) are underneath ICs, so open-frame
IC sockets are required. Large IC sockets can also be expensive (_especially_
DIP-64 sockets!) so a cheaper alternative is to use [breakaway SIP 0.1" machine-pin
headers](https://www.amazon.com/dp/B0187LTEX2/ref=cm_sw_em_r_mt_dp_U_M5IbFbVJ3NVDH).

Due to the spacing of the chips, Textool ZIF sockets won't fit. For the ROMs
U5 and U7, [Aries 28-526-10 low-profile ZIF sockets](https://www.jameco.com/shop/ProductDisplay?catalogId=10001&langId=-1&storeId=10001&productId=102745) will fit,
but _barely_. The release latch may scrape against the RAM IC to its north,
but I've found that it's not a huge deal.

You can use either a full-can or half-can oscillator for X1. Note that if using
an oscillator with a frequency other than 12 MHz, you'll have to update the `F_CPU`
value in the Makefile. (see below) Also note that, if using a 68HC000, the minimum
clock frequency is 4 MHz.

The 44-pin IDE header accepts a CompactFlash adapter board such as [this one](https://www.amazon.com/gp/product/B07Y2MTS13), available from Amazon and other vendors.


## Software

This project uses the [vasm](http://sun.hasenbraten.de/vasm/) assembler and a
standard Makefile. Python 3 and [PySerial](https://pythonhosted.org/pyserial/)
are also required to upload programs to the computer via the serial port. The
Makefile also contains rules for burning ROM images to AT28C256 EEPROMs using
[minipro](https://gitlab.com/DavidGriffith/minipro/) if you have a [TL866II+](https://www.jameco.com/shop/ProductDisplay?catalogId=10001&langId=-1&storeId=10001&productId=2297823)
programmer. (If you don't have a chip programmer, you should get a TL866II+, they
are reasonably priced and available from a number of vendors.)


### Building it

Running `make rom` will build the ROM image and split it into "odd" and "even"
halves, `rom-l.bin` and `rom-u.bin`. Each file should be 32,768 bytes exactly.
Running `make burnrom` will write these files to a pair of AT28C256 EEPROMs
using minipro.

**Note:** if using an oscillator with a frequency other than 12 MHz, change the
value of `F_CPU` appropriately in the Makefile.

The `BAUD` variable in the Makefile sets the baud rate of the serial interface.
Use the included script `baudrate.py` to determine which baud rates are usable
with a given oscillator frequency.


### ROM features

The ROM is fairly minimal--it's closer to a bootloader than an operating system.
It supports loading programs from the root directory of a FAT16-formatted
CompactFlash card and some basic debugging functionality. It also provides a
small set of system calls for I/O to simplify application programs.

The power-up process is as follows:
- Initialize serial port to 57600 baud, 8 data bits, no parity, 1 stop bit.
- Test RAM. If a RAM failure occurs, the failed address is written to the serial
port and the Status LED flashes rapidly. (This is prone to failure when running
on a breadboard at higher clock speeds.)
- Check for the real-time clock module. If found, print the current date and
time to the serial port.
- Check for a CompactFlash card, and attempt to mount the first partition if it
is FAT16-formatted.
- If the "ENTER" pushbutton is NOT held down:
  - If a valid card is inserted, check for a file in the root directory named
    `STARTUP.BIN`. If found, load it into RAM and execute it.
  - If `STARTUP.BIN` was not loaded, enter an interactive command shell.
- If the "ENTER" pushbutton is held down, enter the interactive command shell
  and bypass execution of `STARTUP.BIN`.

Example startup sequence (without `STARTUP.BIN`):
```
68K NANO - ROM VERSION 00009900 (20200708)
(C) 2020 MATT SARNOFF (MSARNOFF.ORG)
TESTING RAM...PASSED

RAM: 1,048,576 BYTES
ROM: 65,536 BYTES
CPU: 12,000,000 HZ
TIME IS 2020-07-07 19:14:36
CARD DETECTED: 250,608 KB 'NO NAME    '
CANNOT LOAD STARTUP.BIN - FILE NOT FOUND

TYPE ? [ENTER] FOR HELP.
>
```

### Command shell

If `STARTUP.BIN` was not loaded, an interactive command shell is available via
the serial port. (57600 baud, 8 data bits, no parity, 1 stop bit). Type `?` and
press Enter for a list of supported commands. They are summarized briefly here.
Note that commands and filenames are _case-insensitive_.

- `.L` - list files in the root directory of the CompactFlash card. Long filenames
  and hidden files are not displayed.
- `.I` - print low-level information about the filesystem on the CompactFlash card.
- `.P file` - print the contents of the file named `file` in the root directory
  of the CompactFlash card as ASCII text.
- `.H file` - print the contenxt of the file named `file` in the root directory
  of the CompactFlash card as a hexdump.
- `.T` - print the current date and time as reported by the real-time clock module.
- `.T YYYYMMDDWWhhmmss` - set the current date and time. The new date/time should
  be expressed as a string of 16 decimal digits with no separators. `WW` should
  be a 2-digit weekday between `01` and `07` inclusive. You can obtain the
  current date/time in this format with the Unix command `date "+%Y%m%d0%u%H%M%S"`.
- `.D` - enter the debugger.
- `file` - any other command is interpreted as a filename to be loaded from the
  root directory of the CompactFlash card and executed. Unlike MS-DOS, the file
  extension _is_ required. e.g. to execute the file named _foo.bin_, enter `FOO.BIN`.
  (`FOO` will attempt to load a file named _foo_ with no extension).


### Serial loader

A break condition on the serial input line enters the serial loader, allowing
programs to be uploaded to RAM from a host computer. The Python script `serload.py`
and the Makefile rules `make load` and `make run` take care of this.


### Debugging

A hardware exception causes a transfer into a basic debugger. The Status LED
flashes rapidly and a register dump is printed to the serial port:

```
*ILLEGAL INSTRUCTION
D0=00000064  D1=0000000A  D2=00000038  D3=00000009
D4=00000000  D5=00000000  D6=00000000  D7=00000000
A0=00E0003C  A1=00001D40  A2=00000000  A3=00F00000
A4=00000000  A5=00000000  A6=000001E2  A7=00EFFFFA
SR=-S0--Z--  PC=00001D3A              USP=7F7EFFDF

[A]BORT/[C]ONTINUE/[S]TEP?
```

Supported commands are:
- `a` - abort execution and return to system.
- `c` - (attempt to) continue execution. (the Trace bit is cleared)
- `s` - set the Trace bit and single-step.

`TRAP #15` is treated as a breakpoint instruction that also enters the debugger.


### API

See `API.md` for a detailed description of the system calls provided by the ROM
API and guidelines for application programs.


## Known issues and shortcomings

- ROM is always present at the bottom of the address space, so the exception
  vectors cannot be overridden by application code.
- Interrupt handling is a hack, since all interrupt sources go through the 16550.
- Address decoding leaves little room for expansion.
- System calls should set the condition codes to simplify application code.

## Future directions

This is a small-scale project. I'm thinking about desiging a larger 68000 board
in the future, so I probably won't continue working on this one. Here are some
project ideas that I won't have time to tackle, but you're welcome to:

- Running an existing 68k monitor like [TUTOR](http://www.easy68k.com/paulrsm/mecb/mecb.htm)
  or [zBug](http://www.kswichit.com/68k/68k.html).
- Running a language like [EhBASIC](https://github.com/jefftranter/68000/tree/master/ehbasic) or a Forth.
- Adding FAT16 write support.
- Booting [uClinux](https://www.bigmessowires.com/2014/11/17/68-katy-68000-linux-on-a-solderless-breadboard/) like Steve Chamberlin did with [68 Katy](https://www.bigmessowires.com/category/68katy/).
- Using the DS3234 real-time clock's SQW output to generate a periodic interrupt.
  (likely necessary for uClinux)


## About

Open hardware, released under the terms of the 3-clause BSD license.

Copyright 2020 Matt Sarnoff.

http://twitter.com/txsector

http://msarnoff.org
