# By default, use the first connected FTDI USB-serial cable we find. (macOS)
# May be overridden by setting the $FTDI_DEV environment variable in your shell
# configuration.
FTDI_DEV ?= $(firstword $(wildcard /dev/cu.usbserial-*))

# Clock frequency in Hz
F_CPU = 12000000
# Serial baud rate
BAUD  = 57600

# make         - build test program
# make load    - load test program binary using serial loader
TESTPROG  = fizzbuzz

# make rom     - build ROM image
# make burnrom - burn ROM image to EEPROMs using minipro
ROM_SRC   = rom.asm
ROM_SREC  = rom.srec
ROM_LST   = rom.lst
ROM_BIN_L = rom-l.bin
ROM_BIN_U	= rom-u.bin
ROM_DASM  = rom-dasm.lst
ROM_BIN   = $(ROM_BIN_L) $(ROM_BIN_U)

# Assembler and disassembler selection
AS  = vasmm68k_mot
DAS = vda68k
SERIALTERM = python3 -m serial.tools.miniterm --raw --eol LF

# Assembler options
# -nowarn=2028: don't warn when loading a constant >= 128 with moveq
ASFLAGS = -esc -DF_CPU=$(F_CPU) -DBAUD=$(BAUD) -m68000 -nowarn=2028
ASFLAGS_SREC = -Fsrec
ASFLAGS_BIN  = -Fbin

#-------------------------------------------------------------------------------
TESTPROG_SRC = $(TESTPROG).asm
TESTPROG_BIN = $(TESTPROG).bin
TESTPROG_LST = $(TESTPROG).lst

.PHONY: all clean rom flashrom

all: $(TESTPROG_BIN)

$(TESTPROG_BIN): $(TESTPROG_SRC)
	$(AS) $(ASFLAGS) -o $@ $(ASFLAGS_BIN) -L $(TESTPROG_LST) $<

load: $(TESTPROG_BIN)
	./serload.py $< $(FTDI_DEV)

run: load
	$(SERIALTERM) $(FTDI_DEV) $(BAUD)

rom: $(ROM_BIN)

serial:
	$(SERIALTERM) $(FTDI_DEV) $(BAUD)

$(ROM_SREC): $(ROM_SRC)
	$(AS) $(ASFLAGS) -o $@ $(ASFLAGS_SREC) -L $(ROM_LST) $<

# Even ROM (upper 8 bits, D15-D8)
$(ROM_BIN_U): $(ROM_SREC)
	srec_cat $< -split 2 0 -o $@ -binary

# Odd ROM (lower 8 bits, D7-D0)
$(ROM_BIN_L): $(ROM_SREC)
	srec_cat $< -split 2 1 -o $@ -binary

$(ROM_DASM): $(ROM_SREC)
	srec_cat $< -o $<.bin -binary
	$(DAS) $<.bin > $(ROM_DASM)

clean:
	rm -f *.bin *.srec *.lst

burnrom: rom
	@echo "Insert lower (odd) ROM and press Return:"
	@read
	@echo "Programming lower (odd) ROM"
	minipro -p AT28C256 -w $(ROM_BIN_L)
	@echo "Insert upper (even) ROM and press Return:"
	@read
	@echo "Programming upper (even) ROM"
	minipro -p AT28C256 -w $(ROM_BIN_U)
	@echo "Done"

