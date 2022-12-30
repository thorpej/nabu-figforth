
all: figforth.nabu

figforth.nabu: conprtio.asm discio.asm figforth.asm nabu.asm irq.asm nhacp.asm
	uz80as -t z80 figforth.asm figforth.nabu figforth.lst

clean:
	rm -f *.lst *.img *.com *.nabu
