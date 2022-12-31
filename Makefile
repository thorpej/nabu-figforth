
all: figforth.nabu

figforth.nabu: conprtio.asm discio.asm figforth.asm nabu.asm irq.asm \
	nhacp.asm nabu-comms.asm
	uz80as -t z80 figforth.asm figforth.nabu figforth.lst

nabu-comms.asm: nabu-comms.md defmaker/index.js
	node defmaker/index.js nabu-comms.md

clean:
	rm -f *.lst *.img *.com *.nabu
