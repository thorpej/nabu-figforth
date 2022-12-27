HCCA_REGISTER     .EQU 80H
CONTROL_REGISTER  .EQU 00H

PSG_DATA          .EQU 40H
PSG_ADDRESS       .EQU 41H

PSG_REG_IO_A      .EQU 0eH
PSG_REG_IO_B      .EQU 0fH

INT_MASK_HCCARINT .EQU 80H
INT_MASK_HCCATINT .EQU 40H
INT_MASK_KEYBOARD .EQU 20H
INT_MASK_VDP      .EQU 10H

VDP_DATA          .EQU 0a0H
VDP_STATUS        .EQU 0a1H

KEYBOARD_DATA     .EQU 90H
KEYBOARD_STATUS   .EQU 91H

KEYBOARD_RXRDY    .EQU 02H

VDP_R0_EXTVID     .EQU 01H
VDP_R0_M3         .EQU 02H
VDP_R0_M4         .EQU 04H
VDP_R0_IE1        .EQU 10H

VDP_R1_MAG        .EQU 01H
VDP_R1_SIZE       .EQU 02H
VDP_R1_M2         .EQU 08H
VDP_R1_M1         .EQU 10H
VDP_R1_IE0        .EQU 20H
VDP_R1_BLANK      .EQU 40H
VDP_R1_16K        .EQU 80H

VDP_FONT_BASE     .EQU 0100H
VDP_FONT_SIZE     .EQU 768
VDP_PAGE_BASE     .EQU 1000H

VDP_TEXT_ROWS     .EQU 23

init_nabu:
        ld      a, 0
        call    set_interrupt_mask
        call    init_vdp
        call    init_interrupts
        ld      a, INT_MASK_KEYBOARD | INT_MASK_VDP
        call    set_interrupt_mask
        ret
