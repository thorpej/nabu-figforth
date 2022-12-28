;;; NABU interrupt handling example.

;;; The NABU PC uses interrupt mode 2.  It has eight interrupt
;;; sources, which are numbered from 0 to 7.  Four sources are used by
;;; the on-board peripherals, and four are available to extension
;;; cards.
init_interrupts:
        ld      a, irq_table/256
        ld      i, a
        im      2
        ei
        ret

;;; Set current interrupt mask, A=>mask
set_interrupt_mask:
        di
        push    af
        ld      a, PSG_REG_IO_A
        out     (PSG_ADDRESS), a
        pop     af
        out     (PSG_DATA), a
        ei
        ret

;;; Get current interrupt mask, returns mask in A
get_interrupt_mask:
        di
        ld      a, PSG_REG_IO_A
        out     (PSG_ADDRESS), a
        in      a, (PSG_DATA)
        ei
        ret

;;; HCCA receiver - Receives a fixed-length message.  To start
;;; receiving, hcca_receive_pointer needs to point to the start of the
;;; receive buffer, hcca_receive_count needs to contain the number of
;;; bytes to read and the HCCA receive interrupt needs to be enabled.
;;; When a message has been read, the HCCA receive interrupt is
;;; disabled and needs to be re-enabled to receive the next message.
hccar_irq:
        push    af
        push    hl
        ld      hl, (hcca_receive_pointer)
        in      a, (HCCA_REGISTER)
        ld      (hl), a
        inc     hl
        ld      (hcca_receive_pointer), hl
        ld      hl, (hcca_receive_count)
        dec     hl
        ld      a, h
        or      l
        jr      z, end_of_receive
        ld      (hcca_receive_count), hl
        jr      z, end_of_receive
        jr      count_receive
end_of_receive:
        ;; end of message, switch off the HCCA RX interrupt
        ld      a, PSG_REG_IO_A
        out     (PSG_ADDRESS), a
        in      a, (PSG_DATA)
        and     ~INT_MASK_HCCARINT
        push    af
        ld      a, PSG_REG_IO_A
        out     (PSG_ADDRESS), a
        pop     af
        out     (PSG_DATA), a
        ld      a, 0
        ld      (hcca_receive_busy), a
count_receive:
        ld      hl, hccar_count
        jp      increment_counter

;;; HCCA transmitter.  To transmit, the hcca_transmit_pointer needs to
;;; point to the start of the message, hcca_transmit_count needs to
;;; contain the number of bytes to transmit and the HCCA transmit
;;; interrupt needs to be enabled.  Once the message has been sent,
;;; the HCCA transmit interrupt will be disabled and needs to be
;;; re-enabled to transmit the next message.
hccat_irq:
        push    af
        push    hl
        ld      hl, (hcca_transmit_pointer)
        ld      a, (hl)
        out     (HCCA_REGISTER), a
        inc     hl
        ld      (hcca_transmit_pointer), hl
        ld      hl, (hcca_transmit_count)
        dec     hl
        ld      a, h
        or      l
        jr      z, end_of_transmit
        ld      (hcca_transmit_count), hl
        jr      z, end_of_transmit
        jr      count_transmit
end_of_transmit:
        ;; end of message, switch off the HCCA TX interrupt
        ld      a, PSG_REG_IO_A
        out     (PSG_ADDRESS), a
        in      a, (PSG_DATA)
        and     ~INT_MASK_HCCATINT
        push    af
        ld      a, PSG_REG_IO_A
        out     (PSG_ADDRESS), a
        pop     af
        out     (PSG_DATA), a
        ld      a, 0
        ld      (hcca_transmit_busy), a
count_transmit:
        ld      hl, hccat_count
        jp      increment_counter

;;; Keyboard handler.  When a key is pressed, it is put into the
;;; _last_char variable which can be read from the user program.
keyb_irq:
        push    af
        push    hl
        in      a, (KEYBOARD_DATA)
        bit     7, a
        jr      nz, ignore_char
        ld      (last_char), a
ignore_char:
        ld      hl, keyb_count
        jp      increment_counter

;;; VDP interrupt handler.  The VDP issues an interrupt at the end (?)
;;; of each scan line.
vdp_irq:
        push    af
        push    hl
        in      a, (VDP_STATUS)
        ld      hl, vdp_count
        jp      increment_counter

option0_irq:
        push    af
        push    hl
        ld      hl, option0_count
        jp      increment_counter

option1_irq:
        push    af
        push    hl
        ld      hl, option1_count
        jp      increment_counter

option2_irq:
        push    af
        push    hl
        ld      hl, option2_count
        jp      increment_counter

option3_irq:
        push    af
        push    hl
        ld      hl, option3_count
        jp      increment_counter

increment_counter:
        inc     (hl)
        jr      nz, skip
        inc     l
        jr      nz, skip_h
        inc     h
skip_h:
        inc     (hl)
skip:
        pop     hl
        pop     af
        ei
        reti

;;; The irq_table holds the interrupt vectors for the Z80 interrupt
;;; mode 2.
        .ORG ($ + 0FFH) & 0FF00H                            ; .align  256
irq_table:
        .dw      hccar_irq
        .dw      hccat_irq
        .dw      keyb_irq
        .dw      vdp_irq
        .dw      option0_irq
        .dw      option1_irq
        .dw      option2_irq
        .dw      option3_irq

;;; Each interrupt is counted and can be read from the user program
irq_counters:
hccar_count:
        .dw     0
hccat_count:
        .dw     0
keyb_count:
        .dw     0
vdp_count:
        .dw     0
option0_count:
        .dw     0
option1_count:
        .dw     0
option2_count:
        .dw     0
option3_count:
        .dw     0

;;; Keyboard and HCCA variables
last_char:
        .dw     0

hcca_receive_pointer:
        .dw     0
hcca_receive_count:
        .dw     0
hcca_receive_busy:
        .byte   0

hcca_transmit_pointer:
        .dw     0
hcca_transmit_count:
        .dw     0
hcca_transmit_busy:
        .byte   0
