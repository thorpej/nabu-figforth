;  NABU CONSOLE & PRINTER INTERFACE
;
; Last update:
;
; 221226 - ported to NABU
; 850511 - Saved BC' prior to CP/M calls
; 841010 - Saved IX & IY prior to CP/M calls
; 840909 - Converted all BIOS calls to BDOS calls for compatibility
;          with CP/M 3.0
;
;
RUBOUT	.EQU	7FH
;
EPRINT:	.BYTE	0		;printer flag
				;0=disabled, 1=enabled

;;; set VDP register, A => value, L => register
vdp_set_register:
        out     (VDP_STATUS), a
        ld      a, l
        or      80h
        out     (VDP_STATUS), a
        ret

;;; set VRAM write address, HL => address
vram_set_write_address:
        ld      a, l
        out     (VDP_STATUS), a
        ld      a, h
        and     3fh
        or      40h
        out     (VDP_STATUS), a
        ret

;;; set VRAM read address, HL => address
vram_set_read_address:
        ld      a, l
        out     (VDP_STATUS), a
        ld      a, h
        and     3fh
        out     (VDP_STATUS), a
        ret

;;; clear VRAM
clear_vram:
        ld      hl, 0
        call    vram_set_write_address
        ld      hl, 4000h
        xor     a
clear_vram_loop:
        out     (VDP_DATA), a
        dec     l
        jr      nz, clear_vram_loop
        dec     h
        jr      nz, clear_vram_loop
        ret

;;; load font, HL => font pointer (starts with ' ')
load_font:
        push    hl
        ld      hl, VDP_FONT_BASE
        call    vram_set_write_address
        pop     hl
        ld      b, 0
        ld      c, VDP_DATA
        otir
        otir
        otir
        ret

;;; initialize the console
init_vdp:
        di
        push    af
        push    bc
        push    de
        push    hl

        call    clear_vram
        ld      hl, font_data
        call    load_font

        ld      l, 0
        ld      a, 0
        call    vdp_set_register

        ld      l, 1
        ld      a, VDP_R1_16K | VDP_R1_IE0 | VDP_R1_BLANK | VDP_R1_M1 | VDP_R1_SIZE
        call    vdp_set_register

        ld      l, 2
        ld      a, 04H                                      ; name table at 1000H
        call    vdp_set_register

        ld      l, 4
        ld      a, 00H                                      ; pattern table at 0000H
        call    vdp_set_register

        ld      l, 7
        ld      a, 0F0H                                     ; colors: white on black
        call    vdp_set_register

        call    vdp_set_cursor_address

        pop     hl
        pop     de
        pop     bc
        pop     af
        ret

scroll_up:
        push    bc
        push    de
        ld      c, VDP_DATA
        ld      a, (scroll_first)
        ld      d, a
        ld      a, (scroll_last)
        ld      e, a
        inc     e
scroll_up_loop:
        ld      a, d
        inc     a
        call    get_row_address
        call    vram_set_read_address
        ld      hl, scroll_buf
        ld      a, (width)
        ld      b, a
        inir
        ld      a, d
        call    get_row_address
        call    vram_set_write_address
        ld      hl, scroll_buf
        ld      a, (width)
        ld      b, a
        otir
        inc     d
        ld      a, d
        cp      e
        jr      nz, scroll_up_loop
        dec     a
        call    get_row_address
        call    vram_set_write_address
        ld      a, (width)
        ld      b, a
        ld      a, ' '
clear_bottom_loop:
        out     (c), a
        dec     b
        jr      nz, clear_bottom_loop
        pop     de
        pop     bc
        ret


;;; debugging
delay:
        push    hl
        ld      hl, 8000h
delay_loop:
        dec     l
        jr      nz, delay_loop
        dec     h
        jr      nz, delay_loop
        pop     hl
        ret

;;; Get row address in VRAM, A=>row number, returns VRAM address in HL
get_row_address:
        push    bc
        sla     a
        ld      b, row_addresses/256
        ld      c, a
        ld      a, (bc)
        ld      l, a
        inc     c
        ld      a, (bc)
        ld      h, a
        pop     bc
        ret

;;; Set VRAM write address to cursor position
vdp_set_cursor_address:
        ld      a, (row)
        call    get_row_address
        ld      a, (col)
        add     a, l
        ld      l, a
        jr      nc, set_address
        inc     h
set_address:
        call    vram_set_write_address
        ret

COUT:
CPOUT:
        ld      a, e
        and     0e0h                                        ; mask off bits 7-5
        jr      z, control_character                        ; if zero -> control character
        and     080h                                        ; check if bit 7 is set
        jr      z, printable_character                      ; if not set, printable
        ret                                                 ; 8 bit characters not printed for now
control_character:
        ld      a, e
        cp      ACR
        jr      nz, check_lf
        ld      a, 0
        ld      (col), a
        ret
check_lf:
        cp      LF
        jr      nz, check_bs
        ld      a, (row)
        cp      VDP_TEXT_ROWS-1
        jr      nz, next_line
        call    scroll_up
        ret
next_line:
        inc     a
        ld      (row), a
        ret
check_bs:
        cp      BSOUT
        jr      z, bs
        ret
bs:
        ld      a, (col)
        cp      0
        jr      nz, do_bs
        ret
do_bs:
        dec     a
        ld      (col), a
        ret
printable_character:
        call    vdp_set_cursor_address
        ld      a, e
        out     (VDP_DATA), a
        ld      a, (col)
        inc     a
        ld      hl, width
        cp      (hl)
        jr      nz, same_line
        ld      hl, row
        ld      a, (scroll_last)
        cp      (hl)
        jr      z, wraparound_scroll
        inc     (hl)
        xor     a
        jr      same_line
wraparound_scroll:
        call    scroll_up
        xor     a
same_line:
        ld      (col), a
        ret
;
;	FORTH to NABU PC console interface
;
PQTER:	LD	HL,0
        ld      a, (last_char)
        cp      0
	JR	Z,PQTE1		;NO
	INC	L		;YES, (S1)<--TRUE
PQTE1:	JHPUSH
;
        ;; Ctrl-P toggles the printer on and off
PKEY:   ld      a, (last_char)
        cp      0
	JR	Z,PKEY		;NO
	LD	E,A
        xor     a
        ld      (last_char), a
        ld      a, e
	CP	DLE		;^P?
	JR	NZ,PKEY1	;NO
	LD	HL,EPRINT
	LD	E,ABL		;(E)<--BLANK
	LD	A,(HL)
	XOR	01H		;TOGGLE (EPRINT) LSB
	LD	(HL),A
PKEY1:	LD	L,E
	LD	H,0
	JHPUSH			;(S1)LB<--CHR
;
PEMIT:	.WORD	$+2		;(EMIT) orphan
	POP	DE		;(E)<--(S1)LB = CHR
	LD	A,E
	CP	BSOUT
	JR	NZ,PEMIT1
	CALL	COUT		;backspace
	LD	E,ABL		;blank
	CALL	COUT		;erase CHR on CON:
	LD	E,BSOUT		;backspace
PEMIT1:	CALL	CPOUT		;send CHR to CON:
				;and LST: if (EPRINT)=01H
	JNEXT
;
PCR:	LD	E,ACR
	CALL	CPOUT		;output CR
	LD	E,LF
	CALL	CPOUT		;and LF
	JNEXT
;
        .ORG ($ + 0FFH) & 0FF00H                            ; .align  256
row_addresses:
        .dw     VDP_PAGE_BASE
        .dw     VDP_PAGE_BASE+40
        .dw     VDP_PAGE_BASE+80
        .dw     VDP_PAGE_BASE+120
        .dw     VDP_PAGE_BASE+160
        .dw     VDP_PAGE_BASE+200
        .dw     VDP_PAGE_BASE+240
        .dw     VDP_PAGE_BASE+280
        .dw     VDP_PAGE_BASE+320
        .dw     VDP_PAGE_BASE+360
        .dw     VDP_PAGE_BASE+400
        .dw     VDP_PAGE_BASE+440
        .dw     VDP_PAGE_BASE+480
        .dw     VDP_PAGE_BASE+520
        .dw     VDP_PAGE_BASE+560
        .dw     VDP_PAGE_BASE+600
        .dw     VDP_PAGE_BASE+640
        .dw     VDP_PAGE_BASE+680
        .dw     VDP_PAGE_BASE+720
        .dw     VDP_PAGE_BASE+760
        .dw     VDP_PAGE_BASE+800
        .dw     VDP_PAGE_BASE+840
        .dw     VDP_PAGE_BASE+880
        .dw     VDP_PAGE_BASE+1020
row:    .db     0
col:    .db     0
width:  .db     40
scroll_first:
        .db     0
scroll_last:
        .db     VDP_TEXT_ROWS-1
scroll_buf:
        .dw     80

#include "font.inc"
