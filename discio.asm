; NABU virtual disk interface using the NHACP protocol
;
;
;	FORTH variables & constants used in disc interface
;
	.BYTE	83H		;USE
	.TEXT	"US"
	.BYTE	'E'+$80
	.WORD	T80-6
USE:	.WORD	DOVAR,0		;/ initialised by CLD
;
	.BYTE	84H		;PREV
	.TEXT	"PRE"
	.BYTE	'V'+$80
	.WORD	USE-6
PREV:	.WORD	DOVAR,0		;/ initialised by CLD
;
	.BYTE	85H		;#BUFF
	.TEXT	"#BUF"
	.BYTE	'F'+$80
	.WORD	PREV-07H
NOBUF:	.WORD	DOCON,NBUF
;
	.BYTE	8AH		;DISK-ERROR
	.TEXT	"DISK-ERRO"
	.BYTE	'R'+$80
	.WORD	NOBUF-08H
DSKERR:	.WORD	DOVAR,0
;
;	DISC INTERFACE HIGH LEVEL ROUTINES
;
	.BYTE	84H		;+BUF
	.TEXT	"+BU"
	.BYTE	'F'+$80
	.WORD	DSKERR-0DH
PBUF:	.WORD	DOCOL
	.WORD	LIT,CO
	.WORD	PLUS,DUP
	.WORD	LIMIT,EQUAL
	.WORD	ZBRAN
	.WORD	PBUF1-$
	.WORD	DROP,FIRST
PBUF1:	.WORD	DUP,PREV
	.WORD	AT,SUBB
	.WORD	SEMIS
;
	.BYTE	86H		;UPDATE
	.TEXT	"UPDAT"
	.BYTE	'E'+$80
	.WORD	PBUF-07H
UPDAT:	.WORD	DOCOL,PREV
	.WORD	AT,AT
	.WORD	LIT,8000H
	.WORD	ORR
	.WORD	PREV,AT
	.WORD	STORE,SEMIS
;
	.BYTE	8DH		;EMPTY-BUFFERS
	.TEXT	"EMPTY-BUFFER"
	.BYTE	'S'+$80
	.WORD	UPDAT-9
MTBUF:	.WORD	DOCOL,FIRST
	.WORD	LIMIT,OVER
	.WORD	SUBB,ERASEE
	.WORD	SEMIS
;
	.BYTE	83H		;DR0
	.TEXT	"DR"
	.BYTE	'0'+$80
	.WORD	MTBUF-10H
DRZER:	.WORD	DOCOL
	.WORD	ZERO
	.WORD	OFSET,STORE
	.WORD	SEMIS
;
	.BYTE	83H		;DR1
	.TEXT	"DR"
	.BYTE	'1'+$80
	.WORD	DRZER-6
DRONE:	.WORD	DOCOL
	.WORD	LIT,1600	;Osborne DD
DRON2:	.WORD	OFSET,STORE
	.WORD	SEMIS
;
	.BYTE	86H		;BUFFER
	.TEXT	"BUFFE"
	.BYTE	'R'+$80
	.WORD	DRONE-6
BUFFE:	.WORD	DOCOL,USE
	.WORD	AT,DUP
	.WORD	TOR
BUFF1:	.WORD	PBUF		; won't work if single buffer
	.WORD	ZBRAN
	.WORD	BUFF1-$
	.WORD	USE,STORE
	.WORD	RR,AT
	.WORD	ZLESS
	.WORD	ZBRAN
	.WORD	BUFF2-$
	.WORD	RR,TWOP
	.WORD	RR,AT
	.WORD	LIT,7FFFH
	.WORD	ANDD,ZERO
	.WORD	RSLW
BUFF2:	.WORD	RR,STORE
	.WORD	RR,PREV
	.WORD	STORE,FROMR
	.WORD	TWOP,SEMIS
;
	.BYTE	85H		;BLOCK
	.TEXT	"BLOC"
	.BYTE	'K'+$80
	.WORD	BUFFE-9
BLOCK:	.WORD	DOCOL,OFSET
	.WORD	AT,PLUS
	.WORD	TOR,PREV
	.WORD	AT,DUP
	.WORD	AT,RR
	.WORD	SUBB
	.WORD	DUP,PLUS
	.WORD	ZBRAN
	.WORD	BLOC1-$
BLOC2:	.WORD	PBUF,ZEQU
	.WORD	ZBRAN
	.WORD	BLOC3-$
	.WORD	DROP,RR
	.WORD	BUFFE,DUP
	.WORD	RR,ONE
	.WORD	RSLW
	.WORD	TWOMIN		;/
BLOC3:	.WORD	DUP,AT
	.WORD	RR,SUBB
	.WORD	DUP,PLUS
	.WORD	ZEQU
	.WORD	ZBRAN
	.WORD	BLOC2-$
	.WORD	DUP,PREV
	.WORD	STORE
BLOC1:	.WORD	FROMR,DROP
	.WORD	TWOP,SEMIS
;
	.BYTE	83H		;R/W ( addr block# write -- ) ( 0 == write )
	.TEXT	"R/"
	.BYTE	'W'+$80
	.WORD	BLOCK-8
RSLW:	.WORD	$+2
DORSLW: pop     hl
        ld      a, l
        cp      0
        ;; save parameters
        pop     hl
        ld      (block_number), hl
        pop     hl
        ld      (block_address), hl
        jr      z, put_request

get_request:
        ;; set up storage-get request
        ld      hl, storage_get_req.index
        ld      (hl), 0                                     ; fixme: should use "current index"
        ld      hl, KBBUF
        ld      (storage_get_req.length), hl
        push    bc
        ld      bc, (block_number)
        ld      hl, storage_get_req.offset
        call    block_number_to_offset
        pop     bc
        ;; set up stack for nhacp_request
        ld      hl, storage_get_req
        push    hl
        ld      hl, storage_get_req_length
        push    hl
        ld      hl, (block_address)
        push    hl
        ld      hl, 0
        push    hl
        jp      nhacp_request

put_request:
        ;; set up storage-put request
        ld      hl, storage_put_req.index
        ld      (hl), 0                                     ; fixme: should use "current index"
        ld      hl, KBBUF
        ld      (storage_put_req.length), hl
        push    bc
        ld      bc, (block_number)
        ld      hl, storage_put_req.offset
        call    block_number_to_offset
        pop     bc
        ;; set up stack for nhacp_request
        ld      hl, storage_put_req
        push    hl
        ld      hl, storage_put_req_length
        push    hl
        ld      hl, (block_address)
        push    hl
        ld      hl, KBBUF
        push    hl
        jp      nhacp_request

block_number:
        .ds     2
block_address:
        .ds     2

;;; convert block number to offset, BC => block-number, HL => pointer
;;; to 32 bit offset
block_number_to_offset:
        push    hl
        ld      (hl), 0
        inc     hl
        ld      (hl), c
        inc     hl
        ld      (hl), b
        inc     hl
        ld      (hl), 0
        pop     hl
        inc     hl
        push    hl
        sla     (hl)
        inc     hl
        rl      (hl)
        inc     hl
        rl      (hl)
        pop     hl
        sla     (hl)
        inc     hl
        rl      (hl)
        inc     hl
        rl      (hl)
        ret
;;;
        .byte   82H                                         ;NR (nabu request)
        .text   "N"
        .byte   'R'+$80
        .word   RSLW-6
NR:     .word	$+2
        jp      nhacp_request
;;; 
        .byte   86H
        .text   "NROPE"
        .byte   'N'+$80
        .word   NR-5
NROPEN: .word   $+2
        pop     hl
        ld      a, l
        ld      (storage_open_req.url_length), a
        pop     hl
        ld      (adr_tmp), hl
        ld      hl, storage_open_req
        push    hl
        ld      hl, storage_open_req_length
        push    hl
        ld      hl, (adr_tmp)
        push    hl
        ld      a, (storage_open_req.url_length)
        ld      h, 0
        ld      l, a
        push    hl
        jp      nhacp_request
adr_tmp:
        .ds     2
;;;
        .byte   89H
        .text   "AUTOSTAR"
        .byte   'T'+$80
        .word   NROPEN-9
AUTOSTART:
        .word   DOCOL
        .word   LIT,default_url
        .word   LIT,default_url_length
        .word   OVER,OVER
        .word   LIT,opening_msg
        .word   LIT,opening_msg_len
        .word   TYPE
        .word   TYPE
        .word   NROPEN
        .word   CR
;        .word   LIT,1,LOAD
        .word   SEMIS

default_url:
;        .text   "https://github.com/hanshuebner/nabu-figforth/blob/main/AUTOSTART.FTH?raw=true"
;        .text   "file:test.txt?autoscreen=true"
        .text   "AUTOSTART.FTH"
default_url_length: .equ    $-default_url

opening_msg:
        .text   "Opening: "
opening_msg_len: .equ    $-opening_msg

nhacp_error_buf:
        .ds     128
;;;
        .byte   88h
        .text   ".ADAPTE"
        .byte   'R' + $80
	.word	AUTOSTART-12
ADAPTER:.word   DOCOL
        .word   LIT,adapter_heading
        .word   LIT,adapter_heading_length
        .word   TYPE
        .word   CR
        .word   LIT,nhacp_adapter_id
        .word   LIT,nhacp_started_res.adapter_id_le,CAT
        .word   TYPE
        .word   CR
        .word   SEMIS
adapter_heading:
        .text   "Connected to network adapter:"
adapter_heading_length: .equ   $-adapter_heading
;;;
        .BYTE   84H
        .TEXT   "TES"
        .BYTE   'T'+$80
        .WORD	ADAPTER-11
XTEST:  .WORD   DOCOL
        .WORD   LIT,get_date_time_req
        .WORD   LIT,1
        .WORD   LIT,$8000
        .WORD   LIT,0
        .WORD   NR
        .WORD   LIT,date_time_res+1
        .WORD   LIT,date_time_res_length-1
        .WORD   TYPE
        .WORD   SEMIS
;;; 
	.BYTE	85H		;FLUSH
	.TEXT	"FLUS"
	.BYTE	'H'+$80
	.WORD   XTEST-7
FLUSH:	.WORD	DOCOL
	.WORD	NOBUF,ONEP
	.WORD	ZERO,XDO
FLUS1:	.WORD	ZERO,BUFFE
	.WORD	DROP
	.WORD	XLOOP
	.WORD	FLUS1-$
	.WORD	SEMIS
;
	.BYTE	84H			;LOAD
	.TEXT	"LOA"
	.BYTE	'D'+$80
	.WORD	FLUSH-08h
LOAD:	.WORD	DOCOL,BLK
	.WORD	AT,TOR
	.WORD	INN,AT
	.WORD	TOR,ZERO
	.WORD	INN,STORE
	.WORD	BSCR,STAR
	.WORD	BLK,STORE		;BLK <-- SCR * B/SCR
	.WORD	INTER			;INTERPRET FROM OTHER SCREEN
	.WORD	FROMR,INN
	.WORD	STORE
	.WORD	FROMR,BLK
	.WORD	STORE,SEMIS
;
	.BYTE	0C3H			;-->
	.TEXT	"--"
	.BYTE	'>'+$80
	.WORD	LOAD-7
ARROW:	.WORD	DOCOL,QLOAD
	.WORD	ZERO
	.WORD	INN,STORE
	.WORD	BSCR,BLK
	.WORD	AT,OVER
	.WORD	MODD,SUBB
	.WORD	BLK,PSTOR
	.WORD	SEMIS
;
;
;
