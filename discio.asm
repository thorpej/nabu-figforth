;  CP/M DISC INTERFACE
;
;
;	FORTH variables & constants used in disc interface
;
	.BYTE	83H		;USE
	.TEXT	"US"
	.BYTE	'E'+$80
	.WORD	PTSTO-5
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
        jr      z, put_request

get_request:
        pop     hl
        pop     hl
        JNEXT

put_request:
        pop     hl
        pop     hl
        ld      h, 0
        ld      l, 0
        JHPUSH

;;; convert block number to offset, HL => block-number, returns offset in HL
block_number_to_offset:
        sla     l
        sla     l
        ld      h, l
        ld      l, 0
        ret
;
        .byte   83H                                         ;URL
        .text   "UR"
        .byte   'L'+$80
        .WORD   RSLW-6
URL:    .WORD	$+2
        JNEXT

storage_http_get_request:
        .byte   0a3h
        .byte   0                                           ;index
        .byte   storage_http_get_request_length-3           ;length
        .text   "https://vaxbusters.org/nabu-forth-startup.fth"
storage_http_get_request_length:        .equ    $-storage_http_get_request

;;; 
	.BYTE	85H		;FLUSH
	.TEXT	"FLUS"
	.BYTE	'H'+$80
	.WORD	URL-6
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
