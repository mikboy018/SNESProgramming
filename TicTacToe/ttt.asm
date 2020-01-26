; SNES Initialization Tutorial, Adapted into a simple 1 controller platformer

; Tic Tac Toe Demo, from https://wiki.superfamicom.org/making-a-small-game-tic-tac-toe

; 1) This will load a palette and some tiles
; 2) set up two backgrounds, one scrolled
; 3) finish set up
; 4) main loop converts data from ram to snes registers 
; 5) in VBlank, get joypad input and execute accordingly


.include "Header.inc"
.include "Snes_Init.asm"

.macro ConvertX
	; Data in: our coord in A
	; Data out: SNES scroll data in C (the 16 bit A)
	.rept 5
		asl a           ; multiply A by 32
	.endr

	rep #%00100000  ; 16 bit A
	eor #$FFFF      ; this will do A=1-A
	inc a           ; A=A+1
	sep #%00100000  ; 8 bit A
.endm

.macro ConvertY
	; Data in: our coord in A
	; Data out: SNES scroll data in C (the 16 bit A)
	.rept 5
		asl a           ; multiply A by 32
	.endr

	rep #%00100000  ; 16 bit A
	eor #$FFFF      ; this will do A=1-A
	sep #%00100000  ; 8 bit A
.endm

.bank 0 slot 0
.org 0
.section "Vblank"
;-----------------------
VBlank:
lda $4212       ; get joypad status
and #%00000001  ; if joy is not ready
bne VBlank      ; wait
lda $4219       ; read joypad (BYSTudlr)
sta $0201       ; store it
cmp $0200       ; compare it with the previous
bne +           ; if not equal, go
rti	            ; if it's equal, then return

+ sta $0200     ; store
and #%00010000  ; get the start button
                ; this will be the delete key
beq +           ; if it's 0, we don't have to delete
ldx #$0000
- stz $0000,x   ; delete addresses $0000 to $0008
inx
cpx #$09        ; this is 9. Guess why (homework :) )
bne -
stz $0100       ; delete the scroll
stz $0101       ; data also

+ lda $0201     ; get back the temp value
and #%11000000  ; Care only about B and Y
beq +           ; if empty, skip this
; so, B or Y is pressed. Let's say B is O,
; and Y is X.
cmp #%11000000  ; both are pressed?
beq +		    ; then don't do anything
cmp #%10000000  ; B?
bne ++          ; no, try Y
; B is pressed, write an O ($08)
; we have to tell the cursor position,
; and calculate an address from that
; Formula: Address=3*Y+X
lda $0101       ; get Y
sta $0202       ; put it to a temp value
clc
adc $0202       ; multiply by 3 - an easy way
adc $0202       ; A*3=A+A+A :)
adc $0100       ; add X
; Now A contains our address
ldx #$0000      ; be on the safe side
tax
lda #$08
sta $0000,x     ; put $08 to the good address
jmp +           ; done with this

++              ; now for Y
cmp #%01000000  ; Y?
bne +           ; no, jump forward (this should not happen)
; Y is pressed, write an X ($0A)
lda $0101       ; get Y
sta $0202       ; put it to a temp value
clc
adc $0202       ; multiply by 3 - an easy way
adc $0202       ; A*3=A+A+A :)
adc $0100       ; add X
; Now A contains our address
ldx #$0000      ; be on the safe side
tax
lda #$0A
sta $0000,x     ; put $0A to the good address
+               ; finished putting tiles

; cursor moving comes now
lda $0201       ; get control
and #%00001111  ; care about directions
sta $0201       ; store this

cmp #%00001000  ; up?
bne +           ; if not, skip
lda $0101       ; get scroll Y
cmp #$00        ; if on the top,
beq +           ; don't do anything
dec $0101       ; sub 1 from Y
+

lda $0201       ; get control
cmp #%00000100  ; down?
bne +           ; if not, skip
lda $0101
cmp #$02        ; if on the bottom,
beq +           ; don't do anything
inc $0101       ; add 1 to Y
+

lda $0201       ; get control
cmp #%00000010  ; left?
bne +           ; if not, skip
lda $0100
cmp #$00        ; if on the left,
beq +           ; don't do anything
dec $0100       ; sub 1 from X
+

lda $0201       ; get control
cmp #%00000001  ; right?
bne +           ; if not, skip
lda $0100
cmp #$02        ; if on the right,
beq +           ; don't do anything
inc $0100       ; add 1 to X
+
rti             ; F|NisH3D!
;-----------------------
.ends

.bank 0 slot 0
.org 0
.section "Main"
;----------------------
Start:
	InitSNES
; Load the palette
rep #%00010000 ; 16 bit x, y 
sep #%00100000 ; 8 bit ab

; take every byte from palette, put it into CGRAM
ldx #$0000
- lda UntitledPalette.l, x 
sta $2122
inx
cpx #8
bne -

; two palettes, one color needed for 2nd palette
lda #33
sta $2121
lda.l Palette2
sta $2122
lda.l Palette2+1
sta $2122

ldx #UntitledData ; Address
lda #:UntitledData ; of UntitledData
ldy #(15*16*2) ; length of data 
stx $4302 ; write
sta $4304 ; Address
sty $4305 ; and length
lda #%00000001 ; set mode - transfer words
sta $4300
lda #$18 ; $211[89] VRAM data write
sta $4301 ; set destination

ldy #$0000 ; write to VRAM from $0000
sty $2116

lda #%00000001 ; start dma channel 0
sta $420B

lda #%10000000 ; VRAM writing mode
sta $2115
ldx #$4000 ; write to VRAM
stx $2116 ; from $4000

; write the # shape - note could conver this to table -- todo later
.rept 2
	; X | X | X 
	.rept 2
		ldx #$0000 ; tile 0 ( )
		stx $2118
		ldx #$0002 ; tile 2 ( | )
		stx $2118
	.endr

	ldx #$0000
	stx $2118

	; first line finished, add BG's 
	.rept 27
		stx $2118 ; x = 0
	.endr

	; begin 2nd line -+-+-
	.rept 2
		ldx #$0004 ; tile 4 (-)
		stx $2118
		ldx #$0006 ; tile 6 (+)
		stx $2118
	.endr
	ldx #$0004 ; tile 4 again, to finish line 
	stx $2118

	ldx #$0000
	.rept 27
		stx $2118
	.endr
.endr

.rept 2
	ldx #$0000 ; tile 0 ( )
	stx $2118
	ldx #$0002 ; tile 2 ( | )
	stx $2118
.endr

; set up BG 2
ldx #$6000 ; bg2 starts here 
stx $2116 
ldx #$000C ; contains 1 tile 
stx $2118

; set up the screen
lda #%00110000 ; 16x16 tiles, mode 0
sta $2105 ; screen mode register 
lda #%01000000 ; data starts from $4000
sta $2107 ; for bg1
lda #%01100000 ; and $6000
sta $2108 ; for bg2

stz $210B ; bg1 and bg2 use $0000 tiles

lda #%00000011 ; enable bg1 and bg2
sta $212C

; PPU won't process top line, so we move 1 line down
rep #$20 ; 16bit A 
lda #$07FF ; this is -1 for bg1
sep #$20 ; 8 bit A 
sta $210E ; BG1 vert scroll
xba
sta $210E

rep #$20 ; 16bit A
lda #$FFFF ; this is -1 for BG2
sep #$20   ; 8bit a
sta $2110  ; BG2 vert scroll
xba
sta $2110

lda #%00001111 ; enable screen, set brightness to 15
sta $2100

lda #%10000001 ; enable NMI and joypads
sta $4200

forever:
wai

rep #%00100000  ; get 16 bit A
lda #$0000      ; empty it
sep #%00100000  ; 8 bit A
lda $0100       ; get our X coord
 ConvertX       ; WLA needs a space before a macro name
sta $210F       ; BG2 horz scroll
xba
sta $210F       ; write 16 bits

;now repeat it, but change $0100 to $0101, and $210F to $2110
rep #%00100000  ; get 16 bit A
lda #$0000      ; empty it
sep #%00100000  ; 8 bit A
lda $0101       ; get our Y coord
 ConvertY       ; WLA needs a space before a macro name
sta $2110       ; BG2 vert scroll
xba
sta $2110       ; write 16 bits

;write this after the conversion routine, just before jmp forever
ldx #$0000; reset our counter
-
rep #%00100000     ; 16 bit A
lda #$0000         ; empty it
sep #%00100000     ; 8 bit a
lda VRAMtable.l,x  ; this is a long indexed address, nice :)
rep #%00100000
clc
adc #$4000         ; add $4000 to the value
sta $2116          ; write to VRAM from here
lda #$0000         ; reset A while it's still 16 bit
sep #%00100000     ; 8 bit A
lda $0000,x        ; get the corresponding tile from RAM
; VRAM data write mode is still %10000000
sta $2118          ; write
stz $2119          ; this is the hi-byte
inx
cpx #9             ; finished?
bne -              ; no, go back

jmp forever
;----------------------
.ends


.bank 1 slot 0 ; loading tiles into bank 1
.org 0
.section "Tiledata"
.include "tiles.inc" ; replace if updating w/other tiles
.ends

; The XO must be copied to VRAM, first make conversion table, then add $4000 to it
;put this after everything
.bank 2 slot 0
.org 0
.section "Conversiontable"
VRAMtable:
.db $00,$02,$04,$40,$42,$44,$80,$82,$84
.ends



