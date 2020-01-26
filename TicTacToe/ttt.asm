; SNES Initialization Tutorial, Adapted into a simple 1 controller platformer

; Tic Tac Toe Demo, from https://wiki.superfamicom.org/making-a-small-game-tic-tac-toe

; 1) This will load a palette and some tiles
; 2) set up two backgrounds, one scrolled
; 3) finish set up
; 4) main loop converts data from ram to snes registers 
; 5) in VBlank, get joypad input and execute accordingly


.include "Header.inc"
.include "Snes_Init.asm"

; satisfies interrupt definition in "Header.inc"
VBlank:
  RTI

.bank 0
.section "MainCode"

Start:
  ; Initialize Console
  Snes_Init

  ; Set initial background color
  sep 	#$20 ;set A register to 8 bit, SEP - Set Flag, Immediate, Remember $20 is 32 in decimal (5th bit)
  lda 	#%10000000 ; Forces VBlank (turns off screen), LDA - Load into A
  sta 	$2100 ; store a at location $2100 // PPU Write-Only Port, INIDISP - Display Control 1
  lda 	#%11100000 ; load into accumulator, low byte of green color
  sta 	$2122 ; store a at location $2122 // CGDATA - Palette CGRAM Data Write (write-twice)
  lda  	#%00000000 ; load into accumulator, high byte of green color
  sta 	$2122 ; store a at location $2122 (high byte)
  lda 	#%00001111 ; End VBlank, set brightness to 15 (100%)
  sta 	$2100 ; store a at location $2100 ; INIDISP - turn on

  ; Main Loop
  Forever:
  	jmp Forever ; end main loop


.ends