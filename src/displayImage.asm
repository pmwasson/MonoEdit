;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------
; Display DHGR monochrome image
;-----------------------------------------------------------------------------

.include "defines.asm"
.include "macros.asm"
;-----------------------------------------------------------------------------

.segment "CODE"
.org    $6000

;-----------------------------------------------------------------------------

.proc main
	jsr 	init
    jsr     clearScreen

    lda     #0
    sta     imageNumber

    ;1,1
slideShow:

    lda     #1
    sta     imageX
    lda     #1
    sta     imageY
    lda     imageNumber
    jsr     drawImage

    lda     #1+13
    sta     imageX
    jsr     incImage
    jsr     drawImage

    lda     #1+13*2
    sta     imageX
    jsr     incImage
    jsr     drawImage

    lda     #1
    sta     imageX
    lda     #12
    sta     imageY
    jsr     incImage
    jsr     drawImage

    lda     #1+13
    sta     imageX
    jsr     incImage
    jsr     drawImage

    lda     #1+13*2
    sta     imageX
    jsr     incImage
    jsr     drawImage

    jsr     decImage
    jsr     decImage
    jsr     decImage
    jsr     decImage
    jsr     decImage

    jsr     RDKEY
    cmp     #KEY_LEFT
    bne     :+
    jsr     decImage
    jmp     slideShow

:
    cmp     #KEY_ESC
    beq     done
    inc     imageNumber
    lda     imageNumber
    cmp     imageCount
    bne     slideShow
    lda     #0
    sta     imageNumber
    jmp     slideShow

done:
    jmp 	monitor

incImage:
    inc     imageNumber
    lda     imageNumber
    cmp     imageCount
    bne     :+
    lda     #0
    sta     imageNumber
:
    rts
decImage:
    dec     imageNumber
    bpl     :+
    lda     imageCount
    sta     imageNumber
    dec     imageNumber
:
    rts

imageNumber:
    .byte   0
.endproc

;-----------------------------------------------------------------------------
; Draw Image
;
; 	Data is split between even and odd using ptrAA and ptrAB
;-----------------------------------------------------------------------------
.proc drawImage
    sta     CLR80COL        ; Use RAMWRT for aux mem

    asl
    asl     ; *4
    tax
    lda     imageTable,x
    sta     ptrAA0
    lda     imageTable+1,x
    sta     ptrAA1
    lda     imageTable+2,x
    sta     ptrAB0
    lda     imageTable+3,x
    sta     ptrAB1

    lda     imageY
    tax
    clc
    adc     imageHeight
    sta     imageEnd

loopY:
    lda     lineOffset,x
    clc
    adc 	imageX
    sta     screenPtr0
    lda     linePage,x
    sta     screenPtr1

    lda 	#8
    sta 	lineCount
loop8:

    ldy 	#0
loopX:
    sta     RAMWRTON  			; aux  
    lda		(ptrAA0),y
    sta 	(screenPtr0),y
    sta     RAMWRTOFF  			; main  
    lda		(ptrAB0),y
    sta 	(screenPtr0),y
    iny
    cpy 	imageWidth
    bne 	loopX

    ; increment pointers

    clc
    lda 	imageWidth
    adc 	ptrAA0
   	sta 	ptrAA0
   	lda 	#0
   	adc 	ptrAA1
   	sta 	ptrAA1

    clc
    lda 	imageWidth
    adc 	ptrAB0
   	sta 	ptrAB0
   	lda 	#0
   	adc 	ptrAB1
   	sta 	ptrAB1

    clc
   	lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dec 	lineCount
    bne 	loop8

    inx
    cpx 	imageEnd
    bne 	loopY

    rts

lineCount: 	.byte 	0
imageEnd:   .byte   0

.endproc

;-----------------------------------------------------------------------------
; Init
;-----------------------------------------------------------------------------
.proc init

    ; Set ctrl-y vector
    lda     #$4c        ; JMP
    sta     $3f8
    lda     #<quit
    sta     $3f9
    lda     #>quit
    sta     $3fa

    ; Set up text screen
    jsr     $c300       ; 80 column mode
    jsr     HOME        ; clear screen
    lda     #23         ; put cursor on last line
    sta     CV
    jsr     VTAB

    ; init DHGR (monochrome)

    sta     MIXCLR
    sta     HIRES
    sta     TXTCLR
    sta     LOWSCR
    ldx     #2
:
    sta     SET80COL
    sta     SET80VID 
    sta     CLR80VID
    sta     DHIRESON
    sta     DHIRESOFF
    sta     SET80VID 
    sta     DHIRESON
    dex
    bne     :-
    sta     MIXSET
    rts
.endproc

;-----------------------------------------------------------------------------
; Monitor
;
;  Exit to monitor
;-----------------------------------------------------------------------------
.proc monitor

    jsr    inline_print
    StringCR "Enter ctrl-y to quit to ProDos"

    jmp     MONZ        ; enter monitor

.endproc

;-----------------------------------------------------------------------------
; Quit
;
;   Exit to ProDos
;-----------------------------------------------------------------------------
.proc quit

    jsr     MLI
    .byte   CMD_QUIT
    .word   quit_params

quit_params:
    .byte   4               ; 4 parameters
    .byte   0               ; 0 is the only quit type
    .word   0               ; Reserved pointer for future use (what future?)
    .byte   0               ; Reserved byte for future use (what future?)
    .word   0               ; Reserved pointer for future use (what future?)

.endproc

;-----------------------------------------------------------------------------
; Clear screen
;-----------------------------------------------------------------------------

.proc clearScreen
    sta     CLR80COL        ; Use RAMWRT for aux mem

    lda     #$00
    sta     screenPtr0
    lda     #$20
    sta     screenPtr1

loop:
    ldy     #0
    lda     #0
    sta     RAMWRTON	; aux mem  
:
	sta     (screenPtr0),y
	iny
	bne     :-    
	sta     RAMWRTOFF	; main mem
:
    sta     (screenPtr0),y
    iny
    bne     :-    
 
 	inc     screenPtr1
    lda     #$40
    cmp     screenPtr1
    bne     loop
    rts
.endproc

;-----------------------------------------------------------------------------
; Globals
;-----------------------------------------------------------------------------

; All the follow use 2-byte increments for X / Width
;                and 8-byte increments for Y / Height
imageWidth: 	.byte 	10
imageHeight: 	.byte 	8 	
imageX: 		.byte 	1 		; [0..39-width]
imageY: 		.byte 	1 		; [0..23-height]

;-----------------------------------------------------------------------------
; Utilies
;-----------------------------------------------------------------------------

.include "inline_print.asm"

;-----------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------

.align      64
lineOffset:
    .byte   <$2000
    .byte   <$2080
    .byte   <$2100
    .byte   <$2180
    .byte   <$2200
    .byte   <$2280
    .byte   <$2300
    .byte   <$2380
    .byte   <$2028
    .byte   <$20A8
    .byte   <$2128
    .byte   <$21A8
    .byte   <$2228
    .byte   <$22A8
    .byte   <$2328
    .byte   <$23A8
    .byte   <$2050
    .byte   <$20D0
    .byte   <$2150
    .byte   <$21D0
    .byte   <$2250
    .byte   <$22D0
    .byte   <$2350
    .byte   <$23D0

linePage:
    .byte   >$2000
    .byte   >$2080
    .byte   >$2100
    .byte   >$2180
    .byte   >$2200
    .byte   >$2280
    .byte   >$2300
    .byte   >$2380
    .byte   >$2028
    .byte   >$20A8
    .byte   >$2128
    .byte   >$21A8
    .byte   >$2228
    .byte   >$22A8
    .byte   >$2328
    .byte   >$23A8
    .byte   >$2050
    .byte   >$20D0
    .byte   >$2150
    .byte   >$21D0
    .byte   >$2250
    .byte   >$22D0
    .byte   >$2350
    .byte   >$23D0

;-----------------------------------------------------------------------

; DHGR (20 bytes x 64 bytes) -> (140 pixels x 64 pixels)

imageCount:
    .byte   (imageTableEnd-imageTable)/4

imageTable:
    .word   goblinEven
    .word   goblinOdd
    .word   ogreEven
    .word   ogreOdd
    .word   warriorEven
    .word   warriorOdd
    .word   warrior2Even
    .word   warrior2Odd
    .word   warrior3Even
    .word   warrior3Odd
    .word   elf1Even
    .word   elf1Odd
    .word   girlEven
    .word   girlOdd
    .word   girl3Even
    .word   girl3Odd
    .word   heroEven
    .word   heroOdd
    .word   wizardEven
    .word   wizardOdd
    .word   robotEven
    .word   robotOdd
    .word   gypsy1Even
    .word   gypsy1Odd


imageTableEnd:

.include "images.asm"