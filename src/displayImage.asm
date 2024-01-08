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

BOX_HORZ        = $13
BOX_VERT        = $7c
BOX_UPPER_LEFT  = $1c
BOX_UPPER_RIGHT = $1d
BOX_LOWER_LEFT  = $1e
BOX_LOWER_RIGHT = $1f

;-----------------------------------------------------------------------------

.proc main
	jsr 	init
    jsr     clearScreen

    ; set param
    lda     #10
    sta     DHGR_IMAGE_WIDTH
    lda     #8
    sta     DHGR_IMAGE_HEIGHT
    lda     #1
    sta     DHGR_IMAGE_X
    sta     DHGR_IMAGE_Y
    lda     #0
    sta     imageNumber

    lda     #0
    sta     boxTop
    lda     #0+8+1
    sta     boxBottom
    lda     #1
    sta     boxLeft
    lda     #1+20+1
    sta     boxRight
    jsr     drawBox

    lda     #78-20-1
    sta     boxLeft
    lda     #78
    sta     boxRight
    jsr     drawBox

showTitle:
    ; show title on page2
    sta     HISCR
    ; Wait for keypress
    jsr     getKey
    sta     LOWSCR

    ;1,1
slideShow:

    lda     #1
    sta     DHGR_IMAGE_X
    lda     #1
    sta     DHGR_IMAGE_Y
    lda     imageNumber
    jsr     drawImage

    lda     #1+14
    sta     DHGR_IMAGE_X
    jsr     incImage
    jsr     drawImage

    lda     #1+14*2
    sta     DHGR_IMAGE_X
    jsr     incImage
    jsr     drawImage

    lda     #1
    sta     DHGR_IMAGE_X
    lda     #12
    sta     DHGR_IMAGE_Y
    jsr     incImage
    jsr     drawImage

    lda     #1+14
    sta     DHGR_IMAGE_X
    jsr     incImage
    jsr     drawImage

    lda     #1+14*2
    sta     DHGR_IMAGE_X
    jsr     incImage
    jsr     drawImage

    jsr     decImage
    jsr     decImage
    jsr     decImage
    jsr     decImage
    jsr     decImage

    jsr     getKey

    cmp     #KEY_SPACE
    bne     :+
    jmp     showTitle
:

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

getKey:
    lda     KBD
    bpl     getKey
    sta     KBDSTRB     ; clean up
    rts

imageNumber:
    .byte   0

.endproc

;-----------------------------------------------------------------------------
; Draw Image
;
; 	Data is split between even and odd using tilePtr and maskPtr
;-----------------------------------------------------------------------------
.proc drawImage
    sta     CLR80COL        ; Use RAMWRT for aux mem

    asl
    asl     ; *4
    tax
    lda     imageTable,x
    sta     tilePtr0
    lda     imageTable+1,x
    sta     tilePtr1
    lda     imageTable+2,x
    sta     maskPtr0
    lda     imageTable+3,x
    sta     maskPtr1

    jsr     DHGR_DRAW_IMAGE

    rts

.endproc


;-----------------------------------------------------------------------------
; Draw box
;-----------------------------------------------------------------------------

.proc drawBox

    ; Draw corners
    lda     boxLeft
    sta     tileX
    lda     boxTop
    sta     tileY
    lda     #BOX_UPPER_LEFT
    jsr     DHGR_DRAW_7X8

    lda     boxRight
    sta     tileX
    lda     #BOX_UPPER_RIGHT
    jsr     DHGR_DRAW_7X8

    lda     boxBottom
    sta     tileY
    lda     #BOX_LOWER_RIGHT
    jsr     DHGR_DRAW_7X8

    lda     boxLeft
    sta     tileX
    lda     #BOX_LOWER_LEFT
    jsr     DHGR_DRAW_7X8

    ; Draw horizontal

    inc     tileX
:
    lda     boxTop
    sta     tileY
    lda     #BOX_HORZ
    jsr     DHGR_DRAW_7X8

    lda     boxBottom
    sta     tileY
    lda     #BOX_HORZ
    jsr     DHGR_DRAW_7X8

    inc     tileX
    lda     boxRight
    cmp     tileX
    bne     :-

    ; Draw vertical

    lda     boxTop
    sta     tileY
    inc     tileY

:
    lda     boxLeft
    sta     tileX
    lda     #BOX_VERT
    jsr     DHGR_DRAW_7X8

    lda     boxRight
    sta     tileX
    lda     #BOX_VERT
    jsr     DHGR_DRAW_7X8

    inc     tileY
    lda     boxBottom
    cmp     tileY
    bne     :-

    rts

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
    ; // GS B&W
    lda     #$21
    sta     NEWVIDEO    ; B&W mode
    lda     CLOCKCTL    ; RMW to set border color
    and     #$f0
    ora     #$05        ; Gray
    sta     CLOCKCTL

    ; // E B&W
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
    sta     MIXCLR
    rts
.endproc

;-----------------------------------------------------------------------------
; Monitor
;
;  Exit to monitor
;-----------------------------------------------------------------------------
.proc monitor
    sta     TXTSET
    jsr     inline_print
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

boxLeft:        .byte   0
boxRight:       .byte   0
boxTop:         .byte   0
boxBottom:      .byte   0

;-----------------------------------------------------------------------------
; Utilies
;-----------------------------------------------------------------------------

.include "inline_print.asm"

;-----------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------

; DHGR (20 bytes x 64 bytes) -> (140 pixels x 64 pixels)

imageCount:
    .byte   (imageTableEnd-imageTable)/4

imageTable:

    .word   wizard1Even
    .word   wizard1Odd
    .word   wizard2Even
    .word   wizard2Odd
    .word   robotEven
    .word   robotOdd

    .word   gypsy1Even
    .word   gypsy1Odd
    .word   computerEven
    .word   computerOdd
    .word   ffEven
    .word   ffOdd

    .word   elf1Even
    .word   elf1Odd
    .word   girlEven
    .word   girlOdd
    .word   girl3Even
    .word   girl3Odd

    .word   heroEven
    .word   heroOdd
    .word   ogreEven
    .word   ogreOdd
    .word   warriorEven
    .word   warriorOdd

    .word   goblinEven
    .word   goblinOdd
    .word   warrior2Even
    .word   warrior2Odd
    .word   warrior3Even
    .word   warrior3Odd


imageTableEnd:

.include "images0.asm"
.include "images1.asm"
.include "images2.asm"
.include "images3.asm"
.include "images4.asm"
