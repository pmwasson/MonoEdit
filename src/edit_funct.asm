;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------
; Edit functions
;   Input/Output:
;       - initMonochrome*
;       - initColor*
;       - clearScreen*
;       - printAll
;       - printDump
;       - drawString
;       - drawNumber
;       - drawBox
;   Modify:
;       - fillPixels
;       - invertPixels
;       - flipVert
;       - flipHor
;       - rotateUp
;       - rotateDown
;       - rotateLeft
;       - rotateRight
;-----------------------------------------------------------------------------

.include "common_funct.asm"

;-----------------------------------------------------------------------------
; Constants
;-----------------------------------------------------------------------------

PIXEL_BLACK     = $20           ; (0) Space
PIXEL_WHITE     = $0e           ; (1) 
PIXEL_BG_EVEN   = $16           ; (2)
PIXEL_BG_ODD    = $17

;-----------------------------------------------------------------------------
; printAll
;-----------------------------------------------------------------------------
.proc printAll

    lda     currentTile
    sta     temp
    lda     #0
    sta     currentTile
:
    jsr     printDump
    lda     currentTile
    clc
    adc     tileInc
    sta     currentTile
    cmp     tileMax
    bne     :-

    lda     temp
    sta     currentTile
    rts

temp:   .byte   0
 .endproc   

;-----------------------------------------------------------------------------
; Draw String
;
;   Use tileX and tileY for start and string inlined
;-----------------------------------------------------------------------------

.proc drawString

    ; Pop return address to find string
    pla
    sta     stringPtr0
    pla
    sta     stringPtr1
    ldy     #0

    lda     tileX
    sta     offset

    ; Print characters until 0 (end-of-string)
printLoop:
    iny
    bne     :+              ; Allow strings > 255
    inc     stringPtr1
:
    tya
    pha
    lda     (stringPtr0),y
    beq     printExit
    cmp     #13
    bne     :+
    inc     tileY
    lda     offset
    sta     tileX
    jmp     continue
:
    and     #$7f
    jsr     DHGR_DRAW_7X8

    inc     tileX
continue:
    pla
    tay
    jmp     printLoop

printExit:    
    pla                 ; clean up stack
    ; calculate return address after print string
    clc
    tya
    adc     stringPtr0  ; add low-byte first
    tax                 ; save in X
    lda     stringPtr1  ; carry to high-byte
    adc     #0          
    pha                 ; push return high-byte
    txa
    pha                 ; push return low-byte
    rts                 ; return

char:   .byte   0
offset: .byte   0
.endproc

;-----------------------------------------------------------------------------
; Draw Number
;
;   Draw 2 digit hex number passed in A
;-----------------------------------------------------------------------------
.proc drawNumber
    sta     temp
    lsr
    lsr
    lsr
    lsr     ; /16
    tax
    lda     numberLookup,x
    jsr     DHGR_DRAW_7X8
    inc     tileX
    lda     temp
    and     #$F
    tax
    lda     numberLookup,x
    jsr     DHGR_DRAW_7X8
    inc     tileX
    rts

temp:       .byte   0

.align 16
numberLookup:   .byte   '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
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
; Erase box
;-----------------------------------------------------------------------------

.proc eraseBox

    ; Draw corners
    lda     boxLeft
    sta     tileX
    lda     boxTop
    sta     tileY
    lda     #BOX_BLANK
    jsr     DHGR_DRAW_7X8

    lda     boxRight
    sta     tileX    
    lda     #$20
    jsr     DHGR_DRAW_7X8

    lda     boxBottom
    sta     tileY
    lda     #BOX_BLANK
    jsr     DHGR_DRAW_7X8

    lda     boxLeft
    sta     tileX
    lda     #BOX_BLANK
    jsr     DHGR_DRAW_7X8

    ; Draw horizontal

    inc     tileX
:
    lda     boxTop
    sta     tileY
    lda     #BOX_BLANK
    jsr     DHGR_DRAW_7X8
    
    lda     boxBottom
    sta     tileY
    lda     #BOX_BLANK
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
    lda     #BOX_BLANK
    jsr     DHGR_DRAW_7X8
    
    lda     boxRight
    sta     tileX
    lda     #BOX_BLANK
    jsr     DHGR_DRAW_7X8
    
    inc     tileY
    lda     boxBottom
    cmp     tileY
    bne     :-

    rts

.endproc


;-----------------------------------------------------------------------------
; Fill pixels
;   Set all pixel to A (0=black, 1=white, 2=background)
;-----------------------------------------------------------------------------

.proc fillPixels

    tax
    lda     colorList,x
    sta     color

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curX

loopX:
    lda     #0
    sta     curY

loopY:
    lda     color
    jsr     copyPixel

    inc     curY
    lda     curY
    cmp     tileHeight
    bne     loopY

    inc     curX
    lda     curX
    cmp     tileWidth
    bne     loopX

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

color:      .byte   0
colorList:  .byte   PIXEL_BLACK,PIXEL_WHITE,PIXEL_BG_EVEN
tempX:      .byte   0
tempY:      .byte   0

.endproc

;-----------------------------------------------------------------------------
; Invert pixels
;-----------------------------------------------------------------------------

.proc invertPixels

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curX

loopX:
    lda     #0
    sta     curY

loopY:

    jsr     getPixel
    cmp     #PIXEL_WHITE
    bne     :+
    lda     #PIXEL_BLACK
    jsr     copyPixel  ; white -> black
    jmp     cont
:
    cmp     #PIXEL_BLACK
    bne     cont
    lda     #PIXEL_WHITE
    jsr     copyPixel   ; black -> white
cont:

    inc     curY
    lda     curY
    cmp     tileHeight
    bne     loopY

    inc     curX
    lda     curX
    cmp     tileWidth
    bne     loopX

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

tempX:      .byte   0
tempY:      .byte   0

.endproc

;-----------------------------------------------------------------------------
; Flip vert
;-----------------------------------------------------------------------------
; Could be done much faster by using bytes, but was too lazy
.proc flipVert

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curX

loopX:
    lda     #0
    sta     indexY
loopY:

    ; swap pixels
    lda     indexY
    sta     curY
    jsr     getPixel
    sta     temp1

    lda     tileHeight
    clc     ; h-index-1
    sbc     indexY
    sta     curY
    jsr     getPixel
    sta     temp2
    lda     temp1
    jsr     copyPixel

    lda     indexY
    sta     curY
    lda     temp2
    jsr     copyPixel

    inc     indexY
    lda     indexY
    asl
    cmp     tileHeight
    bmi     loopY
    
    inc     curX
    lda     curX
    cmp     tileWidth
    bne     loopX

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

tempX:  .byte   0
tempY:  .byte   0
temp1:  .byte   0
temp2:  .byte   0
indexY: .byte   0

.endproc

;-----------------------------------------------------------------------------
; Flip hor
;-----------------------------------------------------------------------------
; Could be done much faster by using bytes, but was too lazy

.proc flipHor

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curY

loopY:
    lda     #0
    sta     indexX
loopX:

    ; swap pixels
    lda     indexX
    sta     curX
    jsr     getPixel
    sta     temp1

    lda     tileWidth
    clc
    sbc     indexX
    sta     curX
    jsr     getPixel
    sta     temp2
    lda     temp1
    jsr     copyPixel

    lda     indexX
    sta     curX
    lda     temp2
    jsr     copyPixel

    inc     indexX
    lda     indexX
    asl
    cmp     tileWidth
    bmi     loopX
    
    inc     curY
    lda     curY
    cmp     tileHeight
    bne     loopY

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

tempX:  .byte   0
tempY:  .byte   0
temp1:  .byte   0
temp2:  .byte   0
indexX: .byte   0

.endproc

;-----------------------------------------------------------------------------
; Rotate up 
;-----------------------------------------------------------------------------
.proc rotateUp

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curX

loopX:
    lda     #0
    sta     curY

    ; save first pixel
    jsr     getPixel
    sta     temp
    inc     curY

loopY:
    ; read pixel and write pixel above
    jsr     getPixel
    dec     curY
    jsr     copyPixel
    inc     curY
    inc     curY
    lda     curY
    cmp     tileHeight
    bne     loopY

    lda     temp
    dec     curY
    jsr     copyPixel
    
    inc     curX
    lda     curX
    cmp     tileWidth
    bne     loopX

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

tempX:  .byte   0
tempY:  .byte   0
temp:   .byte   0

.endproc

;-----------------------------------------------------------------------------
; Rotate down
;-----------------------------------------------------------------------------

.proc rotateDown

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curX

loopX:
    lda     tileHeight
    sta     curY
    dec     curY

    ; save first pixel
    jsr     getPixel
    sta     temp
    dec     curY

loopY:
    ; read pixel and write pixel above
    jsr     getPixel
    inc     curY
    jsr     copyPixel
    dec     curY
    dec     curY
    lda     curY
    bpl     loopY

    lda     temp
    inc     curY
    jsr     copyPixel
    
    inc     curX
    lda     curX
    cmp     tileWidth
    bne     loopX

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

tempX:  .byte   0
tempY:  .byte   0
temp:   .byte   0

.endproc

;-----------------------------------------------------------------------------
; Rotate Left 
;-----------------------------------------------------------------------------

.proc rotateLeft

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curY

loopY:
    lda     #0
    sta     curX

    ; save first pixel
    jsr     getPixel
    sta     temp
    inc     curX

loopX:
    ; read pixel and write pixel above
    jsr     getPixel
    dec     curX
    jsr     copyPixel
    inc     curX
    inc     curX
    lda     curX
    cmp     tileWidth
    bne     loopX

    lda     temp
    dec     curX
    jsr     copyPixel
    
    inc     curY
    lda     curY
    cmp     tileHeight
    bne     loopY

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

tempX:  .byte   0
tempY:  .byte   0
temp:   .byte   0

.endproc

;-----------------------------------------------------------------------------
; Rotate right 
;-----------------------------------------------------------------------------

.proc rotateRight

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curY

loopY:
    lda     tileWidth
    sta     curX
    dec     curX

    ; save first pixel
    jsr     getPixel
    sta     temp
    dec     curX

loopX:
    ; read pixel and write pixel above
    jsr     getPixel
    inc     curX
    jsr     copyPixel
    dec     curX
    dec     curX
    lda     curX
    bpl     loopX

    lda     temp
    inc     curX
    jsr     copyPixel
    
    inc     curY
    lda     curY
    cmp     tileHeight
    bne     loopY

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

tempX:  .byte   0
tempY:  .byte   0
temp:   .byte   0

.endproc

;-----------------------------------------------------------------------------
; Global data
;-----------------------------------------------------------------------------

; Box routine   
boxLeft:            .byte   0
boxRight:           .byte   0
boxTop:             .byte   0
boxBottom:          .byte   0

.align      256
clipboardData:      .res    8*16*2