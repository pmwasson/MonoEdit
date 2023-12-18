;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------
; Edit functions
;
; invertPixels
; copyTile
; pastTile

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
    jsr     clearPixel  ; white -> black
    jmp     cont
:
    cmp     #PIXEL_BLACK
    bne     cont
    jsr     setPixel    ; black -> white
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
; copyTile
;-----------------------------------------------------------------------------
.proc copyTile

    lda     tileIndex
    jsr     setTilePointer

    ldy     #0
:
    lda     (bgPtr0),y
    sta     clipboardData,y

    iny
    cpy     tileLength
    bne     :-
    rts
.endproc

;-----------------------------------------------------------------------------
; pasteTile
;-----------------------------------------------------------------------------
.proc pasteTile

    lda     tileIndex
    jsr     setTilePointer

    ldy     #0
:
    lda     clipboardData,y
    sta     (bgPtr0),y

    iny
    cpy     tileLength
    bne     :-
    rts
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
;  Rotate all pixels based on tile size
;-----------------------------------------------------------------------------
; Could be done much faster by using bytes, but was too lazy
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
