;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------
; Isometric drawing routines

ISO_XOFFSET := 1
ISO_YOFFSET := 1

ISO_WIDTH := 4
ISO_HEIGHT := 2

;-----------------------------------------------------------------------------
; isoDrawMap
;
;   Draw ISO map
;-----------------------------------------------------------------------------
.proc isoDrawMap

    jsr     clearScreen

    sta     CLR80COL        ; Use RAMWRT for aux mem

    ldx     #0
    stx     isoIdx

    lda     #ISO_YOFFSET
    sta     tileY

loopY:
    lda     #ISO_XOFFSET
    sta     tileX

loopX1:
    ldx     isoIdx
    lda     isoMap0,x
    jsr     DHGR_DRAW_28X8
    ldx     isoIdx
    lda     isoMap1,x
    jsr     DHGR_DRAW_MASK_28X8
    inc     isoIdx

    clc
    lda     tileX
    adc     #2
    sta     tileX
    cmp     #ISO_XOFFSET+ISO_WIDTH*4
    bmi     loopX1

    inc     tileY
    lda     tileY
    cmp     #ISO_YOFFSET+ISO_HEIGHT*2+2
    bmi     loopY

    rts

isoIdx:    .byte   0
isoX:      .byte   0
isoY:      .byte   0

clearScreen:
    lda     #$00
    sta     screenPtr0
    lda     #$20
    sta     screenPtr1

    sta     CLR80COL        ; Use RAMWRT for aux mem

    lda     #$ff
    sta     pattern

loop:
    ldy     #0

    ; aux mem
    sta     RAMWRTON

    lda     #0
:
    lda     #$2a
    eor     pattern
    sta     (screenPtr0),y
    iny
    bne     :-

    sta     RAMWRTOFF

    ; main mem
:
    lda     #$55
    eor     pattern
    sta     (screenPtr0),y
    iny
    bne     :-

    inc     screenPtr1

    lda     screenPtr1
    and     #$3
    bne     :+
    lda     pattern
    eor     #$ff
    sta     pattern
:

    lda     #$40
    cmp     screenPtr1
    bne     loop
    rts

pattern:    .byte   0
.endproc

.align 256

isoMap0:   ; 5x5
    .byte       $02,  $02,  $02,  $02,  $02,  $02,  $02,  $02
    .byte       $02,  $02,  $1c,  $1e,  $1c,  $1e,  $1c,  $1e
    .byte       $02,  $1c,  $1e,  $1c,  $1e,  $1c,  $1e,  $24
    .byte       $1c,  $1e,  $1c,  $1e,  $1c,  $1e,  $1c,  $1e
    .byte       $20,  $02,  $02,  $22,  $20,  $22,  $20,  $22
    .byte       $02,  $02,  $02,  $02,  $02,  $02,  $02,  $02

isoMap1:   ; 5x5
    .byte       $00,  $00,  $18,  $1a,  $18,  $1a,  $18,  $1a
    .byte       $00,  $18,  $1a,  $18,  $1a,  $18,  $1a,  $00
    .byte       $18,  $1a,  $18,  $1a,  $18,  $1a,  $18,  $1a
    .byte       $00,  $28,  $2a,  $00,  $00,  $00,  $00,  $00
    .byte       $00,  $2c,  $2e,  $00,  $00,  $00,  $00,  $00
    .byte       $24,  $00,  $00,  $26,  $24,  $26,  $24,  $26
