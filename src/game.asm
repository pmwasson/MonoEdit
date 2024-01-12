;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------
; Map edit
;------------------------------------------------

;------------------------------------------------
.include "defines.asm"
.include "macros.asm"

;-----------------------------------------------------------------------------
; Constants
;-----------------------------------------------------------------------------

; map-width     map-height  map-length <= 256
; ---------     ----------  ----------
; 8*            32          256
; 10            24          240
; 12            20          240
; 14            18          252
; 16*           16          256
; 18            14          252
; 20            12          240
; * = power of 2

MAP_WIDTH       = 13
MAP_HEIGHT      = 19
MAP_LENGTH      = MAP_WIDTH*MAP_HEIGHT

BOX_HORZ        = $13
BOX_VERT        = $7c
BOX_UPPER_LEFT  = $1c
BOX_UPPER_RIGHT = $1d
BOX_LOWER_LEFT  = $1e
BOX_LOWER_RIGHT = $1f
BOX_LEFT_T      = $0f
BOX_RIGHT_T     = $11

;------------------------------------------------
.segment "CODE"
.org    $6000

;=============================================================================
; Main
;=============================================================================

.proc main

    jsr     initMonochrome

    ; Clear screen 0 and draw map
    lda     #$00
    jsr     drawTestScreen

    ; Clear screen 1 and draw map
    lda     #$20
    jsr     drawTestScreen

    ; Cycle through map for animation
    lda     #255
    sta     mapCursor

    lda     #0
    sta     update

displayLoop:
    lda     drawPage
    beq     display1

    ; we just finished drawing on page 2, so display it and now drawing on page 1
    sta     HISCR
    lda     #$00
    beq     :+

    ; we just finished drawing on page 1, so display it and now drawing on page 2
display1:
    sta     LOWSCR
    lda     #$20

:
    sta     drawPage


    ; if updated last time, need to draw again on this page
    lda     update
    beq     :+
    jsr     setCoordinate
    jsr     drawQuarter
:

    lda     #0
    sta     update

    ; check next tile
    inc     mapCursor

; Update Tiles

    ldx     mapCursor

    ; Map 4
    lda     isoMap4,x
    tay
    lda     animateMap,y
    beq     :+
    sta     isoMap4,x
    inc     update
:

    ; Map 2
    lda     isoMap2,x
    tay
    lda     animateMap,y
    beq     :+
    sta     isoMap2,x
    inc     update
:
    ; Map 1
    lda     isoMap1,x
    tay
    lda     animateMap,y
    beq     :+
    sta     isoMap1,x
    inc     update
:
    ; Map 0
    lda     isoMap0,x
    tay
    lda     animateMap,y
    beq     :+
    sta     isoMap0,x
    inc     update
:

    lda     update
    beq     :+
    jsr     setCoordinate
    jsr     drawQuarter
:

    ;   check for keypress
    lda     mapCursor
    bne     continue
    lda     KBD
    bpl     continue

exit:
    bit     KBDSTRB     ; clean up

    ; Make sure to display and draw on page 1 on exit
    lda     #$00
    sta     drawPage
    sta     LOWSCR
    sta     MIXSET  ; mixed screen
    rts

continue:
    jmp     displayLoop

update:         .byte   0
storeWidth:     .byte   0
storeOffsetX:   .byte   0


drawTestScreen:
    sta     drawPage

    ldx     #0
    jsr     setBackground
    jsr     DHGR_CLEAR_SCREEN
    jsr     drawFrame

    jsr     drawTitle

    lda     #0
    jsr     DHGR_DRAW_INDEX_IMAGE

    jsr     drawText

    ldx     bgColor
    jsr     setBackground
    jsr     isoDrawMap

    rts

drawQuarter:
    ldx     mapCursor
    lda     isoMap0,x
    jsr     DHGR_DRAW_BG_28X8
    ldx     mapCursor
    lda     isoMap1,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     mapCursor
    lda     isoMap2,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     mapCursor
    lda     isoMap3,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     mapCursor
    lda     isoMap4,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     mapCursor
    lda     isoMap5,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     mapCursor
    lda     isoMap6,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     mapCursor
    lda     isoMap7,x
    jsr     DHGR_DRAW_MASK_28X8
    rts

drawFrame:

    lda     #<$B000
    sta     DHGR_TILE_7X8
    lda     #>$B000
    sta     DHGR_TILE_7X8+1

    lda     #1
    sta     boxLeft
    lda     #1
    sta     boxTop
    lda     #22
    sta     boxRight
    lda     #10
    sta     boxBottom
    clc
    jsr     drawBox

    lda     #1
    sta     boxLeft
    lda     #10
    sta     boxTop
    lda     #22
    sta     boxRight
    lda     #21
    sta     boxBottom
    sec
    jsr     drawBox

    lda     #23
    sta     boxLeft
    lda     #1
    sta     boxTop
    lda     #76
    sta     boxRight
    lda     #21
    sta     boxBottom
    clc
    jsr     drawBox
    rts

drawTitle:
    lda     #2
    sta     tileX
    lda     #0
    sta     tileY
    jsr     DHGR_DRAW_STRING

    .byte   $93,$93,$93,$93,$93," Merlin-8 ",$93,$93,$93,$93,$93,0

    lda     #24
    sta     tileX
    lda     #0
    sta     tileY
    jsr     DHGR_DRAW_STRING
    .byte   $93,$93,$93,$93,$93,$93,$93,$93,$93,$93,$93,$93,$93
    .byte   " Somewhere in the forest "
    .byte   $93,$93,$93,$93,$93,$93,$93,$93,$93,$93,$93,$93,$93,$93
    .byte   0
    rts

drawText:

    ; Use alternate font
    lda     #>$B400
    sta     DHGR_TILE_7X8+1

    lda     #2
    sta     tileX
    lda     #11
    sta     tileY
    jsr     DHGR_DRAW_STRING
    ;             <-- 20 columns ---->
    StringBold   "H"
    StringCont    "ello!"
    StringCont   "You can call me"
    StringBold   "MERLIN"
    StringCont          "-8 as I come"
    StringCont   "from a long line of"
    StringCont   "wizards."
    StringCont   "Are you looking for"
    .byte        "the "
    StringBold        "APPLESOFT"
    String       " trail?"

    ; Use restore font
    lda     #>$B000
    sta     DHGR_TILE_7X8+1

    rts

.endproc

.proc initMonochrome
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
    sta     MIXCLR      ; Mixed
    rts
.endproc



;-----------------------------------------------------------------------------
; Draw box
;
;-----------------------------------------------------------------------------

.proc drawBox

    bcc     topCorners

    ;   draw Ts for tops

    ; Draw corners
    lda     boxLeft
    sta     tileX
    lda     boxTop
    sta     tileY
    lda     #BOX_LEFT_T
    jsr     DHGR_DRAW_7X8

    lda     boxRight
    sta     tileX
    lda     #BOX_RIGHT_T
    jsr     DHGR_DRAW_7X8
    jmp     cont

topCorners:
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

cont:
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
; isoDrawMap
;
;   Draw ISO map
;-----------------------------------------------------------------------------

.proc isoDrawMap

    sta     CLR80COL        ; Use RAMWRT for aux mem

    clc
    lda     displayOffsetX
    adc     displayWidth
    sta     rightEdge

    lda     displayOffsetY
    adc     displayHeight
    sta     bottomEdge

    ldx     #0
    stx     isoIdxY

    lda     displayOffsetY
    sta     tileY

loopY:
    lda     displayOffsetX
    sta     tileX

    lda     isoIdxY
    sta     isoIdxX

loopX1:
    ldx     isoIdxX
    lda     isoMap0,x
    jsr     DHGR_DRAW_BG_28X8
    ldx     isoIdxX
    lda     isoMap1,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     isoIdxX
    lda     isoMap2,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     isoIdxX
    lda     isoMap3,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     isoIdxX
    lda     isoMap4,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     isoIdxX
    lda     isoMap5,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     isoIdxX
    lda     isoMap6,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     isoIdxX
    lda     isoMap7,x
    jsr     DHGR_DRAW_MASK_28X8

    inc     isoIdxX

    clc
    lda     tileX
    adc     #2
    sta     tileX
    cmp     rightEdge
    bmi     loopX1

    clc
    lda     isoIdxY
    adc     #MAP_WIDTH
    sta     isoIdxY

    inc     tileY
    lda     tileY
    cmp     bottomEdge
    bmi     loopY

    rts

isoIdxX:    .byte    0
isoIdxY:    .byte    0
rightEdge:  .byte    0
bottomEdge: .byte    0
.endproc


;-----------------------------------------------------------------------------
; setCoordinate
;
;-----------------------------------------------------------------------------
.proc setCoordinate
    ; cursor = yyyyxxxx

    lda     displayOffsetY
    sta     tileY

    lda     mapCursor
divideLoop:
    sec
    sbc     #MAP_WIDTH
    bcc     :+
    inc     tileY
    bne     divideLoop
:
    clc
    adc     #MAP_WIDTH
    asl     ;remainder*2
    adc     displayOffsetX
    sta     tileX

    rts

.endproc

;-----------------------------------------------------------------------------
; set background
;   Pattern index passed in X
;-----------------------------------------------------------------------------

.proc setBackground
    lda     pattern,x
    sta     bgPattern00
    lda     pattern+1,x
    sta     bgPattern01
    lda     pattern+2,x
    sta     bgPattern10
    lda     pattern+3,x
    sta     bgPattern11
    rts

pattern:
    .byte   $00,$00,$00,$00     ; black
    .byte   $ff,$ff,$ff,$ff     ; white
    .byte   $55,$2a,$2a,$55     ; gray1
    .byte   $2a,$55,$55,$2a     ; gray2
    .byte   $7f,$7f,$00,$00     ; horizontal
    .byte   $63,$63,$63,$63     ; vertical
    .byte   $54,$15,$6a,$2b     ; pattern
    .byte   $2a,$57,$57,$2a     ; border
    .byte   $7f,$00,$00,$7f     ; checker

.endproc

;-----------------------------------------------------------------------------
; Global
;-----------------------------------------------------------------------------
displayOffsetX: .byte   12
displayOffsetY: .byte   2
displayWidth:   .byte   MAP_WIDTH*2
displayHeight:  .byte   MAP_HEIGHT

mapCursor:  .byte   0   ; Offset into map table
bgColor:    .byte   2*4

boxLeft:        .byte   0
boxRight:       .byte   0
boxTop:         .byte   0
boxBottom:      .byte   0

.align 256


; Lookup table for animation sequence
; Water:    $14 <-> $64
;           $15 <-> $65
;           $16 <-> $66
;           $17 <-> $67
;           $18 <-> $68
;           $19 <-> $69
;           $1a <-> $6a
;           $1b <-> $6b

animateMap:
    ;        x0  x1  x2  x3  x4  x5  x6  x7  x8  x9  xA  xB  xC  xD  xE  xF
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 0x
    .byte   $00,$00,$00,$00,$64,$65,$66,$67,$68,$69,$6a,$6b,$00,$00,$00,$00     ; 1x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 2x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 3x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 4x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 5x
    .byte   $00,$00,$00,$00,$14,$15,$16,$17,$18,$19,$1a,$1b,$00,$00,$00,$00     ; 6x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 7x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 8x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 9x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; Ax
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; Bx
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; Cx
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; Dx
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; Ex
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; Fx


.align 256
isoMap0:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$6A,$6B,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$6A,$6B,$6A,$6B,$00,$00,$00,$00,$00,$00,$00
.byte $00,$6A,$6B,$6A,$6B,$00,$00,$00,$00,$00,$00,$00,$00,$6A,$6B,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$6A,$6B,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$6A,$6B,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$6A,$6B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$6A
.byte $6B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$6A,$6B,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$6A,$6B,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$6A,$6B,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
isoMap1:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$10,$11
.byte $00,$00,$00,$00,$10,$11,$00,$00,$00,$00,$10,$11,$68,$69,$00,$00
.byte $00,$00,$10,$11,$00,$00,$10,$11,$68,$69,$68,$69,$10,$11,$00,$00
.byte $10,$11,$10,$11,$68,$69,$68,$69,$10,$11,$00,$00,$00,$00,$10,$11
.byte $68,$69,$10,$11,$10,$11,$00,$00,$00,$00,$10,$11,$68,$69,$10,$11
.byte $10,$11,$00,$00,$00,$00,$10,$11,$68,$69,$10,$11,$10,$11,$10,$11
.byte $00,$00,$10,$11,$68,$69,$10,$11,$10,$11,$10,$11,$00,$00,$00,$00
.byte $10,$11,$68,$69,$10,$11,$10,$11,$10,$11,$00,$00,$10,$11,$68,$69
.byte $10,$11,$10,$11,$10,$11,$00,$00,$00,$00,$10,$11,$68,$69,$10,$11
.byte $10,$11,$00,$00,$00,$00,$00,$00,$68,$69,$10,$11,$10,$11,$00,$00
.byte $00,$00,$00,$00,$00,$00,$10,$11,$10,$11,$10,$11,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$10,$11,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$10,$11,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
isoMap2:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$0E,$0F,$00,$00,$00,$00,$0E,$0F,$00,$00,$00,$00,$0E,$0F,$66
.byte $67,$00,$00,$00,$00,$0E,$0F,$00,$00,$0E,$0F,$66,$67,$36,$37,$0E
.byte $0F,$00,$00,$0E,$0F,$32,$33,$66,$67,$36,$37,$0E,$0F,$00,$00,$00
.byte $00,$0E,$0F,$66,$67,$32,$33,$0E,$0F,$00,$00,$00,$00,$0E,$0F,$62
.byte $63,$0E,$0F,$0E,$0F,$00,$00,$00,$00,$0E,$0F,$66,$67,$0E,$0F,$0E
.byte $0F,$0E,$0F,$00,$00,$32,$33,$66,$67,$0E,$0F,$0E,$0F,$0E,$0F,$00
.byte $00,$00,$00,$0E,$0F,$66,$67,$0E,$0F,$0E,$0F,$0E,$0F,$00,$00,$0E
.byte $0F,$36,$37,$0E,$0F,$0E,$0F,$0E,$0F,$00,$00,$00,$00,$0E,$0F,$66
.byte $67,$0E,$0F,$0E,$0F,$00,$00,$00,$00,$00,$00,$66,$67,$0E,$0F,$0E
.byte $0F,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$0F,$0E,$0F,$0E,$0F,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$0F,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$0F,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
isoMap3:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$56,$57,$5E,$5F,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
isoMap4:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$0C,$0D,$00,$00,$00,$00,$0C,$0D,$00,$00,$00,$00
.byte $0C,$0D,$64,$65,$00,$00,$00,$00,$0C,$0D,$00,$00,$0C,$0D,$64,$65
.byte $34,$35,$0C,$0D,$00,$00,$0C,$0D,$30,$31,$64,$65,$34,$35,$0C,$0D
.byte $00,$00,$00,$00,$0C,$0D,$64,$65,$30,$31,$0C,$0D,$00,$00,$00,$00
.byte $0C,$0D,$60,$61,$0C,$0D,$0C,$0D,$00,$00,$00,$00,$0C,$0D,$64,$65
.byte $0C,$0D,$0C,$0D,$0C,$0D,$00,$00,$30,$31,$64,$65,$0C,$0D,$0C,$0D
.byte $0C,$0D,$00,$00,$00,$00,$0C,$0D,$64,$65,$0C,$0D,$0C,$0D,$0C,$0D
.byte $00,$00,$0C,$0D,$34,$35,$0C,$0D,$0C,$0D,$0C,$0D,$00,$00,$00,$00
.byte $0C,$0D,$64,$65,$0C,$0D,$0C,$0D,$00,$00,$00,$00,$00,$00,$64,$65
.byte $0C,$0D,$0C,$0D,$00,$00,$00,$00,$00,$00,$00,$00,$0C,$0D,$0C,$0D
.byte $0C,$0D,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0C,$0D
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0C,$0D,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
isoMap5:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$54,$55,$5C,$5D,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
isoMap6:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$2E,$2F,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$2E,$2F,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$2E,$2F,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$52,$53,$5A
.byte $5B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
isoMap7:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$2C,$2D
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$2C,$2D
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$2C,$2D
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $50,$51,$58,$59,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
