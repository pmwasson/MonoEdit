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

MAP_OFFSET_X    = 12
MAP_OFFSET_Y    = 2
MAP_RIGHT_EDGE  = MAP_OFFSET_X + MAP_WIDTH*2
MAP_BOTTOM_EDGE = MAP_OFFSET_Y + MAP_HEIGHT

BOX_HORZ        = $13
BOX_VERT        = $7c
BOX_UPPER_LEFT  = $1c
BOX_UPPER_RIGHT = $1d
BOX_LOWER_LEFT  = $1e
BOX_LOWER_RIGHT = $1f
BOX_LEFT_T      = $0f
BOX_RIGHT_T     = $11

IMAGE_TABLE     := $6000    ; AUX memory

;------------------------------------------------
.segment "CODE"
.org    $6000

;=============================================================================
; Main
;=============================================================================

.proc main

    jsr     initMonochrome
    sta     CLR80COL

    ; Display Title
    sta     HISCR

;    ; wait for key (ignore key)
;:
;    lda     KBD
;    bpl     :-
;    bit     KBDSTRB     ; clean up

    ; Clear screen 0 and draw map
    lda     #$00
    jsr     drawGameScreen

    ; Clear screen 1 and draw map
    lda     #$20
    jsr     drawGameScreen

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
    bne     :+
    inc     gameTime0
    bne     :+
    inc     gameTime1
:

; Update Tiles

    ldx     mapCursor

    ; Map 2
    lda     isoMapOp2,x
    tay
    lda     animateMap,y
    beq     :+
    sta     isoMapOp2,x
    inc     update
:
    ; Map 1
    lda     isoMapOp1,x
    tay
    lda     animateMap,y
    beq     :+
    sta     isoMapOp1,x
    inc     update
:
    ; Map 0
    lda     isoMapOp0,x
    tay
    lda     animateMap,y
    beq     :+
    sta     isoMapOp0,x
    inc     update
:

    lda     update
    beq     :+
    jsr     setCoordinate
    jsr     drawQuarter
:

    lda     #0
    sta     tileX
    lda     #23
    sta     tileY
    lda     gameTime1
    jsr     drawNumber
    lda     gameTime0
    jsr     drawNumber
    lda     mapCursor
    jsr     drawNumber

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
    jmp     DHGR_LOADER_MENU

continue:

    jmp     displayLoop

update:         .byte   0
storeWidth:     .byte   0
storeOffsetX:   .byte   0

.endproc

;------------------------------------------------
; Draw Game Screen
;
;   Clear screen and draw all elements
;------------------------------------------------
.proc drawGameScreen
    sta     drawPage

    ldx     #0
    jsr     setBackground
    jsr     DHGR_CLEAR_SCREEN
    jsr     drawFrame

    jsr     drawTitles

    lda     #0
    jsr     gameDrawImage

    jsr     drawText

    ldx     bgColor
    jsr     setBackground
    jsr     isoDrawMap

    rts

.endproc

;------------------------------------------------
; Draw Frame
;------------------------------------------------
.proc drawFrame

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

.endproc

;------------------------------------------------
; Draw Titles
;------------------------------------------------
.proc drawTitles
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
.endproc

;------------------------------------------------
; Draw Text
;------------------------------------------------
.proc drawText

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

;-----------------------------------------------------------------------------
; Game Draw Image
;
;   Calculate pointers and call engine
;-----------------------------------------------------------------------------
.proc gameDrawImage

    sta     temp
    asl
    asl
    adc     temp    ; *5
    adc     #>IMAGE_TABLE
    sta     tilePtr1
    adc     #2
    sta     maskPtr1
    lda     #0
    sta     tilePtr0
    lda     #$80
    sta     maskPtr0

    jmp     DHGR_DRAW_IMAGE_AUX

temp:   .byte   0

.endproc

;-----------------------------------------------------------------------------
; Init
;
;-----------------------------------------------------------------------------

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

;------------------------------------------------
; Draw Quarter
;------------------------------------------------
.proc drawQuarter
    ldy     tileY
    lda     playerLine,y
    ldx     mapCursor
    clc
    adc     isoMapOpInfo,x
    tay
    lda     jumpTable,y
    sta     *+4
    jmp     draw012         ; link return
.endproc

;-----------------------------------------------------------------------------
; isoDrawMap
;
;   Draw ISO map
;-----------------------------------------------------------------------------

.proc isoDrawMap

    sta     CLR80COL        ; Use RAMWRT for aux mem

    ldx     #0
    stx     isoIdxY

    lda     #MAP_OFFSET_Y
    sta     tileY

loopY:
    tay
    lda     playerLine,y
    sta     playerOffset

    lda     #MAP_OFFSET_X
    sta     tileX

    lda     isoIdxY
    tax

loopX1:
    lda     isoMapOpInfo,x
    clc
    adc     playerOffset
    tay
    lda     jumpTable,y

    sta     *+4
    jsr     draw012

    inx

    clc
    lda     tileX
    adc     #2
    sta     tileX
    cmp     #MAP_RIGHT_EDGE
    bmi     loopX1

    clc
    lda     isoIdxY
    adc     #MAP_WIDTH
    sta     isoIdxY

    inc     tileY
    lda     tileY
    cmp     #MAP_BOTTOM_EDGE
    bmi     loopY

    rts

isoIdxY:        .byte   0
playerOffset:   .byte   0

.endproc

.align 256

; should be draw012
.proc draw012
    lda     isoMapOp0,x
    jsr     DHGR_DRAW_BG_28X8
    lda     isoMapOp1,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMapOp2,x
    jsr     DHGR_DRAW_MASK_28X8
    rts
.endproc

.proc draw012P
    lda     isoMapOp0,x
    jsr     DHGR_DRAW_BG_28X8
    lda     isoMapOp1,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMapOp2,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMapOpP,x
    jsr     DHGR_DRAW_MASK_28X8
    rts
.endproc

.proc draw01P2
    lda     isoMapOp0,x
    jsr     DHGR_DRAW_BG_28X8
    lda     isoMapOp1,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMapOpP,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMapOp2,x
    jsr     DHGR_DRAW_MASK_28X8
    rts
.endproc

.proc draw0P12
    lda     isoMapOp0,x
    jsr     DHGR_DRAW_BG_28X8
    lda     isoMapOpP,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMapOp1,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMapOp2,x
    jsr     DHGR_DRAW_MASK_28X8
    rts
.endproc

.proc drawP012
    lda     isoMapOpP,x
    jsr     DHGR_DRAW_BG_28X8
    lda     isoMapOp0,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMapOp1,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMapOp2,x
    jsr     DHGR_DRAW_MASK_28X8
    rts
.endproc

.proc drawBad
    brk     ; Should not get here!
.endproc

;-----------------------------------------------------------------------------
; Tables
;-----------------------------------------------------------------------------

jumpTable:

    ; including "bad" to catch player/map (blocked) and map/map conflicts (more than 3 levels)
    ; should probably replace with 012P for release to avoid crashes with just a glitch

    ; no player - $00
    .byte   <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012
    .byte   <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012
    .byte   <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012
    .byte   <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012

    ; player7   - $20
    .byte   <draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P
    .byte   <draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<drawBad
    .byte   <draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<drawBad
    .byte   <draw012P,<draw012P,<draw012P,<drawBad, <draw012P,<drawBad, <drawBad, <drawBad

    ; player6   - $40
    .byte   <draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P
    .byte   <draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<drawBad
    .byte   <draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<drawBad
    .byte   <draw01P2,<draw01P2,<draw01P2,<drawBad, <draw01P2,<drawBad, <drawBad, <drawBad

    ; player5   - $60
    .byte   <draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P
    .byte   <draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<drawBad
    .byte   <draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<drawBad
    .byte   <draw0P12,<draw0P12,<draw0P12,<drawBad, <draw0P12,<drawBad, <drawBad, <drawBad

    ; player3   - $80
    .byte   <draw012P,<draw012P,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw0P12,<draw0P12
    .byte   <draw01P2,<draw01P2,<draw0P12,<draw0P12,<draw0P12,<draw0P12,<drawP012,<drawBad
    .byte   <draw01P2,<draw01P2,<draw0P12,<draw0P12,<draw0P12,<draw0P12,<drawP012,<drawBad
    .byte   <draw0P12,<draw0P12,<drawP012,<drawBad, <drawP012,<drawBad, <drawBad, <drawBad

; optmizied to use a global Y [0..23]
playerLine:
    .byte   $00,$00,$00,$00,$00,$00,$00,$00
    .byte   $00,$20,$40,$60,$80,$00,$00,$00     ; 9,10,11,12
    .byte   $00,$00,$00,$00,$00,$00,$00,$00


;-----------------------------------------------------------------------------
; setCoordinate
;
;-----------------------------------------------------------------------------
.proc setCoordinate
    ; cursor = yyyyxxxx

    lda     #MAP_OFFSET_Y
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
    adc     #MAP_OFFSET_X
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
; Global
;-----------------------------------------------------------------------------
mapCursor:  .byte   0   ; Offset into map table
gameTime0:  .byte   0   ; incremented every 256 frames
gameTime1:  .byte   0   ; incremented every 256*256 frames
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
.include "map00.asm"

; player
isoMapOpP:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$58,$59,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$5A,$5B,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$5C,$5D,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$5E,$5F,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00