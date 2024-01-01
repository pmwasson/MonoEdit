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

CURSOR_TILE     = $1c
CURSOR_INIT     = 2*MAP_WIDTH

CURSOR_W        = 256 - 2
CURSOR_E        =       2
CURSOR_N        = 256 - MAP_WIDTH*2
CURSOR_S        =       MAP_WIDTH*2
CURSOR_NE       = 256 - MAP_WIDTH   + 1
CURSOR_NW       = 256 - MAP_WIDTH   - 1
CURSOR_SE       =       MAP_WIDTH   + 1
CURSOR_SW       =       MAP_WIDTH   - 1

CURSOR_U2       = 256 - MAP_WIDTH*2
CURSOR_U1       = 256 - MAP_WIDTH
CURSOR_D1       =       MAP_WIDTH
CURSOR_D2       =       MAP_WIDTH*2
CURSOR_D3       =       MAP_WIDTH*3

MACRO_ERASE     = 20

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

    ; set up 80 columns
    lda     #23         ; put cursor on last line
    sta     CV
    jsr     VTAB

    ; display a greeting
    jsr     inline_print
    StringCR    "Isometric map editor - ? for help"

    ; init
    jsr     DHGR_INIT
    jsr     initMonochrome  ; Turn on monochrome dhgr

    ; Set ctrl-y vector
    lda     #$4c        ; JMP
    sta     $3f8
    lda     #<main
    sta     $3f9
    lda     #>main
    sta     $3fa

reset_loop:

    ldx     #7*4
    jsr     setBackground

    jsr     DHGR_CLEAR_SCREEN

    ldx     bgColor
    jsr     setBackground

    lda     #CURSOR_INIT
    sta     mapCursor

refresh_loop:
    jsr     isoDrawMap

command_loop:
    jsr     inline_print
    String  "Command:"

skip_prompt:
    jsr     getInput

    ;------------------
    ; ESC = Toggle Text
    ;------------------
    cmp     #KEY_ESC
    bne     :+
    ; dont display anything
    lda     TEXTMODE
    bmi     toggle_text_off
    bit     TXTSET
    jmp     skip_prompt
toggle_text_off:
    bit     TXTCLR
    jmp     skip_prompt
:

    ;==================
    ; Directions
    ;==================

    cmp     #$80 | 'S'
    bne     :+
    lda     #0              ; just print index
    jmp     cursorMove
:
    cmp     #$80 | 'Q'
    bne     :+
    lda     #CURSOR_NW
    jmp     cursorMove
:
    cmp     #$80 | 'W'
    bne     :+
    lda     #CURSOR_N
    jmp     cursorMove
:
    cmp     #$80 | 'E'
    bne     :+
    lda     #CURSOR_NE
    jmp     cursorMove
:
    cmp     #$80 | 'D'
    bne     :+
    lda     #CURSOR_E
    jmp     cursorMove
:
    cmp     #$80 | 'C'
    bne     :+
    lda     #CURSOR_SE
    jmp     cursorMove
:
    cmp     #$80 | 'X'
    bne     :+
    lda     #CURSOR_S
    jmp     cursorMove
:
    cmp     #$80 | 'Z'
    bne     :+
    lda     #CURSOR_SW
    jmp     cursorMove
:
    cmp     #$80 | 'A'
    bne     :+
    lda     #CURSOR_W
    jmp     cursorMove
:

    ;------------------
    ; 0-9 = choose macro (plus shift)
    ;------------------
    cmp     #KEY_0
    bmi     :+
    cmp     #KEY_9+1
    bpl     :+
    and     #$f
chooseMacro:
    sta     macroIndex
    jsr     inline_print
    .byte   "Set macro to ",0
    lda     macroIndex
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     command_loop
:

    ; Also check for shift number for the next set of macros:
    ; !  @  #  $  %  ^  &  *  (  )
    ; 11 12 13 14 15 16 17 18 19 10

    cmp     #$80 | ')'
    bne     :+
    lda     #10
    jmp     chooseMacro
:
    cmp     #$80 | '!'
    bne     :+
    lda     #11
    jmp     chooseMacro
:
    cmp     #$80 | '@'
    bne     :+
    lda     #12
    jmp     chooseMacro
:
    cmp     #$80 | '#'
    bne     :+
    lda     #13
    jmp     chooseMacro
:
    cmp     #$80 | '$'
    bne     :+
    lda     #14
    jmp     chooseMacro
:
    cmp     #$80 | '%'
    bne     :+
    lda     #15
    jmp     chooseMacro
:
    cmp     #$80 | '^'
    bne     :+
    lda     #16
    jmp     chooseMacro
:
    cmp     #$80 | '&'
    bne     :+
    lda     #17
    jmp     chooseMacro
:
    cmp     #$80 | '*'
    bne     :+
    lda     #18
    jmp     chooseMacro
:
    cmp     #$80 | '('
    bne     :+
    lda     #19
    jmp     chooseMacro
:
    ;------------------
    ; SP = Set Tile
    ;------------------
    cmp     #KEY_SPACE
    bne     :+
    jsr     inline_print
    StringCR "Set tile"
    ldy     macroIndex
    lda     macroOverlay,y
    tax
    lda     macroIndex
    jsr     setMapTile
    jmp     refresh_loop
:

    ;------------------
    ; DEL = Clear Tile
    ;------------------
    cmp     #KEY_DEL
    bne     :+
    jsr     inline_print
    StringCR "Delete tile"
    lda     #MACRO_ERASE
    ldx     #1
    jsr     eraseMapTile
    jmp     refresh_loop
:

    ;------------------
    ; ^R = Rotate
    ;------------------
    cmp     #KEY_CTRL_R
    bne     rotate_after
    jsr     inline_print
    .byte   "Rotate Direction (or cancel):",0
    jsr     getInputDirection
    beq     rotate_cancel

    cmp     #KEY_UP
    bne     :+
    jsr     rotateUp
    jmp     rotate_done
:

    cmp     #KEY_DOWN
    bne     :+
    jsr     rotateDown
    jmp     rotate_done
:
    cmp     #KEY_LEFT
    bne     :+
    jsr     rotateLeft
    jmp     rotate_done
:
    ; must be right
    jsr     rotateRight

rotate_done:
    jmp     refresh_loop

rotate_cancel:
    jmp     command_loop

rotate_after:
    ;------------------
    ; ^E = erase map 
    ;------------------
    cmp     #KEY_CTRL_E
    bne     :+
    jsr     inline_print
    StringCR   "Erase map"
    jsr     eraseMap
    jmp     refresh_loop
:

    ;------------------
    ; ^O = Optimize map
    ;------------------
    cmp     #KEY_CTRL_O
    bne     :+
    jsr     inline_print
    StringCR   "Experiment to optimize map to 3 levels"
    jsr     optimizeMap
    jmp     refresh_loop
:

    ;------------------
    ; ^T = Test map
    ;------------------
    cmp     #KEY_CTRL_T
    bne     :+
    jsr     inline_print
    StringCR   "Testing Map"
    jsr     testMap
    jmp     reset_loop
:

    ;------------------
    ; ^P = Print
    ;------------------
    cmp     #KEY_CTRL_P
    bne     :+
    bit     TXTSET
    jsr     inline_print
    StringCR   "Print map"
    jsr     printMap
    jmp     command_loop
:

    ;------------------
    ; Tab = switch tools
    ;------------------
    cmp     #KEY_TAB
    bne     :+
    jsr     inline_print
    StringCR    "Switching tools..."
    jmp     $4000       ; Maybe should look into the linker
:

    ;------------------
    ; Q = QUIT
    ;------------------
    cmp     #KEY_CTRL_Q
    bne     :+
    jsr     inline_print
    .byte   "Quit",13,0
    bit     TXTSET
    jmp     quit
:

    ;------------------
    ; \ = Monitor
    ;------------------
    cmp     #$80 | '\'
    bne     :+
    jsr     inline_print
    .byte   "Monitor",13,"(enter CTRL-Y to return)",13,0

    bit     TXTSET
    jmp     MONZ        ; enter monitor
:

    ;------------------
    ; ? = HELP
    ;------------------
    cmp     #$80 + '?'
    bne     :+
    jsr     inline_print
    .byte   "Help (ESC when done)",13,0
    jsr     printHelp
    jmp     command_loop
:

    ;------------------
    ; ^C = Color
    ;------------------
    cmp     #KEY_CTRL_C
    bne     :+
    jsr     inline_print
    StringCR   "Toggle Color Mode"
    lda     colorMode
    beq     doBW 
    lda     #0
    sta     colorMode
    jsr     initColorMode
    jmp     command_loop
doBW:
    lda     #1
    sta     colorMode
    jsr     initMonochrome
    jmp     command_loop

colorMode:  .byte $1

:
    ;------------------
    ; ^B = Background
    ;------------------
    cmp     #KEY_CTRL_B
    beq     :+
    jmp     notBackground
:
    jsr     inline_print
    String  "Choose background: (0=black, 1=white, 2=gray1, 3=gray2, 4=horizonal, 5=vertical, 6=stripes, 7=border, 8=checker): "
    lda     #9
    jsr     getInputNumber
    bmi     backgroundCancel
    asl
    asl     ; *4
    sta     bgColor
    lda     #13
    jsr     COUT
    jmp     reset_loop

backgroundCancel:
    jsr     inline_print
    StringCR  "Cancel"
    jmp     command_loop
notBackground:

    ;------------------
    ; Unknown
    ;------------------
    jsr     inline_print
    .byte   "Unknown command (? for help)",13,0
    jmp     command_loop

cursorMove:
    clc
    adc     mapCursor
    ldx     mapCursor
    sta     mapCursor
    stx     prevCursor
    jsr     setCoordinate

    ; stay on-grid
    lda     tileX
    lsr     ; /2
    eor     tileY
    eor     displayOffsetX
    eor     displayOffsetY
    and     #1
    bne     badCursorMove

    ; check bottom (assume top wraps)
    lda     tileY
    clc
    adc     #1              ; half tile
    sec
    sbc     displayOffsetY
    cmp     displayHeight
    bcs     badCursorMove

    ; check right (assume left wraps)
    lda     tileX
    clc
    adc     #2              ; half tile
    sec
    sbc     displayOffsetX
    cmp     displayWidth
    bcs     badCursorMove

    jmp     doneCursorMove

badCursorMove:
    lda     prevCursor
    sta     mapCursor
doneCursorMove:
    jsr     inline_print
    .byte   "Map index:",0
    lda     mapCursor
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     command_loop

prevCursor:  .byte   0
.endproc

;-----------------------------------------------------------------------------
; printHelp
;-----------------------------------------------------------------------------
.proc printHelp
    bit     TXTSET
    jsr     inline_print
    StringCont  "  [QWE]:   Move cursor North-West, North, or North-East"
    StringCont  "  [A D]:   Move cursor West or East"
    StringCont  "  [ZXC]:   Move cursor South-West, South, or South-East"
    StringCont  "  Space:   Set current map position to selected macro"
    StringCont  "  Del:     Clear current map position"
    StringCont  "  Ctrl-P:  Print all tiles (do a 1^P in monitor first, 3^P after!)"
    StringCont  "  Ctrl-B:  Display in black and white"
    StringCont  "  Ctrl-C:  Display in color (not recommended)"
    StringCont  "  ?:       This help screen"
    StringCont  "  \:       Monitor"
    StringCont  "  Ctrl-Q:  Quit"
    StringCont  "  Escape:  Toggle text/graphics"
    StringCont  "  Tab:     Switch Tools"
    .byte   0

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
    adc     #CURSOR_D1
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
; getInput
;   Blink cursors and wait for keypress
;   Return key in A (upper bit set)
;-----------------------------------------------------------------------------
.proc getInput

cursor_loop:
    ; Display cursor
    lda     #$FF
    jsr     COUT

    jsr     drawCursor

    ; Wait (on)
    jsr     wait

    ; Restore
    lda     #$88        ; backspace
    jsr     COUT
    lda     #$A0        ; space
    jsr     COUT
    lda     #$88        ; backspace
    jsr     COUT

    jsr     drawMapTile

    ; check for keypress
    lda     KBD
    bmi     exit

    ; Wait (off)
    jsr     wait

    ; check for keypress
    lda     KBD
    bpl     cursor_loop

exit:
    bit     KBDSTRB     ; clean up

    rts

; Wait loop that can be interrupted by key-press
wait:
    ldx     #$80
wait_x:
    ldy     #0
wait_y:
    lda     KBD
    bmi     waitExit
    dey
    bne     wait_y
    dex
    bne     wait_x
waitExit:
    rts

.endproc


;-----------------------------------------------------------------------------
; drawCursor
;
;-----------------------------------------------------------------------------
.proc drawCursor

    sta     CLR80COL        ; Use RAMWRT for aux mem

    jsr     setCoordinate
    lda     #CURSOR_TILE
    jsr     DHGR_DRAW_BG_28X8

    inc     tileX
    inc     tileX
    lda     #CURSOR_TILE+1
    jsr     DHGR_DRAW_BG_28X8

    dec     tileX
    dec     tileX
    inc     tileY
    lda     #CURSOR_TILE+2
    jsr     DHGR_DRAW_BG_28X8

    inc     tileX
    inc     tileX
    lda     #CURSOR_TILE+3
    jsr     DHGR_DRAW_BG_28X8

    rts

.endproc

;-----------------------------------------------------------------------------
; drawMapTile
;
;-----------------------------------------------------------------------------
.proc drawMapTile

    sta     CLR80COL        ; Use RAMWRT for aux mem

    jsr     setCoordinate
    lda     mapCursor
    sta     index

    jsr     drawQuarter

    inc     index
    inc     tileX
    inc     tileX
    jsr     drawQuarter

    lda     index
    clc
    adc     #MAP_WIDTH-1
    sta     index

    dec     tileX
    dec     tileX
    inc     tileY
    jsr     drawQuarter

    inc     index
    inc     tileX
    inc     tileX
    jsr     drawQuarter

    rts

drawQuarter:
    ldx     index
    lda     isoMap0,x
    jsr     DHGR_DRAW_BG_28X8
    ldx     index
    lda     isoMap1,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     index
    lda     isoMap2,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     index
    lda     isoMap3,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     index
    lda     isoMap4,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     index
    lda     isoMap5,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     index
    lda     isoMap6,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     index
    lda     isoMap7,x
    jsr     DHGR_DRAW_MASK_28X8
    rts

index:   .byte   0

.endproc

.proc drawMapMacroTile

    jsr     drawMapTile

    lda     mapCursor
    sta     temp
    clc
    adc     #CURSOR_N
    sta     mapCursor
    jsr     drawMapTile

    lda     temp
    clc
    adc     #CURSOR_S
    sta     mapCursor
    jsr     drawMapTile

    lda     temp
    sta     mapCursor
    rts

temp:   .byte   0

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
; setMacroPointer
;
;   Index passed in A
;-----------------------------------------------------------------------------
.proc setMacroPointer
    sta     temp
    ; calculate pointer
    asl                     ; *16
    asl
    asl
    asl
    sta     tilePtr0
    lda     temp
    lsr                     ; /16
    lsr
    lsr
    lsr
    clc
    adc     #>macroList
    sta     tilePtr1
    rts

temp:   .byte   0

.endproc

;-----------------------------------------------------------------------------
; erase tile
;
;-----------------------------------------------------------------------------
.proc eraseMapTile

    lda     mapCursor
    clc
    adc     #CURSOR_U2
    tax
    lda     #0
    sta     isoMap7,x
    sta     isoMap7+1,x

    lda     mapCursor
    clc
    adc     #CURSOR_U1
    tax
    lda     #0
    sta     isoMap6,x
    sta     isoMap6+1,x

    ldx     mapCursor
    sta     isoMap5,x
    sta     isoMap5+1,x
    sta     isoMap4,x
    sta     isoMap4+1,x

    lda     mapCursor
    clc
    adc     #CURSOR_D1
    tax
    lda     #0
    sta     isoMap3,x
    sta     isoMap3+1,x
    sta     isoMap2,x
    sta     isoMap2+1,x

    lda     mapCursor
    clc
    adc     #CURSOR_D2
    tax
    lda     #0
    sta     isoMap1,x
    sta     isoMap1+1,x

    lda     mapCursor
    clc
    adc     #CURSOR_D3
    tax
    lda     #0
    sta     isoMap0,x
    sta     isoMap0+1,x

    rts

.endproc

;-----------------------------------------------------------------------------
; setMap tile
;
;-----------------------------------------------------------------------------
.proc setMapTile
    stx     overwrite
    jsr     setMacroPointer

    ;---------------------------------
    ; up2 -> map7 (macro 0,1)
    ldy     #0
    lda     mapCursor
    clc
    adc     #CURSOR_U2
    tax

    lda     (tilePtr0),y
    ora     overwrite
    beq     :+
    lda     (tilePtr0),y
    sta     isoMap7,x
    inx
    iny
    lda     (tilePtr0),y
    sta     isoMap7,x
:
    ;---------------------------------
    ; up1 -> map6 (macro 2,3)
    ldy     #2
    lda     mapCursor
    clc
    adc     #CURSOR_U1
    tax

    lda     (tilePtr0),y
    ora     overwrite
    beq     :+
    lda     (tilePtr0),y
    sta     isoMap6,x
    inx
    iny
    lda     (tilePtr0),y
    sta     isoMap6,x
:
    ;---------------------------------
    ; zero -> map5 (macro 4,5) -- OVERLAY
    ldy     #4
    ldx     mapCursor

    lda     (tilePtr0),y
    ora     overwrite
    beq     :+
    lda     (tilePtr0),y
    sta     isoMap5,x
    inx
    iny
    lda     (tilePtr0),y
    sta     isoMap5,x
:
    ;---------------------------------
    ; zero -> map4 (macro 6,7)
    ldy     #6
    ldx     mapCursor

    lda     (tilePtr0),y
    ora     overwrite
    beq     :+
    lda     (tilePtr0),y
    sta     isoMap4,x
    inx
    iny
    lda     (tilePtr0),y
    sta     isoMap4,x
:
    ;---------------------------------
    ; down1 -> map6 (macro 8,9) -- OVERLAY
    ldy     #8
    lda     mapCursor
    clc
    adc     #CURSOR_D1
    tax

    lda     (tilePtr0),y
    ora     overwrite
    beq     :+
    lda     (tilePtr0),y
    sta     isoMap3,x
    inx
    iny
    lda     (tilePtr0),y
    sta     isoMap3,x
:
    ;---------------------------------
    ; down1 -> map6 (macro 10,11)
    ldy     #10
    lda     mapCursor
    clc
    adc     #CURSOR_D1
    tax

    lda     (tilePtr0),y
    ora     overwrite
    beq     :+
    lda     (tilePtr0),y
    sta     isoMap2,x
    inx
    iny
    lda     (tilePtr0),y
    sta     isoMap2,x
:
    ;---------------------------------
    ; down2 -> map7 (macro 12,13)
    ldy     #12
    lda     mapCursor
    clc
    adc     #CURSOR_D2
    tax

    lda     (tilePtr0),y
    ora     overwrite
    beq     :+
    lda     (tilePtr0),y
    sta     isoMap1,x
    inx
    iny
    lda     (tilePtr0),y
    sta     isoMap1,x
:
    ;---------------------------------
    ; down3 -> map7 (macro 14,15)
    ldy     #14
    lda     mapCursor
    clc
    adc     #CURSOR_D3
    tax

    lda     (tilePtr0),y
    ora     overwrite
    beq     :+
    lda     (tilePtr0),y
    sta     isoMap0,x
    inx
    iny
    lda     (tilePtr0),y
    sta     isoMap0,x
:
    rts

overwrite:      .byte   0

.endproc

;-----------------------------------------------------------------------------
; printMap
;-----------------------------------------------------------------------------
.proc printMap

    lda     #<isoMap0
    sta     tilePtr0
    lda     #>isoMap0
    sta     tilePtr1
    jsr     inline_print
    StringCR    "isoMap0:"
    jsr     printMapSection

    lda     #<isoMap1
    sta     tilePtr0
    lda     #>isoMap1
    sta     tilePtr1
    jsr     inline_print
    StringCR    "isoMap1:"
    jsr     printMapSection

    lda     #<isoMap2
    sta     tilePtr0
    lda     #>isoMap2
    sta     tilePtr1
    jsr     inline_print
    StringCR    "isoMap2:"
    jsr     printMapSection

    lda     #<isoMap3
    sta     tilePtr0
    lda     #>isoMap3
    sta     tilePtr1
    jsr     inline_print
    StringCR    "isoMap3:"
    jsr     printMapSection

    lda     #<isoMap4
    sta     tilePtr0
    lda     #>isoMap4
    sta     tilePtr1
    jsr     inline_print
    StringCR    "isoMap4:"
    jsr     printMapSection

    lda     #<isoMap5
    sta     tilePtr0
    lda     #>isoMap5
    sta     tilePtr1
    jsr     inline_print
    StringCR    "isoMap5:"
    jsr     printMapSection

    lda     #<isoMap6
    sta     tilePtr0
    lda     #>isoMap6
    sta     tilePtr1
    jsr     inline_print
    StringCR    "isoMap6:"
    jsr     printMapSection

    lda     #<isoMap7
    sta     tilePtr0
    lda     #>isoMap7
    sta     tilePtr1
    jsr     inline_print
    StringCR    "isoMap7:"
    jsr     printMapSection

    rts
.endproc

.proc printMapSection
    jsr     inline_print
    .byte   ".byte ",0

    lda     #0
    sta     dump_count
    jmp     dump_loop
dump_comma:
    lda     #$80 + ','
    jsr     COUT
dump_loop:
    lda     #$80 + '$'
    jsr     COUT
    ldy     dump_count
    lda     (tilePtr0),y
    jsr     PRBYTE
    inc     dump_count
    beq     dump_finish     ; assuming 256 bytes
    lda     dump_count
    and     #$f
    bne     dump_comma
    jsr     inline_print
    .byte   13,".byte ",0
    jmp     dump_loop

dump_finish:
    lda     #13
    jsr     COUT
    rts

dump_count: .byte   0

.endproc

;-----------------------------------------------------------------------------
; optimized map
;   find first (3) levels that are non zero and remove the rest
;-----------------------------------------------------------------------------


.proc optimizeMap

    ldy     #0

loop:
    ldx     #0
    stx     result

    ; clear stack
    lda     #0
    sta     stack
    sta     stack+1
    sta     stack+2
    sta     stack+3

    lda     isoMap7,y
    jsr     push
    bcs     next7

    lda     isoMap6,y
    jsr     push
    bcs     next6

    lda     isoMap5,y
    jsr     push
    bcs     next5

    lda     isoMap4,y
    jsr     push
    bcs     next4

    lda     isoMap3,y
    jsr     push
    bcs     next3

    lda     isoMap2,y
    jsr     push
    bcs     next2

    lda     isoMap1,y
    jsr     push
    bcs     next1

    lda     isoMap0,y
    jsr     push
    jmp     next0

next7:
next6:
    brk                 ; should not be possible
next5:
    clc
    rol
next4:
    clc
    rol
next3:
    clc
    rol
next2:
    clc
    rol
next1:
    clc
    rol
next0:
    ; write results
    txa
    sta     isoMapLevels,y
    lda     result
    sta     isoMapInfo,y
    lda     #0
    sta     isoMap7,y
    sta     isoMap6,y
    sta     isoMap5,y
    sta     isoMap4,y
    sta     isoMap3,y
    lda     stack
    sta     isoMap2,y
    lda     stack+1
    sta     isoMap1,y
    lda     stack+2
    sta     isoMap0,y

    iny
    beq     done
    jmp     loop
done:
    rts

push:
    beq     :+
    sta     stack,x    
    sec
    rol     result      ; set result bit to 1
    inx
    cpx     #3
    bne     :+
    sec
    rts
:
    clc
    rol     result      ; set result bit to zero
    clc
    rts

result:     .byte   0
stack:      .byte   0,0,0,0

.endproc

;-----------------------------------------------------------------------------
; erase map
;-----------------------------------------------------------------------------

.proc eraseMap
    ldy     #0
loop:
    lda     #0
    sta     isoMap7,y
    sta     isoMap6,y
    sta     isoMap5,y
    sta     isoMap4,y
    sta     isoMap3,y
    sta     isoMap2,y
    sta     isoMap1,y
    sta     isoMap0,y
    iny
    bne     loop
    rts
.endproc

;-----------------------------------------------------------------------------
; rotate up
;-----------------------------------------------------------------------------

rotateRight:
rotateLeft:
    rts             ; FIXME

.proc rotateDown
    ldy     #255

loop:

.repeat 8,I
    ; swap rows
    lda     .ident(.concat("isoMap",.string(I))),y
    tax
    lda     .ident(.concat("isoMap",.string(I)))+CURSOR_S,y
    sta     .ident(.concat("isoMap",.string(I))),y
    txa
    sta     .ident(.concat("isoMap",.string(I)))+CURSOR_S,y
.endrepeat

    dey

    cpy     #CURSOR_S
    bne     loop
    rts

.endproc


.proc rotateUp
    ldy     #0

loop:

.repeat 8,I
    ; swap rows
    lda     .ident(.concat("isoMap",.string(I))),y
    tax
    lda     .ident(.concat("isoMap",.string(I)))+CURSOR_S,y
    sta     .ident(.concat("isoMap",.string(I))),y
    txa
    sta     .ident(.concat("isoMap",.string(I)))+CURSOR_S,y
.endrepeat

    iny

    cpy     #256-CURSOR_S
    bne     loop
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
; Test Map
;-----------------------------------------------------------------------------
.proc testMap

    sta     MIXCLR  ; full screen

    ; Clear screen 0 and draw map
    lda     #$00
    sta     drawPage

    ldx     #0
    jsr     setBackground
    jsr     DHGR_CLEAR_SCREEN
    jsr     drawFrame

    ldx     #2*4
    jsr     setBackground
    jsr     isoDrawMap

    ; Clear screen 1 and draw map
    lda     #$20
    sta     drawPage

    ldx     #0
    jsr     setBackground
    jsr     DHGR_CLEAR_SCREEN
    jsr     drawFrame
    ldx     #2*4
    jsr     setBackground
    jsr     isoDrawMap

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

    lda     #1
    sta     boxLeft
    lda     #1
    sta     boxTop
    lda     #22
    sta     boxRight
    lda     #12
    sta     boxBottom
    clc
    jsr     drawBox

    lda     #1
    sta     boxLeft
    lda     #12
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

;-----------------------------------------------------------------------------
; Draw Image
;
;   Data is split between even and odd using tilePtr and maskPtr
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

    lda     imageY
    tax
    clc
    adc     imageHeight
    sta     imageEnd

loopY:
    lda     DHGR_LINE_OFFSET,x
    clc
    adc     imageX
    sta     screenPtr0
    lda     DHGR_LINE_PAGE,x
    sta     screenPtr1

    lda     #8
    sta     lineCount
loop8:

    ldy     #0
loopX:
    sta     RAMWRTON            ; aux
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    sta     RAMWRTOFF           ; main
    lda     (maskPtr0),y
    sta     (screenPtr0),y
    iny
    cpy     imageWidth
    bne     loopX

    ; increment pointers

    clc
    lda     imageWidth
    adc     tilePtr0
    sta     tilePtr0
    lda     #0
    adc     tilePtr1
    sta     tilePtr1

    clc
    lda     imageWidth
    adc     maskPtr0
    sta     maskPtr0
    lda     #0
    adc     maskPtr1
    sta     maskPtr1

    clc
    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dec     lineCount
    bne     loop8

    inx
    cpx     imageEnd
    bne     loopY

    rts

lineCount:  .byte   0
imageEnd:   .byte   0

; The follow use 2-byte increments for X / Width
;            and 8-byte increments for Y / Height
imageWidth:     .byte   10
imageHeight:    .byte   8
imageX:         .byte   1       ; [0..39-width]
imageY:         .byte   1       ; [0..23-height]

imageTable:
    .word   0
    .word   0
.endproc


;-----------------------------------------------------------------------------
; Utilies

.include "inline_print.asm"
.include "common_funct.asm"

;-----------------------------------------------------------------------------
; Global
;-----------------------------------------------------------------------------

displayOffsetX: .byte   12
displayOffsetY: .byte   2
displayWidth:   .byte   MAP_WIDTH*2
displayHeight:  .byte   MAP_HEIGHT

mapCursor:  .byte   0   ; Offset into map table
macroIndex: .byte   1
bgColor:    .byte   2*4

boxLeft:        .byte   0
boxRight:       .byte   0
boxTop:         .byte   0
boxBottom:      .byte   0

;-----------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------

macroOverlay:
    .byte   1       ;   0  Cube
    .byte   1       ;   1  Grass
    .byte   1       ;   2  Wate
    .byte   1       ;   3  Reed
    .byte   1       ;   4  Tile
    .byte   1       ;   5  Brick Wall
    .byte   0       ;   6  Wizard
    .byte   0       ;   7  Robot
    .byte   0       ;   8  Goofy
    .byte   0       ;   9  Chair-left
    .byte   0       ;   10 Pattern Cube
    .byte   1       ;   11 Tree
    .byte   1       ;   12 Pond rock
    .byte   0       ;   13
    .byte   0       ;   14
    .byte   0       ;   15
    .byte   0       ;   16
    .byte   0       ;   17
    .byte   0       ;   18
    .byte   0       ;   19 Chair-right

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


macroList:

;  Upper
;  Upper
;  Overlay
;  Overlay
;  Floor
;  Floor
;  Below
;  Below  (needed?)

; 0 - cube
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $04,$05
.byte   $00,$00
.byte   $06,$07
.byte   $08,$09
.byte   $00,$00

; 1 - grass
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $0c,$0d
.byte   $00,$00
.byte   $0e,$0f
.byte   $10,$11
.byte   $00,$00

; 2 - water
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $14,$15
.byte   $00,$00
.byte   $16,$17
.byte   $18,$19
.byte   $1a,$1b

; 3 - reed
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $34,$35
.byte   $00,$00
.byte   $36,$37
.byte   $18,$19
.byte   $1a,$1b

; 4 - tile
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $1c,$1d
.byte   $00,$00
.byte   $1e,$1f
.byte   $00,$00
.byte   $00,$00

; 5 - brick wall
.byte   $00,$00
.byte   $20,$21
.byte   $00,$00
.byte   $22,$23
.byte   $00,$00
.byte   $24,$25
.byte   $26,$27
.byte   $00,$00

; 6 - Wizard
.byte   $50,$51
.byte   $52,$53
.byte   $54,$55
.byte   $00,$00
.byte   $56,$57
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00

; 7 - Robot
.byte   $58,$59
.byte   $5a,$5b
.byte   $5c,$5d
.byte   $00,$00
.byte   $5e,$5f
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00

; 8 - Goofy
.byte   $48,$49
.byte   $4a,$4b
.byte   $4c,$4d
.byte   $00,$00
.byte   $4e,$4f
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00

; 9 - Chair Left
.byte   $00,$00
.byte   $00,$00
.byte   $38,$39
.byte   $00,$00
.byte   $3a,$3b
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00

; 10 - Pattern cube
.byte   $40,$41
.byte   $42,$43
.byte   $00,$00
.byte   $44,$45
.byte   $00,$00
.byte   $46,$47
.byte   $00,$00
.byte   $00,$00

; 11 - tree
.byte   $2c,$2d
.byte   $2e,$2f
.byte   $00,$00
.byte   $30,$31
.byte   $00,$00
.byte   $32,$33
.byte   $10,$11
.byte   $00,$00

; 12 - pond rock
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $60,$61
.byte   $00,$00
.byte   $62,$63
.byte   $18,$19
.byte   $1a,$1b

; 13
.res    16

; 14
.res    16

; 15
.res    16

; 16
.res    16

; 17
.res    16

; 18
.res    16

; 19 - Chair Right
.byte   $00,$00
.byte   $00,$00
.byte   $3c,$3d
.byte   $00,$00
.byte   $3e,$3f
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00

; 20 - DEL
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00



.align 256

isoMap0:    ; down 3
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

isoMap1:    ; down 2
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

isoMap2:    ; down 1
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

isoMap3:    ; down 1'
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

isoMap4:    ; zero
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

isoMap5:    ; zero'
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

isoMap6:    ; up1
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

isoMap7:    ; up2
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

isoMapInfo:     .res    256
isoMapLevels:     .res    256

