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

MAP_XOFFSET := 4
MAP_YOFFSET := 2

; map-width     map-height  map-length <= 256
; ---------     ----------  ----------
; 4*            14          256
; 5             10          240
; 6             8           240
; 7             7           252
; 8*            6           256
; 9             5           252
; 10            4           240
; * = power of 2

MAP_WIDTH       = 8     ;  MAP_WIDTH    *2
MAP_HEIGHT      = 6     ; (MAP_HEIGHT+2)*2
MAP_LENGTH      = (MAP_WIDTH*2)*(MAP_HEIGHT+2)*2

CURSOR_TILE     = $38
CURSOR_INIT     = 2*MAP_WIDTH*2

CURSOR_W        = 256 - 2
CURSOR_E        =       2
CURSOR_N        = 256 - MAP_WIDTH*2*2
CURSOR_S        =       MAP_WIDTH*2*2
CURSOR_NE       = 256 - MAP_WIDTH*2   + 1
CURSOR_NW       = 256 - MAP_WIDTH*2   - 1
CURSOR_SE       =       MAP_WIDTH*2   + 1
CURSOR_SW       =       MAP_WIDTH*2   - 1

MACRO_ERASE     = 10

;------------------------------------------------
.segment "CODE"
.org    $4000

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

    ; set background to gray
    ;lda     #$55
    ;sta     clearColor+0
    ;lda     #$2a
    ;sta     clearColor+1

    ; white background
    lda     #$7f
    sta     clearColor+0
    lda     #$7f    
    sta     clearColor+1
    lda     #$55
    sta     clearColor+2
    lda     #$2a
    sta     clearColor+3

reset_loop:
    jsr     clearScreen
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
    ; 0-9 = choose macro
    ;------------------
    cmp     #KEY_0
    bmi     :+
    cmp     #KEY_9+1
    bpl     :+
    and     #$f
    sta     macroIndex
    jsr     inline_print
    .byte   "Set macro to ",0
    lda     macroIndex
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     command_loop
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
    jsr     setMapTile
    jmp     refresh_loop
:


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
    jmp     $6000       ; Maybe should look into the linker
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

    ; Set ctrl-y vector
    lda     #$4c        ; JMP
    sta     $3f8
    lda     #<main
    sta     $3f9
    lda     #>main
    sta     $3fa

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
    StringCR   "Color Mode"
    jsr     initColorMode
    jmp     command_loop
:

    ;------------------
    ; ^B = B&W
    ;------------------
    cmp     #KEY_CTRL_B
    bne     :+
    jsr     inline_print
    StringCR   "Monochrome Mode"
    jsr     initMonochrome
    jmp     command_loop
:

    ;------------------
    ; Unknown
    ;------------------
    jsr     inline_print
    .byte   "Unknown command (? for help)",13,0
    jmp     command_loop

cursorMove:
    clc
    adc     mapCursor
    sta     newCursor

    ; check if new cursor is good

    ; both nibble have to be even or both odd
    lsr
    lsr
    lsr
    lsr
    eor     newCursor
    and     #1
    bne     doneCursorMove  ; odd result -> bad offset

    ; past top
    lda     newCursor
    cmp     #$20
    bcc     doneCursorMove

    ; past bottom
    lda     newCursor
    cmp     #$d0    ;c+1
    bcs     doneCursorMove

    ; past right
    and     #$f
    cmp     #$0f    ;e+1
    bcs     doneCursorMove

    lda     newCursor
    sta     mapCursor
doneCursorMove:
    jsr     inline_print
    .byte   "Map index:",0
    lda     mapCursor
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     command_loop

newCursor:  .byte   0
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
; isoDrawMap
;
;   Draw ISO map
;-----------------------------------------------------------------------------

.proc isoDrawMap

    sta     CLR80COL        ; Use RAMWRT for aux mem

    ldx     #0
    stx     isoIdx

    lda     #MAP_YOFFSET
    sta     tileY

loopY:
    lda     #MAP_XOFFSET
    sta     tileX

loopX1:
    ldx     isoIdx
    lda     isoMap0,x
    jsr     DHGR_DRAW_28X8
    ldx     isoIdx
    lda     isoMap1,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     isoIdx
    lda     isoMap2,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     isoIdx
    lda     isoMap3,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     isoIdx
    lda     isoMap4,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     isoIdx
    lda     isoMap5,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     isoIdx
    lda     isoMap6,x
    jsr     DHGR_DRAW_MASK_28X8
    ldx     isoIdx
    lda     isoMap7,x
    jsr     DHGR_DRAW_MASK_28X8

    inc     isoIdx

    clc
    lda     tileX
    adc     #2
    sta     tileX
    cmp     #MAP_XOFFSET+MAP_WIDTH*4
    bmi     loopX1

    inc     tileY
    lda     tileY
    cmp     #MAP_YOFFSET+(MAP_HEIGHT+2)*2
    bmi     loopY

    rts

isoIdx:    .byte   0
isoX:      .byte   0
isoY:      .byte   0
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
    jsr     DHGR_DRAW_MASK_28X8

    inc     tileX
    inc     tileX
    lda     #CURSOR_TILE+2
    jsr     DHGR_DRAW_MASK_28X8

    dec     tileX
    dec     tileX
    inc     tileY
    lda     #CURSOR_TILE+4
    jsr     DHGR_DRAW_MASK_28X8

    inc     tileX
    inc     tileX
    lda     #CURSOR_TILE+6
    jsr     DHGR_DRAW_MASK_28X8

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
    adc     #MAP_WIDTH*2-1
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
    jsr     DHGR_DRAW_28X8
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

    lda     mapCursor
    and     #$0f        ; x 0..15
    asl                 ; *2 (subtile 2 wide)
    clc
    adc     #MAP_XOFFSET
    sta     tileX

    lda     mapCursor
    and     #$f0        ; y 0..15
    lsr
    lsr
    lsr
    lsr                 ; /16
    clc
    adc     #MAP_YOFFSET
    sta     tileY

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
; setMap tile
;
;-----------------------------------------------------------------------------
.proc setMapTile
    stx     overwrite
    jsr     setMacroPointer

    ;---------------------------------
    ; above (2 rows above cursor)
    ; -> map7/6
    ldy     #0                  ; above
    lda     mapCursor
    clc
    adc     #CURSOR_N
    tax
    clc
    adc     #MAP_WIDTH*2
    sta     nextLineIndex

    ; above - row 0
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
    ldx     nextLineIndex
    ldy     #2
    ; above - row 1
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
    ; overlay (at cursor)
    ; -> map5/4
    ldy     #4                  ; above
    lda     mapCursor
    tax
    clc
    adc     #MAP_WIDTH*2
    sta     nextLineIndex

    ; overlay - row 0
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
    ldx     nextLineIndex
    ldy     #6
    ; overlay - row 1
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
    ; floor (at cursor)
    ; -> map3/2
    ldy     #8                  ; floor
    lda     mapCursor
    tax
    clc
    adc     #MAP_WIDTH*2
    sta     nextLineIndex

    ; floor - row 0
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
    ldx     nextLineIndex
    ldy     #10
    ; floor - row 1
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
    ; below (2 rows below cursor)
    ; -> map1/0
    ldy     #12                 ; below
    lda     mapCursor
    clc
    adc     #CURSOR_S
    tax
    clc
    adc     #MAP_WIDTH*2
    sta     nextLineIndex

    ; below - row 0
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
    ldx     nextLineIndex
    ldy     #14
    ; below - row 1
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
nextLineIndex:  .byte   0

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
;   find first (4) levels that are non zero and remove the rest
;-----------------------------------------------------------------------------


.proc optimizeMap

    ldy     #0
loop:
    ldx     #0

    ; clear stack
    lda     #0
    sta     stack
    sta     stack+1
    sta     stack+2
    sta     stack+3

    lda     isoMap7,y
    jsr     push
    bcs     next

    lda     isoMap6,y
    jsr     push
    bcs     next

    lda     isoMap5,y
    jsr     push
    bcs     next

    lda     isoMap4,y
    jsr     push
    bcs     next

    lda     isoMap3,y
    jsr     push
    bcs     next

    lda     isoMap2,y
    jsr     push
    bcs     next

    lda     isoMap1,y
    jsr     push
    bcs     next

    ; if we still have room, always push 0
    lda     isoMap0,y
    sta     stack,x    
    inx
next:
    ; write results
    lda     #0
    sta     isoMap7,y
    sta     isoMap6,y
    sta     isoMap5,y
    sta     isoMap4,y
    lda     stack
    sta     isoMap3,y
    lda     stack+1
    sta     isoMap2,y
    lda     stack+2
    sta     isoMap1,y
    lda     stack+3
    sta     isoMap0,y

    iny
    bne     loop
    rts

push:
    beq     :+
    sta     stack,x    
    inx
    cpx     #3
    bne     :+
    sec
    rts
:
    clc
    rts

stack: .byte   0,0,0,0

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
    lda     #MBG
    sta     isoMap0,y
    iny
    bne     loop
    rts
.endproc

;-----------------------------------------------------------------------------
; Utilies

.include "inline_print.asm"
.include "common_funct.asm"

;-----------------------------------------------------------------------------
; Global
;-----------------------------------------------------------------------------

mapCursor:  .byte   0   ; Offset into map table
macroIndex: .byte   1

;-----------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------

macroOverlay:
    .byte   1       ;   0 Cube   
    .byte   1       ;   1 Grass  
    .byte   1       ;   2 Water  
    .byte   1       ;   3 Tile   
    .byte   1       ;   4 Brick  
    .byte   1       ;   5 Tree   
    .byte   1       ;   6 Reed   
    .byte   0       ;   7 Chair_L
    .res    8

.align 256
; map background (2,4,6)
MBG = $02

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
.byte   $00,$00
.byte   $08,$0a
.byte   $0c,$0e
.byte   $10,$12
.byte   MBG,MBG

; 1 - grass
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $18,$1a
.byte   $1c,$1e
.byte   $20,$22
.byte   MBG,MBG

; 2 - water
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $28,$2a
.byte   $2c,$2e
.byte   $30,$32
.byte   $34,$36

; 3 - tile
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $38,$3a
.byte   $3c,$3e
.byte   $00,$00
.byte   MBG,MBG

; 4 - brick wall
.byte   $00,$00
.byte   $40,$42
.byte   $00,$00
.byte   $00,$00
.byte   $44,$46
.byte   $48,$4a
.byte   $4c,$4e
.byte   MBG,MBG

; 5 - tree
.byte   $58,$5a
.byte   $5c,$5e
.byte   $00,$00
.byte   $00,$00
.byte   $60,$62
.byte   $64,$66
.byte   $20,$22
.byte   MBG,MBG

; 6 - reed
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $68,$6a
.byte   $6c,$6e
.byte   $30,$32
.byte   $34,$36

; 7 - Chair Left
.byte   $00,$00
.byte   $70,$72
.byte   $74,$76
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00

; 8 - Chair Right
.byte   $80,$82
.byte   $84,$86
.byte   $00,$00
.byte   $00,$00
.byte   $88,$8a
.byte   $8c,$8e
.byte   $10,$12
.byte   $14,$16

; 9
.res    16

; 10 - DEL
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   $00,$00
.byte   MBG,MBG



.align 256

isoMap0:
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG
.byte MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG,MBG

isoMap1:
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

isoMap2:
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

isoMap3:
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

isoMap4:
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

isoMap5:
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

isoMap6:
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

isoMap7:
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
