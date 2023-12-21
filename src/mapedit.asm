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

MAP_XOFFSET := 1
MAP_YOFFSET := 1

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

CURSOR_TILE     = $30
CURSOR_RANGE    = MAP_WIDTH*2*MAP_HEIGHT*2
CURSOR_MIN      = 2*MAP_WIDTH*2
CURSOR_MAX      = CURSOR_MIN+CURSOR_RANGE

CURSOR_W        = 256 - 2
CURSOR_E        =       2
CURSOR_N        = 256 - MAP_WIDTH*2*2
CURSOR_S        =       MAP_WIDTH*2*2
CURSOR_NE       = 256 - MAP_WIDTH*2   + 1
CURSOR_NW       = 256 - MAP_WIDTH*2   - 1
CURSOR_SE       =       MAP_WIDTH*2   + 1
CURSOR_SW       =       MAP_WIDTH*2   - 1

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
    lda     #$55
    sta     clearColor+0
    lda     #$2a
    sta     clearColor+1

reset_loop:
    jsr     clearScreen
    jsr     isoDrawMap
    lda     #CURSOR_MIN
    sta     mapCursor

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

assert:
    brk

newCursor:  .byte   0
.endproc

;-----------------------------------------------------------------------------
; printHelp
;-----------------------------------------------------------------------------
.proc printHelp
    bit     TXTSET
    jsr     inline_print
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

    jsr     drawTile

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
; drawTile
;
;-----------------------------------------------------------------------------
.proc drawTile

    sta     CLR80COL        ; Use RAMWRT for aux mem

    jsr     setCoordinate
    lda     mapCursor
    sta     index

    jsr     draw4

    inc     tileX
    inc     tileX
    jsr     draw4

    dec     tileX
    dec     tileX
    inc     tileY
    jsr     draw4

    inc     tileX
    inc     tileX
    jsr     draw4

    rts

draw4:
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
    inc     index
    rts

index:   .byte   0

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
; Utilies

.include "inline_print.asm"
.include "common_funct.asm"

;-----------------------------------------------------------------------------
; Global
;-----------------------------------------------------------------------------

mapCursor:  .byte   0   ; Offset into map table

;-----------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------

.align 256

isoMap0:
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
;
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
;
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
.byte   $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06

isoMap1:
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

isoMap2:
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

isoMap3:
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
