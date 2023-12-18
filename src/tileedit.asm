;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------
; Mono Edit
;  DHGR Monochrom Tile editor

;------------------------------------------------
;------------------------------------------------

.include "defines.asm"
.include "macros.asm"

;------------------------------------------------
; Constants
;------------------------------------------------

CURSOR          = $02

BOX_HORZ        = $13
BOX_VERT        = $7c
BOX_UPPER_LEFT  = $1c
BOX_UPPER_RIGHT = $1d
BOX_LOWER_LEFT  = $1e
BOX_LOWER_RIGHT = $1f
BOX_BLANK       = $20

SIZE_28x8       = 0
SIZE_56x16      = 1
SIZE_56x32      = 2

MODE_NO_MASK    = 0
MODE_MASK       = 1

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
    StringCR    "DHGR Monochrome tile editor - ? for help"

    jsr     DHGR_INIT

    ; set default size
    lda     #SIZE_56x16
    ldx     #MODE_MASK
    jsr     setTileSize
    jsr     initMonochrome  ; Turn on monochrome dhgr
    ;jsr     initColorMode

reset_loop:
    jsr     clearScreen

refresh_loop:
    jsr     drawCanvas
    jsr     drawPreview

command_loop:
    jsr     inline_print
    String  "Command:"

skip_prompt:
    jsr     getInput    ; wait for keypress

    ; Parse command

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
    ;------------------
    ; RIGHT (arrow)
    ;------------------
    cmp     #KEY_RIGHT
    bne     :+
    jsr     inline_print
    .byte   "Right ",0
    inc     curX
    lda     tileWidth
    cmp     curX
    bne     right_good
    lda     #0
    sta     curX
right_good:
    jmp     finish_move
:
    ;------------------
    ; LEFT (arrow)
    ;------------------
    cmp     #KEY_LEFT
    bne     :+
    jsr     inline_print
    .byte   "Left  ",0
    dec     curX
    lda     curX
    bpl     left_good
    lda     tileWidth
    sta     curX
    dec     curX
left_good:
    jmp     finish_move
:
    ;------------------
    ; UP (arrow)
    ;------------------
    cmp     #KEY_UP
    bne     :+
    jsr     inline_print
    .byte   "Up    ",0
    dec     curY
    lda     curY
    bpl     up_good
    lda     tileHeight
    sta     curY
    dec     curY
up_good:
    jmp     finish_move
:
    ;------------------
    ; DOWN (arrow)
    ;------------------
    cmp     #KEY_DOWN
    bne     :+
    jsr     inline_print
    .byte   "Down  ",0
    inc     curY
    lda     tileHeight
    cmp     curY
    bne     down_good
    lda     #0
    sta     curY
down_good:
    jmp     finish_move
:
    ;------------------
    ; - = Previous
    ;------------------
    cmp     #$80 | '-'
    bne     :+
    jsr     inline_print
    .byte   "Previous tile: ",0

    lda     tileIndex
    bne     previous_continue
    lda     tileMax
previous_continue:
    clc
    sbc     modeMasked  ; -1 or -2 if masked
    jmp     finishChangeTile
:
    ;------------------
    ; _ = Previous 8
    ;------------------
    cmp     #$80 | '_'
    bne     :+
    jsr     inline_print
    .byte   "Previous 8 tiles: ",0

    lda     tileIndex
    sec     
    sbc     #8
    bpl     previous8_continue
    clc
    adc     tileMax
previous8_continue:
    jmp     finishChangeTile
:
    ;------------------
    ; = = Next
    ;------------------
    cmp     #$80 | '='
    bne     :+
    jsr     inline_print
    .byte   "Next tile: ",0

    lda     tileIndex
    sec
    adc     modeMasked  ; +1 or +2 if masked
    cmp     tileMax
    bne     next_continue
    lda     #0
next_continue:
    jmp     finishChangeTile
:
    ;------------------
    ; + = Next 8
    ;------------------
    cmp     #$80 | '+'
    bne     :+
    jsr     inline_print
    .byte   "Next 8 tiles: ",0

    lda     tileIndex
    clc
    adc     #8
    cmp     tileMax
    bmi     next_continue8
    sec
    sbc     tileMax
next_continue8:
    jmp     finishChangeTile
:
    ;------------------
    ; SP = Toggle Pixel
    ;------------------
    cmp     #KEY_SPACE
    bne     :+
    jsr     inline_print
    StringCR "Toggle Pixel"
    jsr     togglePixel
    jsr     drawPreview
    jmp     command_loop
:
    ;------------------
    ; 0 = Clear Pixel
    ;------------------
    cmp     #$80 | '0'
    bne     :+
    jsr     inline_print
    StringCR "Clear Pixel"
    jsr     clearPixel
    jsr     drawPreview
    lda     #PIXEL_BLACK
    sta     lastColor
    jmp     command_loop
:
    ;------------------
    ; 1 = Set Pixel
    ;------------------
    cmp     #$80 | '1'
    bne     :+
    jsr     inline_print
    StringCR "Set Pixel"
    jsr     setPixel
    jsr     drawPreview
    lda     #PIXEL_WHITE
    sta     lastColor
    jmp     command_loop
:
    ;------------------
    ; 2 = Delete Pixel
    ;------------------
    cmp     #$80 | '2'
    bne     :+
    jsr     inline_print
    StringCR "Delete Pixel"
    lda     modeMasked
    beq     deletePixelError
    jsr     deletePixel
    jsr     drawPreview
    lda     #PIXEL_BG_EVEN
    sta     lastColor
    jmp     command_loop
deletePixelError:
    jsr     inline_print
    StringCR "Error: not in masked mode"
    jmp     command_loop
:
    ;------------------
    ; ^M = Toggle Mask
    ;------------------
    cmp     #KEY_RETURN
    bne     :+
    jsr     inline_print
    String  "Mask "
    lda     modeMasked
    beq     setModeMasked
    jsr     inline_print
    StringCR "Off"
    ldx     #0
    jmp     finishModeMask
setModeMasked:
    jsr     inline_print
    StringCR "On"
    ldx     #1
finishModeMask:
    lda     tileSize
    jsr     setTileSize
    lda     tileIndex
    and     #$FE
    sta     tileIndex
    jmp     refresh_loop
:
    ;------------------
    ; ^C = Copy Tile
    ;------------------
    cmp     #KEY_CTRL_C
    bne     :+
    jsr     inline_print
    StringCR "Copy tile to clipboard"
    jsr     copyTile
    jmp     command_loop
:
    ;------------------
    ; ^V = Paste Tile
    ;------------------
    cmp     #KEY_CTRL_V
    bne     :+
    jsr     inline_print
    StringCR "Paste tile from clipboard"
    jsr     pasteTile
    jmp     refresh_loop
:

    ;------------------
    ; ^F = Fill Color
    ;------------------
    cmp     #KEY_CTRL_F
    bne     :+
    jsr     inline_print


    .byte   "Fill Color. Pick color (0=black 1=white, 2=background):",0
    clc
    lda     modeMasked
    adc     #2
    jsr     getInputNumber
    bmi     fill_cancel
    jsr     fillPixels
    lda     #13
    jsr     COUT
    jmp     refresh_loop
fill_cancel:
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
    ; ^A = Alternate colors
    ;------------------
    cmp     #KEY_CTRL_A
    bne     :+
    jsr     inline_print
    StringCR    "Invert"
    jsr     invertPixels
    jmp     refresh_loop
:

    ;------------------
    ; ^T = Tile Size
    ;------------------
    cmp     #KEY_CTRL_T
    beq     :+
    jmp     afterTileSize
:
    jsr     inline_print
    StringCont "Tile size"
    String "(0=28x8, 1=56x16, 2=56x32):"
    lda     #3
    jsr     getInputNumber
    bmi     tileSizeCancel
    ldx     modeMasked
    jsr     setTileSize
    ; output size
    jsr     inline_print
    String  ": "
    lda     tileWidth
    jsr     PRBYTE
    lda     #$80 + 'x'
    jsr     COUT
    lda     tileHeight
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     reset_loop
tileSizeCancel:
    jmp     command_loop

afterTileSize:

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
    ; ^X = eXchange (flip)
    ;------------------
    cmp     #KEY_CTRL_X
    bne     flip_after
    jsr     inline_print
    .byte   "Exchange (flip) Direction (or cancel):",0
    jsr     getInputDirection
    beq     flip_cancel

    cmp     #KEY_UP
    beq     do_flip_vert
    cmp     #KEY_DOWN
    bne     :+
do_flip_vert:
    jsr     flipVert
    jmp     refresh_loop
:
    ; must be horizontal
    jsr     flipHor
    jmp     refresh_loop
flip_cancel:
    jmp     command_loop

flip_after:

    ;------------------
    ; ! = Dump
    ;------------------
    cmp     #$80 + '!' 
    bne     :+
    bit     TXTSET
    jsr     inline_print
    .byte   "Dump Tile ",0
    lda     tileIndex
    jsr     PRBYTE
    jsr     inline_print
    .byte   " (ESC when done) ",13,0
    jsr     printDump
    jmp     command_loop
:

    ;------------------
    ; ^P = Print
    ;------------------
    cmp     #KEY_CTRL_P
    bne     :+
    bit     TXTSET
    jsr     inline_print
    StringCR   "Print all"
    jsr     printAll
    jmp     command_loop
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
    ; I = Iso Map preview
    ;------------------
    cmp     #$80 + 'I' 
    bne     :+
    jsr     inline_print
    .byte   "Drawing isometric map (press any key to exit)",0

    jsr     clearScreen
    jsr     isoDrawMap
    jsr     getInput
    lda     #13
    jsr     COUT
    jmp     reset_loop
:

    ;------------------
    ; ^L = Load
    ;------------------
    cmp     #KEY_CTRL_L
    bne     :+
    jsr     inline_print
    .byte   "Read slot (0-4):",0
    lda     #5
    jsr     getInputNumber
    bmi     load_exit
    jsr     loadSheet

    ; redraw the screen
    jmp     reset_loop

load_exit:
    jmp     command_loop
:    

    ;------------------
    ; ^S = Save
    ;------------------
    cmp     #KEY_CTRL_S
    bne     :+
    jsr     inline_print
    .byte   "Save slot (0-4):",0
    lda     #5
    jsr     getInputNumber
    bmi     save_exit
    jsr     saveSheet

save_exit:
    jmp     command_loop
: 

    ;------------------
    ; Unknown
    ;------------------
    jsr     inline_print
    .byte   "Unknown command (? for help)",13,0
    jmp     command_loop

; jump to after changing coordinates to display
finish_move:
    jsr     inline_print
    .byte   "X/Y:",0
    lda     curX
    jsr     PRBYTE
    lda     #$80 + ','
    jsr     COUT
    lda     curY
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    ; check for apple keys
    lda     BUTTON0
    bpl     :+
    jsr     inline_print
    StringCR    "Repeat last color"
    lda     lastColor
    jsr     copyPixel
    jsr     drawPreview
:
    jmp     command_loop

; jump to after changing tile
finishChangeTile:
    sta     tileIndex
    lda     modeMasked
    beq     :+
    lda     tileIndex
    lsr
    jmp     finishChangeTile_cont    
:
    lda     tileIndex
finishChangeTile_cont:
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     refresh_loop

.endproc




;-----------------------------------------------------------------------------
; printHelp
;-----------------------------------------------------------------------------
.proc printHelp
    bit     TXTSET
    jsr     inline_print
    StringCont  "  Arrows:  Move cursor (open-apple will set last color after move)"
    StringCont  "  0:       Clear pixel"
    StringCont  "  1:       Set pixel"
    StringCont  "  2:       Delete pixel (masked mode)"    
    StringCont  "  Space:   Toggle pixel"
    StringCont  "  Ctrl-F:  Fill tile with selected color"
    StringCont  "  Ctrl-C:  Copy tile to clipboard"
    StringCont  "  Ctrl-V:  Paste tile from clipboard (overwrites current tile)"
    StringCont  "  Ctrl-R:  Rotate pixels in a direction specified by an arrow key"
    StringCont  "  Ctrl-X:  eXchange pixels in a direction specified by an arrow key"
    StringCont  "  Ctrl-A:  Alternate colors (black <-> white)"    
    StringCont  "  Ctrl-T:  Set tile size"
    StringCont  "  Ctrl-M:  Toggle between normal and masked mode"
    StringCont  "  -,=:     Go to previous/next tile (holding shift moves 8 tile)"
    StringCont  "  Ctrl-L:  Load tilesheet"
    StringCont  "  Ctrl-S:  Save tilesheet"
    StringCont  "  !:       Dump bytes"
    StringCont  "  Ctrl-P:  Print all tiles (do a 1^P in monitor first!)"
    StringCont  "  ?:       This help screen"
    StringCont  "  \:       Monitor"
    StringCont  "  Ctrl-Q:  Quit"
    StringCont  "  Escape:  Toggle text/graphics"
    StringCont  "  Tab:     Switch Tools"
    .byte   0

    rts
.endproc

;-----------------------------------------------------------------------------
; setTileSize
;   size passed in A, mask mode in X
;-----------------------------------------------------------------------------
.proc setTileSize
    stx     modeMasked
    sta     tileSize
    tax
    lda     sizeWidth,x
    sta     tileWidth
    lda     sizeWidthBytes,x
    sta     tileWidthBytes
    lda     sizeHeight,x
    sta     tileHeight
    lda     sizeLength,x
    sta     tileLength
    lda     sizeMax,x
    sta     tileMax
    lda     sizeCanvasLeft,x
    sta     canvasLeft
    lda     sizeCanvasTop,x
    sta     canvasTop
    lda     sizeCanvasRight,x
    sta     canvasRight
    lda     sizeCanvasBottom,x
    sta     canvasBottom
    lda     sizePixelOffsetX,x
    sta     pixelOffsetX
    lda     sizePixelOffsetY,x
    sta     pixelOffsetY

    ; adjust tilelength for masked mode
    lda     modeMasked
    beq     :+
    asl     tileLength
:

    ; check that cursor is still in range
    lda     tileWidth
    cmp     curX
    bpl     :+
    lda     tileWidth
    sta     curX
    dec     curX
:     
    lda     tileHeight
    cmp     curY
    bpl     :+
    lda     tileHeight
    sta     curY
    dec     curY
:

    ; check that tile index is still in range
    lda     tileMax
    cmp     tileIndex
    bpl     :+
    lda     #0
    sta     tileIndex
:

    rts

.align 8
sizeWidth:          .byte   28, 56,  56
sizeWidthBytes:     .byte   4,  8,   8
sizeHeight:         .byte   8,  16,  32
sizeLength:         .byte   32, 128, 0
sizeCanvasLeft:     .byte   0,  0,   0
sizeCanvasTop:      .byte   0,  0,   0   
sizeCanvasRight:    .byte   17, 33,  66
sizeCanvasBottom:   .byte   5,  9,   9
sizePixelOffsetX:   .byte   2,  2,   2
sizePixelOffsetY:   .byte   2,  2,   2

; Tile max should be <= 4K
; size   bytes/tile  4k/x    max    
; 28x8   32          128     64
; 56x16  128         32      32
; 56x32  256         16      16

sizeMax:        .byte   64, 32, 16
.endproc

;-----------------------------------------------------------------------------
; drawPreview
;
;-----------------------------------------------------------------------------

.proc drawPreview

    ; title
    lda     #72
    sta     tileX
    lda     #0
    sta     tileY
    jsr     drawString
    String  "Tile:"
    lda     tileIndex
cont:
    jsr     drawNumber

    lda     tileSize
    cmp     #SIZE_28x8
    bne     :+
    jmp     drawPreview_28x8
:
    cmp     #SIZE_56x16
    bne     :+
    jmp     drawPreview_56x16
:
    ; Assuming only 56x32 left
    ;jmp     drawPreview_56x32
    brk

.endproc

.proc drawPreview_56x16
    lda     #30
    sta     tileX
    lda     #2
    sta     tileY
    lda     tileIndex
    jsr     drawTile_56x16


    lda     #30
    sta     tileX
    lda     #6
    sta     tileY
    lda     tileIndex
    jsr     drawTile_56x16

    lda     #34
    sta     tileX
    lda     tileIndex
    jsr     drawTile_56x16
 
    lda     #30
    sta     tileX
    lda     #8
    sta     tileY
    lda     tileIndex
    jsr     drawTile_56x16

    lda     #34
    sta     tileX
    lda     tileIndex
    jsr     drawTile_56x16
 
    rts
.endproc

.proc drawPreview_28x8

    ; erase previous box
    lda     prevIndex
    jsr     setupBox_28x8
    jsr     eraseBox

    ; Draw all tiles
    lda     #0
    sta     index

    lda     #2
    sta     tileY

yloop:
    lda     #20+1
    sta     tileX

xloop:
    lda     index
    jsr     drawTile_28x8

    lda     index
    sec
    adc     modeMasked
    sta     index

    lda     tileX
    clc
    adc     #4
    sta     tileX
    cmp     #20+1+4*4
    bne     xloop

    inc     tileY
    inc     tileY
    lda     tileY
    cmp     #2+8*2          ; 8 high
    bne     yloop 

    lda     tileIndex
    sta     prevIndex
    jsr     setupBox_28x8
    jsr     drawBox

    rts

setupBox_28x8:
    sta     boxIndex
    lda     modeMasked
    beq     :+
    lsr     boxIndex
:
    lda     boxIndex
    tax
    ; Draw box around current tile
    lsr
    and     #$fe            ; /4 * 2 -> /2 and mask bit 0 
    clc
    adc     #1 
    sta     boxTop
    sec
    adc     #1              ; 1 (7x*) tile high
    sta     boxBottom
    txa

    and     #$3
    asl                     ; %4 * 8
    asl
    asl
    clc
    adc     #41
    sta     boxLeft
    sec
    adc     #4              ; 4 (7x8) tile wide
    sta     boxRight
    rts

index:      .byte 0
prevIndex:  .byte 0
boxIndex:   .byte 0

.endproc

;-----------------------------------------------------------------------------
; DrawCanvas
;
;-----------------------------------------------------------------------------
.proc drawCanvas

    lda     canvasLeft
    sta     boxLeft
    lda     canvasTop
    sta     boxTop
    lda     canvasRight
    sta     boxRight
    lda     canvasBottom
    sta     boxBottom
    jsr     drawBox

    ; save cursor
    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curY

yloop:
    lda     #0
    sta     curX
xloop:
    jsr     drawPixel

    inc     curX
    lda     curX
    cmp     tileWidth
    bne     xloop

    inc     curY
    lda     curY
    cmp     tileHeight
    bne     yloop

    ; restore cursor
    lda     tempX
    sta     curX
    lda     tempY
    sta     curY

    rts

tempX:  .byte   0
tempY:  .byte   0

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

    jsr     drawPixel

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
; DrawPixel
;   Based on curX, curY and color
;-----------------------------------------------------------------------------

.proc drawPixel

    clc

    ; set location
    lda     curY
    adc     pixelOffsetY
    sta     tileY

    lda     curX
    adc     pixelOffsetX
    sta     tileX

    ; small pixels
    jsr     getPixelRaw
    tax
    jsr     DHGR_DRAW_PIXEL_4X4
    rts

.endproc

;-----------------------------------------------------------------------------
; DrawCursor
;   Based on curX, curY
;-----------------------------------------------------------------------------
.proc drawCursor

    ; set location
    lda     curY
    clc
    adc     pixelOffsetY
    sta     tileY

    lda     curX
    clc
    adc     pixelOffsetX
    sta     tileX

    ldx     #4
    jsr     DHGR_DRAW_PIXEL_4X4
    rts

.endproc

;-----------------------------------------------------------------------------
; Draw Tile (56x16)
;-----------------------------------------------------------------------------

.proc drawTile_56x16

    ; tile index passes in A
    jsr     setTilePointer_56x16

    sta     CLR80COL        ; Use RAMWRT for aux mem (needed after COUT)

    ; calculate screen pointer
    ldx     tileY
    lda     tileX           ; interpreting x as x*2
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    lda     linePage,x
    sta     screenPtr1

    clc     ; no carry generated inside of loop
    ldx     #8

drawLoop:
    ; Byte 0..3 in AUX memory
    sta     RAMWRTON
    ldy     #0
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (bgPtr0),y
    sta     (screenPtr0),y

    ; Bytes 4..7 in MAIN memory
    lda     bgPtr0
    adc     #4
    sta     bgPtr0
    sta     RAMWRTOFF
    ldy     #0
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    lda     bgPtr0
    adc     #4
    sta     bgPtr0

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    bne     drawLoop


    ; move to second half
    lda     screenPtr0
    clc
    adc     #$80
    sta     screenPtr0
    lda     screenPtr1
    sbc     #$1f        ; subtract 20 if no carry, 19 if carry
    sta     screenPtr1

    clc     ; no carry generated inside of loop
    ldx     #8

drawLoop2:
    ; Byte 0..3 in AUX memory
    sta     RAMWRTON
    ldy     #0
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (bgPtr0),y
    sta     (screenPtr0),y

    ; Bytes 4..7 in MAIN memory
    lda     bgPtr0
    adc     #4
    sta     bgPtr0
    sta     RAMWRTOFF
    ldy     #0
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    lda     bgPtr0
    adc     #4
    sta     bgPtr0

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    bne     drawLoop2

    rts    

.endproc

;-----------------------------------------------------------------------------
; drawTile_28x8
;
;   Always starts in AUX memory
;
;-----------------------------------------------------------------------------
.proc drawTile_28x8

    ; tile index passes in A
    jsr     setTilePointer_28x8

    sta     CLR80COL        ; Use RAMWRT for aux mem (needed after COUT)

    ; calculate screen pointer
    ldx     tileY
    lda     tileX           ; index by half - tiles in X
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    lda     linePage,x
    sta     screenPtr1

    clc     ; no carry generated inside of loop
    ldx     #8

drawLoop:
    ; Byte 0,1 in AUX memory
    sta     RAMWRTON
    ldy     #0
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (bgPtr0),y
    sta     (screenPtr0),y

    ; Bytes 2,3 in MAIN memory
    inc     bgPtr0
    inc     bgPtr0
    sta     RAMWRTOFF
    ldy     #0
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    inc     bgPtr0
    inc     bgPtr0

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    bne     drawLoop

    rts    

.endproc

;-----------------------------------------------------------------------------
; setTilePointer
;
;   Index passed in A
;-----------------------------------------------------------------------------
.proc setTilePointer

    ; Dispatch
    ldy     tileSize
    cpy     #SIZE_28x8
    bne     :+
    jmp     setTilePointer_28x8
:   
    cpy     #SIZE_56x16
    bne     :+
    jmp     setTilePointer_56x16
:
    brk

.endproc

.proc setTilePointer_28x8
    ; 32 bytes
    tay     ; copy A
    ; calculate tile pointer
    asl                     ; *32
    asl
    asl
    asl
    asl
    sta     bgPtr0
    tya     ; restore A
    lsr                     ; /8
    lsr
    lsr
    clc
    adc     currentSheet_28x8+1
    sta     bgPtr1

    rts

.endproc

.proc setTilePointer_56x16
    ; 128 bytes
    tay     ; copy A
    ; calculate tile pointer
    asl                     ; *128
    asl
    asl
    asl
    asl
    asl
    asl
    sta     bgPtr0
    tya     ; restore A
    lsr                     ; /2
    clc
    adc     currentSheet_56x16+1
    sta     bgPtr1

    rts

.endproc

;-----------------------------------------------------------------------------
; getPixel
;
;  Read single pixel from tile returned in A using curX and curY
;
;-----------------------------------------------------------------------------

.proc getPixel
    jsr     getPixelRaw
    tax
    lda     colorChar,x
    rts

colorChar:  .byte PIXEL_BLACK,PIXEL_WHITE,PIXEL_BG_EVEN,PIXEL_BG_ODD

.endproc


; Return 0(black), 1(white) or 2(masked)
.proc getPixelRaw
    inc     tileIndex
    lda     modeMasked
    beq     :+
    lda     tileIndex
    jsr     setTilePointer
    jsr     getPixelOffset
    and     (bgPtr0),y
    beq     :+
    dec     tileIndex
    lda     tileX
    and     #1
    ora     #2
    rts
:
    dec     tileIndex
    lda     tileIndex
    jsr     setTilePointer
    jsr     getPixelOffset
    and     (bgPtr0),y
    beq     :+  ; 0 = black
    lda     #1  ; 1 = white
:
    rts
.endproc

;-----------------------------------------------------------------------------
; Routines to modify pixels
;
; clearPixel    - set to black
; setPixel      - set to white
; copyPixel     - set to passed in value
; togglePixel   - rotate through colors (black/white or black/white/masked)
; deletePixel   - set to masked
;-----------------------------------------------------------------------------

.proc clearPixel
    lda     modeMasked
    beq     clearPixel1

    ; When in masked mode, clear the mask also
    inc     tileIndex
    jsr     clearPixel1
    dec     tileIndex

clearPixel1:
    lda     tileIndex
    jsr     setTilePointer
    jsr     getPixelOffset
    eor     #$ff
    and     (bgPtr0),y
    sta     (bgPtr0),y
    rts
.endproc

.proc setPixel
    lda     modeMasked
    beq     setPixel1

    ; When in masked mode, clear the mask also
    inc     tileIndex
    jsr     clearPixel::clearPixel1
    dec     tileIndex

setPixel1:
    lda     tileIndex
    jsr     setTilePointer
    jsr     getPixelOffset
    ora     (bgPtr0),y
    sta     (bgPtr0),y
    rts
.endproc

.proc copyPixel
    cmp     #PIXEL_BLACK
    beq     clearPixel
    cmp     #PIXEL_WHITE
    beq     setPixel
    jmp     deletePixel
.endproc

; Masked black -> white -> background -> black ...
; Normal black -> white -> black ...
.proc togglePixel
    lda     modeMasked
    beq     :+
    jsr     getPixel
    cmp     #PIXEL_WHITE
    bne     :+
    lda     #PIXEL_BG_EVEN
    jmp     finish
:    
    jsr     getPixel
    cmp     #PIXEL_BLACK
    bne     :+
    lda     #PIXEL_WHITE
    jmp     finish
:
    lda     #PIXEL_BLACK
finish:
    sta     lastColor
    jmp     copyPixel
.endproc

.proc deletePixel
    ; Assuming in modeMasked, so not checking
    ; Clearing foreground also to keep clean, but may not want to
    jsr     clearPixel::clearPixel1
    inc     tileIndex
    jsr     setPixel::setPixel1
    dec     tileIndex
    rts
.endproc

;-----------------------------------------------------------------------------
; getPixelOffset
;   input: curX, curY
;   y = offset to byte in pixel array
;   a = bitmask within byte
;-----------------------------------------------------------------------------

.proc getPixelOffset

    clc
    ldx     curX
    lda     pixelByteOffset,x
    adc     tileWidthBytes
    tax     
    lda     pixelInterLeaveOffset,x

    ; multiply y by width in bytes
    ldx     curY
    beq     multY
:
    adc     tileWidthBytes
    dex
    bne     :-
multY:
    ; a = y * width in bytes + x/7(interleaved)
    tay

    ldx     curX
    lda     pixelByteMask,x
    rts
.endproc


;-----------------------------------------------------------------------------
; Set file params
;
;   A = file number
;-----------------------------------------------------------------------------
.proc setFileParams

    ; set filename
    ;--------------------------
    clc
    adc     #'0'
    sta     pathname_tilesheet_end-1

    ; set pathname
    lda     #<pathname_tilesheet
    sta     open_params+1
    sta     create_params+1
    sta     stringPtr0
    lda     #>pathname_tilesheet
    sta     open_params+2
    sta     create_params+2
    sta     stringPtr1

    ; set address
    lda     #<tileSheet_4k
    sta     rw_params+2
    lda     #>tileSheet_4k
    sta     rw_params+3

    ; set size
    lda     #<tileSheet_4k_size
    sta     rw_params+4
    lda     #>tileSheet_4k_size
    sta     rw_params+5

    lda     #':' + $80
    jsr     COUT
    jsr     print_length
    lda     #13
    jsr     COUT

    rts

.endproc

;-----------------------------------------------------------------------------
; Utilies

.include "edit_funct.asm"
.include "inline_print.asm"
.include "iso.asm"

; Global Variables
;-----------------------------------------------------------------------------

; Edit

color:              .byte   0
curX:               .byte   0
curY:               .byte   0

tileIndex:          .byte   0
tileMax:            .byte   0
tileSize:           .byte   0
tileWidth:          .byte   0
tileWidthBytes:     .byte   0
tileHeight:         .byte   0
tileLength:         .byte   0
canvasLeft:         .byte   0
canvasTop:          .byte   0
canvasRight:        .byte   0
canvasBottom:       .byte   0
pixelOffsetX:       .byte   0
pixelOffsetY:       .byte   0

modeMasked:         .byte   1
lastColor:          .byte   PIXEL_WHITE

; General

currentSheet_28x8:  .word   tileSheet_4k
currentSheet_56x16: .word   tileSheet_4k

; ProDos pathname

pathname_tilesheet:
    StringLen "/DHGR/DATA/TILESHEET.0"
pathname_tilesheet_end:

; Lookup tables
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

.align      128

; 56 bytes
pixelByteOffset:                ; x / 7
    .byte   0,0,0,0,0,0,0
    .byte   1,1,1,1,1,1,1
    .byte   2,2,2,2,2,2,2
    .byte   3,3,3,3,3,3,3
    .byte   4,4,4,4,4,4,4
    .byte   5,5,5,5,5,5,5
    .byte   6,6,6,6,6,6,6
    .byte   7,7,7,7,7,7,7

; 16 bytes
pixelInterLeaveOffset:
    .byte   255                 ; 0:     not used
    .byte   0                   ; 1:     size 1 + (0)
    .byte   0,1                 ; 2..3:  size 2 + (0..1)
    .byte   0,2,1,3             ; 4..7:  size 4 + (0..3)
    .byte   0,4,1,5,2,6,3,7     ; 8..15: size 8 + (0..7)

; 56 bytes
pixelByteMask:                  ; 1 << (x % 7)
    .byte   $01,$02,$04,$08,$10,$20,$40
    .byte   $01,$02,$04,$08,$10,$20,$40
    .byte   $01,$02,$04,$08,$10,$20,$40
    .byte   $01,$02,$04,$08,$10,$20,$40
    .byte   $01,$02,$04,$08,$10,$20,$40
    .byte   $01,$02,$04,$08,$10,$20,$40
    .byte   $01,$02,$04,$08,$10,$20,$40
    .byte   $01,$02,$04,$08,$10,$20,$40

; Tiles
;-----------------------------------------------------------------------------

.align 256

tileSheet_4k_size = tileSheet_4k_end - tileSheet_4k

tileSheet_4k:
.include "tilesheet_iso.asm"
tileSheet_4k_end:

    .dword  .time   ; Time of compilation