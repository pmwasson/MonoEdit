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

SIZE_28x8           = 0
SIZE_56x16          = 1

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
    ldx     #1
    jsr     setTileSize
    jsr     initMonochrome  ; Turn on monochrome dhgr
    ;jsr     initColorMode

    lda     #0
    sta     clearColor+0
    sta     clearColor+1
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
    jsr     decCurrentTile
    jmp     finishChangeTile
:
    ;------------------
    ; _ = Previous 8
    ;------------------
    cmp     #$80 | '_'
    bne     :+
    jsr     inline_print
    .byte   "Previous 8 tiles: ",0

    ; Really?  Didn't want to make a loop?
    jsr     decCurrentTile
    jsr     decCurrentTile
    jsr     decCurrentTile
    jsr     decCurrentTile
    jsr     decCurrentTile
    jsr     decCurrentTile
    jsr     decCurrentTile
    jsr     decCurrentTile
    jmp     finishChangeTile
:
    ;------------------
    ; = = Next
    ;------------------
    cmp     #$80 | '='
    bne     :+
    jsr     inline_print
    .byte   "Next tile: ",0

    jsr     incCurrentTile
    jmp     finishChangeTile
:
    ;------------------
    ; + = Next 8
    ;------------------
    cmp     #$80 | '+'
    bne     :+
    jsr     inline_print
    .byte   "Next 8 tiles: ",0
    jsr     incCurrentTile
    jsr     incCurrentTile
    jsr     incCurrentTile
    jsr     incCurrentTile
    jsr     incCurrentTile
    jsr     incCurrentTile
    jsr     incCurrentTile
    jsr     incCurrentTile
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
    lda     #PIXEL_BLACK
    sta     lastColor
    jsr     copyPixel
    jsr     drawPreview
    jmp     command_loop
:
    ;------------------
    ; 1 = Set Pixel
    ;------------------
    cmp     #$80 | '1'
    bne     :+
    jsr     inline_print
    StringCR "Set Pixel"
    lda     #PIXEL_WHITE
    sta     lastColor
    jsr     copyPixel
    jsr     drawPreview
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
    lda     #PIXEL_BG_EVEN
    sta     lastColor
    jsr     copyPixel
    jsr     drawPreview
    jmp     command_loop
deletePixelError:
    jsr     inline_print
    StringCR "Error: not in masked mode"
    jmp     command_loop
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
    String "(0=28x8 (unmasked), 1=28x8, 2=56x16):"
    lda     #3
    jsr     getInputNumber
    bmi     tileSizeCancel
    tay
    lda     convertMode,y     
    tax
    lda     convertSize,y
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

convertSize:    .byte   SIZE_28x8, SIZE_28x8, SIZE_56x16
convertMode:    .byte   0,         1,         1

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
    lda     currentTile
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
; inc/dec tile index
;-----------------------------------------------------------------------------

.proc decCurrentTile
    lda     currentTile
    bne     :+
    lda     tileMax
:
    sec
    sbc     tileInc
    sta     currentTile
    rts
.endproc

.proc incCurrentTile
    lda     currentTile
    clc
    adc     tileInc
    cmp     tileMax
    bcc     :+
    lda     #0
:
    sta     currentTile
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
    lda     sizeInc,x
    sta     tileInc
    lda     sizeInc8,x
    sta     tileInc8
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
    asl     tileInc
    asl     tileInc8
:
    ; reset cursor and index
    lda     #0
    sta     curX
    sta     curY
    sta     currentTile

    rts

.align 8
sizeWidth:          .byte   28, 56 
sizeWidthBytes:     .byte   4,  8   
sizeHeight:         .byte   8,  16  
sizeLength:         .byte   32, 128 
sizeCanvasLeft:     .byte   0,  0  
sizeCanvasTop:      .byte   2,  2   
sizeCanvasRight:    .byte   17, 33  
sizeCanvasBottom:   .byte   7,  11  
sizePixelOffsetX:   .byte   2,  2   
sizePixelOffsetY:   .byte   6,  6   

; 6k / 4*8 = 192
sizeMax:        .byte   192, 192
sizeInc:        .byte   1,   4 
sizeInc8:       .byte   8,   16

.endproc

;-----------------------------------------------------------------------------
; drawPreview
;
;-----------------------------------------------------------------------------

.proc drawPreview

    ; title
    lda     #66
    sta     tileX
    lda     #0
    sta     tileY
    jsr     drawString
    String  "Tile:"
    lda     currentTile
    jsr     drawNumber

    lda     modeMasked
    beq     :+
    lda     #74
    sta     tileX
    jsr     drawString
    String  "Masked"
:
    lda     tileSize
    cmp     #SIZE_28x8
    bne     :+
    jmp     drawPreview_28x8
:
    cmp     #SIZE_56x16
    bne     :+
    jmp     drawPreview_56x16
:
    ; Unexpected size
    brk

.endproc

.proc drawPreview_56x16
    lda     #30
    sta     tileX
    lda     #2
    sta     tileY
    lda     currentTile
    jsr     drawTile_56x16

    lda     currentTile
    beq     :+

    sec
    sbc     tileInc
:
    sta     index
    lda     #30
    sta     tileX
    lda     #6
    sta     tileY
    lda     index
    jsr     drawTile_56x16

    lda     #8
    sta     tileY
    lda     index
    clc
    adc     tileInc
    jsr     drawTile_56x16

    lda     #10
    sta     tileY
    lda     index
    clc
    adc     tileInc
    adc     tileInc
    jsr     drawTile_56x16

    rts

index:  .byte   0

.endproc

.proc drawPreview_28x8

    ; erase previous box
    lda     boxRightCopy
    sta     boxRight
    beq     :+              ; skip if not set
    lda     boxBottomCopy
    sta     boxBottom
    beq     :+
    lda     boxTopCopy
    sta     boxTop
    lda     boxLeftCopy
    sta     boxLeft
    jsr     eraseBox
:

    ; Draw screen full of tiles
    lda     currentTile
    and     #$e0
    sta     previewOffset
    lda     modeMasked
    beq     :+
    and     #$c0
    sta     previewOffset
:
    lda     previewOffset
    sta     index

    lda     #2
    sta     tileY

yloop:
    lda     #20+1
    sta     tileX

xloop:
    lda     modeMasked
    beq     :+
    lda     index
    lsr
    tax
    lda     screenShuffleReverse,x
    jsr     drawTile_28x8
    inc     index
    jmp     cont
:
    lda     index
    jsr     drawTile_28x8
cont:
    inc     index

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

    lda     modeMasked
    beq     :+
    lda     currentTile
    lsr
    tax
    lda     screenShuffle,x
    jmp     cont2
:
    lda     currentTile
    sec
    sbc     previewOffset
cont2:
    jsr     setupBox_28x8
    jsr     drawBox

    lda     boxTop
    sta     tileY
    lda     boxLeft
    clc
    adc     #2
    sta     tileX
    lda     currentTile
    jsr     drawNumber

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
    sta     boxTopCopy
    sec
    adc     #1              ; 1 (7x*) tile high
    sta     boxBottom
    sta     boxBottomCopy
    txa

    and     #$3
    asl                     ; %4 * 8
    asl
    asl
    clc
    adc     #41
    sta     boxLeft
    sta     boxLeftCopy
    sec
    adc     #4              ; 4 (7x8) tile wide
    sta     boxRight
    sta     boxRightCopy
    rts

previewOffset:  .byte 0
index:          .byte 0
boxIndex:       .byte 0
boxTopCopy:     .byte 0
boxBottomCopy:  .byte 0
boxLeftCopy:    .byte 0
boxRightCopy:   .byte 0

screenShuffle:  
    .byte   $00, $02, $08, $0a      ; 00 02 04 06
    .byte   $10, $12, $18, $1a      ; 08 0a 0c 0e
    .byte   $20, $22, $28, $2a      ; 10 12 14 16
    .byte   $30, $32, $38, $3a      ; 18 1a 1c 1e
    .byte   $04, $06, $0c, $0e      ; 20 22 24 26
    .byte   $14, $16, $1c, $1e      ; 28 2a 2c 2e
    .byte   $24, $26, $2c, $2e      ; 30 32 34 36
    .byte   $34, $36, $3c, $3e      ; 38 3a 3c 3e

screenShuffleReverse:  
    .byte   $00, $02, $20, $22
    .byte   $04, $06, $24, $26
    .byte   $08, $0a, $28, $2a
    .byte   $0c, $0e, $2c, $2e
    .byte   $10, $12, $30, $32
    .byte   $14, $16, $34, $36
    .byte   $18, $1a, $38, $3a
    .byte   $1c, $1e, $3c, $3e

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


    ; draw bottom of previous tile
    jsr     decCurrentTile
    lda     pixelOffsetY
    sta     tempOffsetY
    sec
    sbc     tileHeight
    sbc     #1
    sta     pixelOffsetY
    lda     #0
    sta     curX
    lda     tileHeight
    sec
    sbc     #4
    sta     curY
    jsr     yloop
    jsr     incCurrentTile
    lda     tempOffsetY
    sta     pixelOffsetY

    ; draw current tile
    lda     #0
    sta     curX
    sta     curY
    jsr     yloop

    ; draw top of next
    jsr     incCurrentTile
    lda     pixelOffsetY
    sta     tempOffsetY
    clc
    adc     tileHeight
    adc     #2
    sta     pixelOffsetY
    lda     #0
    sta     curX
    lda     tileHeight
    sta     tempHeight
    lda     #4
    sta     tileHeight
    lda     #0
    sta     curX
    sta     curY
    jsr     yloop
    jsr     decCurrentTile
    lda     tempOffsetY
    sta     pixelOffsetY
    lda     tempHeight
    sta     tileHeight

    ; restore cursor
    lda     tempX
    sta     curX
    lda     tempY
    sta     curY

    rts

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

    rts

tempX:          .byte   0
tempY:          .byte   0
tempOffsetY:    .byte   0
tempHeight:     .byte   0

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

    ; small pixels
    jsr     getPixelRaw
    tax

    ; set location
    clc
    lda     curY
    adc     pixelOffsetY
    sta     tileY

    clc
    lda     curX
    adc     pixelOffsetX
    sta     tileX

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
    sta     index
    jsr     drawTile_28x8

    inc     tileX
    inc     tileX
    lda     index
    clc
    adc     #2
    jsr     drawTile_28x8

    dec     tileX
    dec     tileX
    inc     tileY
    lda     index
    clc
    adc     #4
    jsr     drawTile_28x8

    inc     tileX
    inc     tileX
    lda     index
    clc
    adc     #6
    jsr     drawTile_28x8

    dec     tileX
    dec     tileX
    dec     tileY

    rts

index:  .byte   0
.endproc

;-----------------------------------------------------------------------------
; drawTile_28x8
;
;   Always starts in AUX memory
;
;-----------------------------------------------------------------------------
.proc drawTile_28x8

    jmp     DHGR_DRAW_28X8

.endproc

;-----------------------------------------------------------------------------
; setTilePointer
;
;   Index passed in A
;-----------------------------------------------------------------------------
.proc setTilePointer
    ; 32 bytes
    tay     ; copy A
    ; calculate tile pointer
    asl                     ; *32
    asl
    asl
    asl
    asl
    sta     tilePtr0
    tya     ; restore A
    lsr                     ; /8
    lsr
    lsr
    clc
    adc     currentSheet_28x8+1
    sta     tilePtr1

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

    lda     currentTile
    sta     tileIdx

    lda     curX
    sta     tileX
    sec
    sbc     #28
    bmi     :+
    sta     tileX
    inc     tileIdx
    inc     tileIdx
:
    lda     curY
    sta     tileY
    sec
    sbc     #8
    bmi     :+
    sta     tileY
    lda     tileIdx
    clc
    adc     #4
    sta     tileIdx
:
    lda     modeMasked
    bne     :+
    jsr     DHGR_GET_PIXEL_MASK_28X8
    and     #1
    rts
:
    ; masked
    jsr     DHGR_GET_PIXEL_MASK_28X8
    and     #2
    bne     :+
    lda     pixelResult
    rts
:
    lda     tileX
    and     #1
    ora     #2
    rts

.endproc

;-----------------------------------------------------------------------------
; Routines to modify pixels
;
; copyPixel     - set to passed in value
; togglePixel   - rotate through colors (black/white or black/white/masked)
; deletePixel   - set to masked
;-----------------------------------------------------------------------------

; Set the pixel at curX,curY to the color passed in A
.proc copyPixel
    sta     color
    lda     currentTile
    jsr     setTilePointer
    jsr     getPixelOffset
    sta     setMask
    eor     #$ff
    sta     clearMask
    sty     baseOffset
    tya
    clc
    adc     #32
    sta     maskOffset

    ; assume masked if asked to set color to BG
    lda     color
    cmp     #PIXEL_BG_EVEN
    beq     setBackground
    cmp     #PIXEL_BG_ODD
    beq     setBackground

    ; if mask mode, set mask for white or black
    ldx     modeMasked
    beq     finishColor

    ; set foreground
    ldy     maskOffset
    lda     clearMask
    and     (tilePtr0),y
    sta     (tilePtr0),y
    lda     color

finishColor:
    cmp     #PIXEL_BLACK
    bne     white
    ldy     baseOffset
    lda     clearMask
    and     (tilePtr0),y
    sta     (tilePtr0),y
    rts    

white:
    ldy     baseOffset
    lda     setMask
    ora     (tilePtr0),y
    sta     (tilePtr0),y
    rts    

setBackground:
    ldy     maskOffset
    lda     setMask
    ora     (tilePtr0),y
    sta     (tilePtr0),y
    ; also clear pixel

clearPixel:
    ldy     baseOffset
    lda     clearMask
    and     (tilePtr0),y
    sta     (tilePtr0),y
    rts

color:          .byte   0
baseOffset:     .byte   0
maskOffset:     .byte   0
setMask:        .byte   0
clearMask:      .byte   0

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

;-----------------------------------------------------------------------------
; getPixelOffset
;   input: curX, curY
;   y = offset to pixel byte in pixel array
;   a = bitmask within byte
;
; Note: mask byte is always +32 from pixel offset
;-----------------------------------------------------------------------------

; Dispatch
.proc getPixelOffset
    lda     tileWidth
    cmp     #28
    beq     getPixelOffset_28
    jmp     getPixelOffset_56
.endproc

.proc getPixelOffset_56
    lda     #0
    sta     offset

    ; check X range (>28)
    lda     curX
    sta     tempX
    sec
    sbc     #28
    bmi     :+
    sta     curX
    lda     #64
    sta     offset
:
    ; check Y range (>8)
    lda     curY
    sta     tempY
    sec
    sbc     #8
    bmi     :+
    sta     curY
    clc
    lda     offset
    adc     #128
    sta     offset
:

    jsr     getPixelOffset_28
    ldx     tempX
    stx     curX
    ldx     tempY
    stx     curY
    pha     ; mask
    tya
    clc
    adc     offset
    tay
    pla     ; restore mask
    rts

offset:     .byte   0
tempX:      .byte   0
tempY:      .byte   0

.endproc

.proc getPixelOffset_28
    ldx     curX
    lda     pixelByteOffset,x
    tax
    lda     pixelInterLeaveOffset,x
    ; plus y multiplied by width in bytes
    ldx     curY
    beq     multY
    clc
:
    adc     #4  ; hard code since used by other size (tileWidthBytes)
    dex
    bne     :-
multY:
    ; a = y * width in bytes + x/7(interleaved)
    tay

    ldx     curX
    lda     pixelByteMask,x
    rts

; 28 bytes
pixelByteOffset:                ; x / 7
    .byte   0,0,0,0,0,0,0
    .byte   1,1,1,1,1,1,1
    .byte   2,2,2,2,2,2,2
    .byte   3,3,3,3,3,3,3

; 4 bytes
pixelInterLeaveOffset:
    .byte   0,2,1,3             ; 0..3

; 28 bytes
pixelByteMask:                  ; 1 << (x % 7)
    .byte   $01,$02,$04,$08,$10,$20,$40
    .byte   $01,$02,$04,$08,$10,$20,$40
    .byte   $01,$02,$04,$08,$10,$20,$40
    .byte   $01,$02,$04,$08,$10,$20,$40

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
    lda     #<tileSheet
    sta     rw_params+2
    lda     #>tileSheet
    sta     rw_params+3

    ; set size
    lda     #<tileSheet_size
    sta     rw_params+4
    lda     #>tileSheet_size
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

; Global Variables
;-----------------------------------------------------------------------------

; Edit

color:              .byte   0
curX:               .byte   0
curY:               .byte   0

currentTile:          .byte   0
tileMax:            .byte   0
tileInc:            .byte   0
tileInc8:           .byte   0
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

currentSheet_28x8:  .word   tileSheet

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

; Tiles
;-----------------------------------------------------------------------------

.align 256

tileSheet_size = tileSheet_end - tileSheet

tileSheet:

;.include "tilesheet_iso.asm"
.res    4096

tileSheet_end:

    .dword  .time   ; Time of compilation