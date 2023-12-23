;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------
; Font Edit
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

SIZE_7x8        = 0

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
    StringCR    "DHGR font editor - ? for help"

    jsr     DHGR_INIT

    jsr     initMonochrome  ; Turn on monochrome dhgr

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

    lda     tileIndex
    bne     previous_continue
    lda     tileMax
previous_continue:
    sec
    sbc     #1     
    jmp     finishChangeTile
:
    ;------------------
    ; _ = Previous 16
    ;------------------
    cmp     #$80 | '_'
    bne     :+
    jsr     inline_print
    .byte   "Previous 16 tiles: ",0

    lda     tileIndex
    sec     
    sbc     #16
    bpl     previous16_continue
    clc
    adc     tileMax
previous16_continue:
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
    clc
    adc     #1
    cmp     tileMax
    bne     next_continue
    lda     #0
next_continue:
    jmp     finishChangeTile
:
    ;------------------
    ; + = Next 16
    ;------------------
    cmp     #$80 | '+'
    bne     :+
    jsr     inline_print
    .byte   "Next 16 tiles: ",0

    lda     tileIndex
    clc
    adc     #16
    cmp     tileMax
    bmi     next_continue16
    sec
    sbc     tileMax
next_continue16:
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
    .byte   "Fill Color. Pick color (0=black 1=white):",0
    lda     #2
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
    jmp     $6000       ; Maybe should look into the linker
:

    ;------------------
    ; ^A = Alternate colors
    ;------------------
    cmp     #KEY_CTRL_A
    bne     :+
    jsr     inline_print
    StringCR    "Alternate"
    jsr     invertPixels
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
    ; ^T = Test
    ;------------------
    cmp     #KEY_CTRL_T
    bne     :+
    jsr     inline_print
    StringCR   "Test font"
    jsr     fontTest
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
    StringCont  "  Space:   Toggle pixel"
    StringCont  "  Ctrl-F:  Fill tile with selected color"
    StringCont  "  Ctrl-C:  Copy tile to clipboard"
    StringCont  "  Ctrl-V:  Paste tile from clipboard (overwrites current tile)"
    StringCont  "  Ctrl-R:  Rotate pixels in a direction specified by an arrow key"
    StringCont  "  Ctrl-X:  eXchange pixels in a direction specified by an arrow key"
    StringCont  "  Ctrl-A:  Alternate colors (black <-> white)"    
    StringCont  "  -,=:     Go to previous/next tile (holding shift moves 16 tile)"
    StringCont  "  Ctrl-L:  Load font"
    StringCont  "  Ctrl-S:  Save font"
    StringCont  "  !:       Dump bytes of this tile"
    StringCont  "  Ctrl-P:  Print all tiles (do a 1^P in monitor first, 3^P after!)"
    StringCont  "  Ctrl-T:  Display input text to test font"
    StringCont  "  ?:       This help screen"
    StringCont  "  \:       Monitor"
    StringCont  "  Ctrl-Q:  Quit"
    StringCont  "  Escape:  Toggle text/graphics"
    StringCont  "  Tab:     Switch Tools"
    .byte   0

    rts
.endproc

;-----------------------------------------------------------------------------
; fontTest 
;
;-----------------------------------------------------------------------------

.proc fontTest

    jsr     inline_print
    .byte   "Enter text to display",0
    lda     #':' | $80
    sta     PROMPT
    jsr     GETLN

    ; pad out with spaces
:
    lda     #$20
    sta     $200,x
    inx     
    cpx     #80
    bcc     :-

    lda     #19
    sta     tileY

    lda     #0
    sta     tileX
    sta     offset
:
    ldx     offset
    lda     $200,x
    and     #$7f
    jsr     drawTile_7x8
    inc     tileX
    inc     offset
    lda     offset
    cmp     #80
    bcc     :-

    rts

offset:     .byte   0

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
    jsr     drawNumber
    jmp     drawPreview_7x8
    
.endproc

.proc drawPreview_7x8

    ; erase previous box
    lda     prevIndex
    jsr     setupBox_7x8
    jsr     eraseBox

    ; Draw all tiles
    lda     #0
    sta     index

    lda     #2
    sta     tileY

yloop:
    lda     #40+1
    sta     tileX

xloop:
    lda     index
    jsr     drawTile_7x8
    inc     index

    inc     tileX
    inc     tileX
    lda     tileX
    cmp     #40+1+16*2
    bne     xloop

    inc     tileY
    inc     tileY
    lda     tileY
    cmp     #2+8*2          ; 8 high
    bne     yloop 

    lda     tileIndex
    sta     prevIndex
    jsr     setupBox_7x8
    jsr     drawBox

    rts

setupBox_7x8:
    tax
    ; Draw box around current tile
    lsr
    lsr                    
    lsr
    and     #$fe            ; /16 * 2 -> /8 and mask bit 0
    clc
    adc     #1       
    sta     boxTop
    sec
    adc     #1              ; 1 (7x*) tile high
    sta     boxBottom
    txa

    and     #$f
    asl                     ; %16 * 2
    clc
    adc     #40
    sta     boxLeft
    sec
    adc     #1              ; 1 (7x8) tile wide
    sta     boxRight
    rts


index:      .byte 0
prevIndex:  .byte 0

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

; 7x8 pixel
.proc drawPixel

    clc

    ; set location
    lda     curY
    adc     pixelOffsetY
    sta     tileY

    lda     curX
    adc     pixelOffsetX
    sta     tileX

    lda     tilePixelSize
    bne     :+

    ; large pixels
    jsr     getPixel
    jsr     DHGR_DRAW_7X8
    rts
:
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

    lda     tilePixelSize
    bne     :+
    lda     #CURSOR
    jsr     DHGR_DRAW_7X8
    rts

:
    ldx     #4
    jsr     DHGR_DRAW_PIXEL_4X4
    rts

.endproc

;-----------------------------------------------------------------------------
; drawTile_7x8
;  Draw a tile that is 7 pixels wide (1 byte) by 8 pixels high, for a total
;    of 8 bytes.
; Can be either in aux (even) or main (odd) memory depending on X.
;  Assume 7x8, where 7 is 7*4 pixels = 28 -> 4 bytes
;
;-----------------------------------------------------------------------------
.proc drawTile_7x8

    ; tile index passes in A
    jsr     setTilePointer

    sta     CLR80COL        ; Use RAMWRT for aux mem (needed after COUT)

    ; calculate screen pointer
    ldx     tileY
    lda     tileX
    lsr                     ; /2
    bcs     :+              ; odd = main mem
    sta     RAMWRTON        ; aux if even
:
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    lda     linePage,x
    sta     screenPtr1

    clc     ; no carry generated inside of loop
    ldx     #8
    ldy     #0
drawLoop:
    lda     (tilePtr0),y
    sta     (screenPtr0),y

    ; assumes aligned such that there are no page crossing
    inc     tilePtr0

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    bne     drawLoop

    sta     RAMWRTOFF       ; Restore writing to main mem

    rts    

.endproc


;-----------------------------------------------------------------------------
; setTilePointer
;
;   Index passed in A
;-----------------------------------------------------------------------------
.proc setTilePointer
    ; 8 bytes
    tay     ; copy A
    ; calculate tile pointer
    asl                     ; *8
    asl
    asl
    sta     tilePtr0
    tya     ; restore A
    lsr                     ; /32
    lsr
    lsr
    lsr
    lsr
    clc
    adc     currentSheet_7x8+1
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

colorChar:  .byte PIXEL_BLACK,PIXEL_WHITE

.endproc


; Return 0(black), 1(white) or 2(masked)
.proc getPixelRaw
    lda     tileIndex
    jsr     setTilePointer
    jsr     getPixelOffset
    and     (tilePtr0),y
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
    lda     tileIndex
    jsr     setTilePointer
    jsr     getPixelOffset
    eor     #$ff
    and     (tilePtr0),y
    sta     (tilePtr0),y
    rts
.endproc

.proc setPixel
    lda     tileIndex
    jsr     setTilePointer
    jsr     getPixelOffset
    ora     (tilePtr0),y
    sta     (tilePtr0),y
    rts
.endproc

.proc copyPixel
    cmp     #PIXEL_BLACK
    beq     clearPixel
    jmp     setPixel
.endproc

; black -> white -> black ...
.proc togglePixel
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
;   y = offset to byte in pixel array
;   a = bitmask within byte
;-----------------------------------------------------------------------------

.proc getPixelOffset

    lda     #0
    clc
    ; multiply y by width in bytes
    ldx     curY
    beq     multY
:
    adc     tileWidthBytes
    dex
    bne     :-
multY:
    ; a = y * width in bytes
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
    sta     pathname_font_end-1

    ; set pathname
    lda     #<pathname_font
    sta     open_params+1
    sta     create_params+1
    sta     stringPtr0
    lda     #>pathname_font
    sta     open_params+2
    sta     create_params+2
    sta     stringPtr1

    ; set address
    lda     #<tileSheet_7x8
    sta     rw_params+2
    lda     #>tileSheet_7x8
    sta     rw_params+3

    ; set size
    lda     #<tileSheet_7x8_size
    sta     rw_params+4
    lda     #>tileSheet_7x8_size
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

tileIndex:          .byte   0
tileSize:           .byte   0

; fixed parameters fo 7x8
tileMax:            .byte   128
tileWidth:          .byte   7
tileWidthBytes:     .byte   1
tileHeight:         .byte   8
tileLength:         .byte   8
tilePixelSize:      .byte   0
canvasLeft:         .byte   0
canvasTop:          .byte   0
canvasRight:        .byte   8
canvasBottom:       .byte   9
pixelOffsetX:       .byte   1
pixelOffsetY:       .byte   1

lastColor:          .byte   PIXEL_WHITE

; General

currentSheet_7x8:   .word   tileSheet_7x8

; ProDos pathname

pathname_font:
    StringLen "/DHGR/DATA/FONT7X8.0"
pathname_font_end:

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

pixelByteMask:                  ; 1 << (x % 7)
    .byte   $01,$02,$04,$08,$10,$20,$40

; Tiles
;-----------------------------------------------------------------------------

.align 256
tileSheet_7x8_size = tileSheet_7x8_end - tileSheet_7x8

tileSheet_7x8:
.include "font7x8_apple2.asm"
tileSheet_7x8_end:


    .dword  .time   ; Time of compilation

