;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------
; Mono Edit
;  DHGR Monochrom Tile editor

;------------------------------------------------
; Defines
;------------------------------------------------

.include "defines.asm"
.include "macros.asm"

;------------------------------------------------
; Constants
;------------------------------------------------

CURSOR          = $02

PIXEL_BLACK     = $20           ; Space
PIXEL_WHITE     = $0e
PIXEL_BG_EVEN   = $16
PIXEL_BG_ODD    = $17

BOX_HORZ        = $13
BOX_VERT        = $7c
BOX_UPPER_LEFT  = $1c
BOX_UPPER_RIGHT = $1d
BOX_LOWER_LEFT  = $1e
BOX_LOWER_RIGHT = $1f
BOX_BLANK       = $20

;------------------------------------------------

.segment "CODE"
.org    $4000

;=============================================================================
; Main
;=============================================================================

.proc main

    ; set up 80 columns
    jsr     $c300       ; 80 column mode
    jsr     HOME        ; clear screen
    lda     #23         ; put cursor on last line
    sta     CV
    jsr     VTAB

    ; display a greeting
    jsr     inline_print
    StringCR    "DHGR Monochrome tile editor - ? for help"

    ; default to 7x8
    lda     #0
    jsr     setTileSize
    jsr     initMonochrome  ; Turn on monochrome dhgr


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

    jsr     eraseBox
    lda     tileIndex
    bne     previous_continue
    lda     tileMax
    sta     tileIndex
previous_continue:
    dec     tileIndex
    lda     tileIndex
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     refresh_loop
:
    ;------------------
    ; _ = Previous 8
    ;------------------
    cmp     #$80 | '_'
    bne     :+
    jsr     inline_print
    .byte   "Previous 8 tiles: ",0

    jsr     eraseBox
    lda     tileIndex
    sec     
    sbc     #8
    bpl     previous8_continue
    clc
    adc     tileMax
previous8_continue:
    sta     tileIndex
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     refresh_loop
:
    ;------------------
    ; = = Next
    ;------------------
    cmp     #$80 | '='
    bne     :+
    jsr     inline_print
    .byte   "Next tile: ",0

    jsr     eraseBox
    inc     tileIndex
    lda     tileIndex
    cmp     tileMax
    bne     next_continue
    lda     #0
    sta     tileIndex
next_continue:
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     refresh_loop
:
    ;------------------
    ; + = Next 8
    ;------------------
    cmp     #$80 | '+'
    bne     :+
    jsr     inline_print
    .byte   "Next 8 tiles: ",0

    jsr     eraseBox
    lda     tileIndex
    clc
    adc     #8
    cmp     tileMax
    bmi     next_continue8
    sec
    sbc     tileMax
next_continue8:
    sta     tileIndex
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     refresh_loop
:
    ;------------------
    ; SP = Toggle Pixel
    ;------------------
    cmp     #KEY_SPACE
    bne     :+
    jsr     inline_print
    StringCR "Toggle Pixel"
    jsr     togglePixel
    jsr     drawPixel
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
    jsr     drawPixel
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
    jsr     setPixel
    jsr     drawPixel
    jsr     drawPreview
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
    ; ^T = Tile Size
    ;------------------
    cmp     #KEY_CTRL_T
    bne     :+
    jsr     inline_print
    StringCont "Tile size"
    StringCont "(0=7x8  1=14x8  2=28x8  3=56x8,"
    String     " 4=7x16 5=14x16 6=28x16 7=56x16):"
    lda     #8
    jsr     getInputNumber
    bmi     tileSizeCancel
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
    jmp     command_loop

.endproc

;-----------------------------------------------------------------------------
; getInputNumber
;   Get input for a number 0..max+1, where A == max+1
;   Display number or cancel and return result in A (-1 for cancel)
;-----------------------------------------------------------------------------
.proc getInputNumber
    clc
    adc     #$80 + '0'  ; convert A to ascii number
    sta     max_digit     
    jsr     getInput
    cmp     #$80 + '0'
    bmi     cancel
    cmp     max_digit
    bpl     cancel
    jsr     COUT
    sec
    sbc     #$80 + '0'
    rts
cancel:
    jsr     inline_print
    .byte   "Cancel",13,0
    lda     #$ff
    rts

; local variable
max_digit:  .byte   0

.endproc

;-----------------------------------------------------------------------------
; printHelp
;-----------------------------------------------------------------------------
.proc printHelp
    bit     TXTSET
    jsr     inline_print
    StringCont  "  Arrows:  Move cursor"
    StringCont  "  0:       Clear pixel"
    StringCont  "  1:       Set pixel"
    StringCont  "  Space:   Toggle pixel"
    StringCont  "  Ctrl-C:  Copy tile to clipboard"
    StringCont  "  Ctrl-V:  Paste tile from clipboard (overwrites current tile)"
    StringCont  "  -,=:     Go to previous/next tile (holding shift moves 8 tile)"
    StringCont  "  !:       Dump bytes"
    StringCont  "  ?:       This help screen"
    StringCont  "  \:       Monitor"
    StringCont  "  Ctrl-Q:  Quit"
    StringCont  "  Escape:  Toggle text/graphics"
    .byte   0

    rts
.endproc

;-----------------------------------------------------------------------------
; printDump
;-----------------------------------------------------------------------------
.proc printDump

    lda     tileIndex
    jsr     setTilePointer

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
    lda     (bgPtr0),y
    jsr     PRBYTE
    inc     dump_count
    lda     dump_count
    cmp     tileLength
    beq     dump_finish
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
; setTileSize
;   size passed in A  as an ASCII number
;       0:7x8  1:14x8  2:28x8  3:56x8
;       4:7x16 5:14x15 6:28x16 7:56x16
;-----------------------------------------------------------------------------
.proc setTileSize
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
    rts

.align 8
sizeWidth:      .byte   7, 14, 28, 56, 7,  14, 28, 56
sizeWidthBytes: .byte   1, 2,  4,  8,  1,  2,  4,  8
sizeHeight:     .byte   8, 8,  8,  8,  16, 16, 16, 16
sizeLength:     .byte   8, 16, 32, 64, 16, 32, 64, 128
sizeMax:        .byte   128, 64, 64, 64, 64, 64, 64, 64
.endproc

;-----------------------------------------------------------------------------
; Fill pixels
;   Set all pixel to A (0=black, 1=white)
;-----------------------------------------------------------------------------

.proc fillPixels

    sta     color
    beq     :+
    lda     #$7f
    sta     color
:
    lda     tileIndex
    jsr     setTilePointer

    lda     color
    ldy     #0
:
    sta     (bgPtr0),y

    iny
    cpy     tileLength
    bne     :-
    rts

color:      .byte 0

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
; drawPreview
;
;-----------------------------------------------------------------------------

.proc drawPreview
    ; dispatched based on size
    lda     tileSize
    bne     :+
    jmp     drawPreview_7x8
:
    cmp     #1
    bne     :+
    jmp     drawPreview_14x8
:
    cmp     #2
    bne     :+
    jmp     drawPreview_28x8
:
    brk

drawPreview_14x8:
    lda     #20
    sta     tileX
    lda     #0
    sta     tileY
    lda     tileIndex
    jsr     drawTile_14x8
    rts

drawPreview_28x8:
    lda     #20
    sta     tileX
    lda     #0
    sta     tileY
    lda     tileIndex
    jsr     drawTile_28x8
    rts

drawPreview_7x8:

    ; Draw all tiles
    lda     #0
    sta     index

    lda     #1
    sta     tileY

yloop:
    lda     #79-16*2        ; 16 across
    sta     tileX

xloop:
    lda     index
    jsr     drawTile_7x8
    inc     index

    inc     tileX
    inc     tileX
    lda     tileX
    cmp     #79
    bne     xloop

    inc     tileY
    inc     tileY
    lda     tileY
    cmp     #1+8*2          ; 8 high
    bne     yloop 

    ; Draw box around current tile
    lda     tileIndex
    lsr
    lsr                    
    lsr
    and     #$fe            ; /16 * 2 -> /8 and mask bit 0        
    sta     boxTop
    clc
    adc     #2
    sta     boxBottom

    lda     tileIndex
    and     #$f
    asl
    clc
    adc     #78-16*2
    sta     boxLeft
    clc
    adc     #2
    sta     boxRight
    jsr     drawBox

    rts

index:      .byte 0

.endproc

;-----------------------------------------------------------------------------
; DrawCanvas
;
;-----------------------------------------------------------------------------
.proc drawCanvas

    ; Draw outline
    lda     canvasX
    sta     boxLeft
    sec
    adc     tileWidth
    sta     boxRight    ; right = left + width + 1
    lda     canvasY
    sta     boxTop
    sec
    adc     tileHeight
    sta     boxBottom   ; bottom = top + height + 1
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

    ; set location
    lda     curY
    sec
    adc     canvasY
    sta     tileY

    lda     curX
    sec
    adc     canvasX
    sta     tileX

    jsr     getPixel
    bne     :+
    lda     #PIXEL_BLACK
    jmp     cont
:
    lda     #PIXEL_WHITE

cont:
    jsr     drawTile_7x8

    rts

.endproc

;-----------------------------------------------------------------------------
; DrawCursor
;   Based on curX, curY
;-----------------------------------------------------------------------------
.proc drawCursor

    ; set location
    lda     curY
    sec
    adc     canvasY
    sta     tileY

    lda     curX
    sec
    adc     canvasX
    sta     tileX

    lda     #CURSOR
    jsr     drawTile_7x8

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
    jsr     setTilePointer_7x8

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
    lda     (bgPtr0),y
    eor     invMask
    sta     (screenPtr0),y

    ; assumes aligned such that there are no page crossing
    inc     bgPtr0

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    bne     drawLoop

    sta     RAMWRTOFF       ; Restore writing to main mem

    rts    

.endproc

;-----------------------------------------------------------------------------
; drawTile_14x8
;
;   Always starts in AUX memory
;
;-----------------------------------------------------------------------------
.proc drawTile_14x8

    ; tile index passes in A
    jsr     setTilePointer_14x8

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
    ; Byte 0 in AUX memory
    sta     RAMWRTON
    ldy     #0
    lda     (bgPtr0),y
    eor     invMask
    sta     (screenPtr0),y

    ; Bytes 1 in MAIN memory
    inc     bgPtr0
    sta     RAMWRTOFF
    lda     (bgPtr0),y
    eor     invMask
    sta     (screenPtr0),y
    inc     bgPtr0

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    bne     drawLoop

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
    eor     invMask
    sta     (screenPtr0),y
    ldy     #1
    lda     (bgPtr0),y
    eor     invMask
    sta     (screenPtr0),y

    ; Bytes 2,3 in MAIN memory
    inc     bgPtr0
    inc     bgPtr0
    sta     RAMWRTOFF
    ldy     #0
    lda     (bgPtr0),y
    eor     invMask
    sta     (screenPtr0),y
    ldy     #1
    lda     (bgPtr0),y
    eor     invMask
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
    bne     :+
    jmp     setTilePointer_7x8
:   
    cpy     #1
    bne     :+
    jmp     setTilePointer_14x8
:   
    cpy     #2
    bne     :+
    jmp     setTilePointer_28x8
:   
    brk

.endproc

.proc setTilePointer_7x8
    ; 8 bytes
    tay     ; copy A
    ; calculate tile pointer
    asl                     ; *8
    asl
    asl
    sta     bgPtr0
    tya     ; restore A
    lsr                     ; /32
    lsr
    lsr
    lsr
    lsr
    clc
    adc     currentSheet_7x8+1
    sta     bgPtr1

    rts

.endproc

.proc setTilePointer_14x8
    ; 16 bytes
    tay     ; copy A
    ; calculate tile pointer
    asl                     ; *16
    asl
    asl
    asl
    sta     bgPtr0
    tya     ; restore A
    lsr                     ; /16
    lsr
    lsr
    lsr
    clc
    adc     currentSheet_14x8+1
    sta     bgPtr1

    rts

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

.proc setTilePointer_56x8
    ; 64 bytes
    tay     ; copy A
    ; calculate tile pointer
    asl                     ; *64
    asl
    asl
    asl
    asl
    asl
    sta     bgPtr0
    tya     ; restore A
    lsr                     ; /4
    lsr
    clc
    adc     currentSheet_56x8+1
    sta     bgPtr1

    rts

.endproc

;-----------------------------------------------------------------------------
; getPixel
;
;  Read single pixel from tile passed in A using curX and curY
;
;-----------------------------------------------------------------------------

.proc getPixel
    lda     tileIndex
    jsr     setTilePointer
    jsr     getPixelOffset
    and     (bgPtr0),y
    rts
.endproc

.proc setPixel
    lda     tileIndex
    jsr     setTilePointer
    jsr     getPixelOffset
    ora     (bgPtr0),y
    sta     (bgPtr0),y
    rts
.endproc

.proc clearPixel
    lda     tileIndex
    jsr     setTilePointer
    jsr     getPixelOffset
    eor     #$ff
    and     (bgPtr0),y
    sta     (bgPtr0),y
    rts
.endproc

.proc togglePixel
    lda     tileIndex
    jsr     setTilePointer
    jsr     getPixelOffset
    eor     (bgPtr0),y
    sta     (bgPtr0),y
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
; Draw String
;
;   Use tileX and tileY for start and string inlined
;-----------------------------------------------------------------------------

.proc drawString

    ; Pop return address to find string
    pla
    sta     stringPtr0
    pla
    sta     stringPtr1
    ldy     #0

    lda     tileX
    sta     offset

    ; Print characters until 0 (end-of-string)
printLoop:
    iny
    bne     :+              ; Allow strings > 255
    inc     stringPtr1
:
    tya
    pha
    lda     (stringPtr0),y
    beq     printExit
    cmp     #13
    bne     :+
    inc     tileY
    lda     offset
    sta     tileX
    jmp     continue
:
    and     #$7f
    jsr     drawTile_7x8

    inc     tileX
continue:
    pla
    tay
    jmp     printLoop

printExit:    
    pla                 ; clean up stack
    ; calculate return address after print string
    clc
    tya
    adc     stringPtr0  ; add low-byte first
    tax                 ; save in X
    lda     stringPtr1  ; carry to high-byte
    adc     #0          
    pha                 ; push return high-byte
    txa
    pha                 ; push return low-byte
    rts                 ; return

char:   .byte   0
offset: .byte   0
.endproc

;-----------------------------------------------------------------------------
; Draw box
;
;-----------------------------------------------------------------------------

.proc drawBox

    ; Draw corners
    lda     boxLeft
    sta     tileX
    lda     boxTop
    sta     tileY
    lda     #BOX_UPPER_LEFT
    jsr     drawTile_7x8

    lda     boxRight
    sta     tileX    
    lda     #BOX_UPPER_RIGHT
    jsr     drawTile_7x8

    lda     boxBottom
    sta     tileY
    lda     #BOX_LOWER_RIGHT
    jsr     drawTile_7x8

    lda     boxLeft
    sta     tileX
    lda     #BOX_LOWER_LEFT
    jsr     drawTile_7x8

    ; Draw horizontal

    inc     tileX
:
    lda     boxTop
    sta     tileY
    lda     #BOX_HORZ
    jsr     drawTile_7x8
    
    lda     boxBottom
    sta     tileY
    lda     #BOX_HORZ
    jsr     drawTile_7x8
    
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
    jsr     drawTile_7x8
    
    lda     boxRight
    sta     tileX
    lda     #BOX_VERT
    jsr     drawTile_7x8
    
    inc     tileY
    lda     boxBottom
    cmp     tileY
    bne     :-

    rts

.endproc

.proc eraseBox

    ; Draw corners
    lda     boxLeft
    sta     tileX
    lda     boxTop
    sta     tileY
    lda     #BOX_BLANK
    jsr     drawTile_7x8

    lda     boxRight
    sta     tileX    
    lda     #$20
    jsr     drawTile_7x8

    lda     boxBottom
    sta     tileY
    lda     #BOX_BLANK
    jsr     drawTile_7x8

    lda     boxLeft
    sta     tileX
    lda     #BOX_BLANK
    jsr     drawTile_7x8

    ; Draw horizontal

    inc     tileX
:
    lda     boxTop
    sta     tileY
    lda     #BOX_BLANK
    jsr     drawTile_7x8
    
    lda     boxBottom
    sta     tileY
    lda     #BOX_BLANK
    jsr     drawTile_7x8
    
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
    lda     #BOX_BLANK
    jsr     drawTile_7x8
    
    lda     boxRight
    sta     tileX
    lda     #BOX_BLANK
    jsr     drawTile_7x8
    
    inc     tileY
    lda     boxBottom
    cmp     tileY
    bne     :-

    rts

.endproc

;-----------------------------------------------------------------------------
; DHGR clear screen
;-----------------------------------------------------------------------------

.proc clearScreen
    lda     #$00
    sta     screenPtr0
    lda     #$20
    sta     screenPtr1

    sta     CLR80COL        ; Use RAMWRT for aux mem

loop:
    ldy     #0

    ; aux mem
    lda     #0
    sta     RAMWRTON  

:
    sta     (screenPtr0),y
    iny
    bne     :-    

    sta     RAMWRTOFF

    ; main mem
:
    sta     (screenPtr0),y
    iny
    bne     :-    

    inc     screenPtr1
    lda     #$40
    cmp     screenPtr1
    bne     loop
    rts

.endproc

;-----------------------------------------------------------------------------
; Init double hi-res monochrome
;-----------------------------------------------------------------------------

.proc initMonochrome
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
    sta     MIXSET      ; Mixed
    rts
.endproc

;-----------------------------------------------------------------------------
; Quit
;
;   Exit to ProDos
;-----------------------------------------------------------------------------
.proc quit
    bit     TXTSET          ; Make sure in text mode

    jsr     MLI
    .byte   CMD_QUIT
    .word  quit_params

quit_params:
    .byte   4               ; 4 parameters
    .byte   0               ; 0 is the only quit type
    .word   0               ; Reserved pointer for future use (what future?)
    .byte   0               ; Reserved byte for future use (what future?)
    .word   0               ; Reserved pointer for future use (what future?)
.endproc


;-----------------------------------------------------------------------------
; Utilies

.include "inline_print.asm"
.include "sounds.asm"


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


canvasX:            .byte   0
canvasY:            .byte   0

; General

currentSheet_7x8:   .word   tileSheet_7x8
currentSheet_14x8:  .word   tileSheet_2k
currentSheet_28x8:  .word   tileSheet_2k
currentSheet_56x8:  .word   tileSheet_2k
currentSheet_7x16:  .word   tileSheet_2k
currentSheet_14x16: .word   tileSheet_2k
currentSheet_28x16: .word   tileSheet_2k
currentSheet_56x16: .word   tileSheet_2k

; Box routine   
boxLeft:            .byte   0
boxRight:           .byte   0
boxTop:             .byte   0
boxBottom:          .byte   0

clipboardData:      .res    8*16

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
tileSheet_7x8:

.include "font7x8_apple2.asm"

.align 256
tileSheet_2k:
    .res    2048

