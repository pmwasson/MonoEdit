;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------
; Edit functions
;   Input/Output:
;       - initMonochrome
;       - initColor
;       - clearScreen
;       - getInputNumber
;       - getInputDirection
;       - printAll
;       - printDump
;       - drawString
;       - drawNumber
;       - drawBox
;   Modify:
;       - copyTile
;       - pasteTile
;       - fillPixels
;       - invertPixels
;       - flipVert
;       - flipHor
;       - rotateUp
;       - rotateDown
;       - rotateLeft
;       - rotateRight
;   OS:
;       - quit
;       - loadSheet
;       - saveSheet
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Constants
;-----------------------------------------------------------------------------

PIXEL_BLACK     = $20           ; (0) Space
PIXEL_WHITE     = $0e           ; (1) 
PIXEL_BG_EVEN   = $16           ; (2)
PIXEL_BG_ODD    = $17

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
; Init color mode
;
; Reset into color mode
;-----------------------------------------------------------------------------
.proc initColorMode
    sta     TXTCLR      ; Graphics
    sta     HIRES       ; Hi-res
    sta     MIXSET      ; Mixed
    sta     LOWSCR      ; Display page 1
    sta     DHIRESON    ; Annunciator 2 On
    sta     SET80VID    ; 80 column on
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
;    lda     #$2a
    sta     RAMWRTON  

:
    sta     (screenPtr0),y
    iny
    bne     :-    

;    lda     #$55
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
; Get input direction
;   Pick and diplay 1 of 4 directions or cancel
;-----------------------------------------------------------------------------
.proc getInputDirection
    jsr     getInput
    cmp     #KEY_LEFT
    bne     :+
    jsr     inline_print
    .byte   "Left ",13,0
    lda     #KEY_LEFT
    rts
:
    cmp     #KEY_RIGHT
    bne     :+
    jsr     inline_print
    .byte   "Right",13,0
    lda     #KEY_RIGHT
    rts
:
    cmp     #KEY_UP
    bne     :+
    jsr     inline_print
    .byte   "Up   ",13,0
    lda     #KEY_UP
    rts
:
    cmp     #KEY_DOWN
    bne     :+
    jsr     inline_print
    .byte   "Down ",13,0
    lda     #KEY_DOWN
    rts
:
    jsr     inline_print
    .byte   "Cancel",13,0
    LDA     #0
    rts
.endproc

;-----------------------------------------------------------------------------
; printAll
;-----------------------------------------------------------------------------
.proc printAll

    lda     tileIndex
    sta     temp
    lda     #0
    sta     tileIndex
:
    jsr     printDump
    inc     tileIndex
    lda     tileIndex
    cmp     tileMax
    bne     :-
    rts

temp:   .byte   0
 .endproc   

;-----------------------------------------------------------------------------
; printDump
;-----------------------------------------------------------------------------
.proc printDump
    lda     tileIndex
    jsr     setTilePointer

    jsr     inline_print
    String  "; index $"
    lda     tileIndex
    jsr     PRBYTE

    jsr     inline_print
    .byte   13,".byte ",0

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
    jsr     DHGR_DRAW_7X8

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
; Draw box
;-----------------------------------------------------------------------------

.proc drawBox

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
; Erase box
;-----------------------------------------------------------------------------

.proc eraseBox

    ; Draw corners
    lda     boxLeft
    sta     tileX
    lda     boxTop
    sta     tileY
    lda     #BOX_BLANK
    jsr     DHGR_DRAW_7X8

    lda     boxRight
    sta     tileX    
    lda     #$20
    jsr     DHGR_DRAW_7X8

    lda     boxBottom
    sta     tileY
    lda     #BOX_BLANK
    jsr     DHGR_DRAW_7X8

    lda     boxLeft
    sta     tileX
    lda     #BOX_BLANK
    jsr     DHGR_DRAW_7X8

    ; Draw horizontal

    inc     tileX
:
    lda     boxTop
    sta     tileY
    lda     #BOX_BLANK
    jsr     DHGR_DRAW_7X8
    
    lda     boxBottom
    sta     tileY
    lda     #BOX_BLANK
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
    lda     #BOX_BLANK
    jsr     DHGR_DRAW_7X8
    
    lda     boxRight
    sta     tileX
    lda     #BOX_BLANK
    jsr     DHGR_DRAW_7X8
    
    inc     tileY
    lda     boxBottom
    cmp     tileY
    bne     :-

    rts

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
; Fill pixels
;   Set all pixel to A (0=black, 1=white, 2=background)
;-----------------------------------------------------------------------------

.proc fillPixels

    tax
    lda     colorList,x
    sta     color

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curX

loopX:
    lda     #0
    sta     curY

loopY:
    lda     color
    jsr     copyPixel

    inc     curY
    lda     curY
    cmp     tileHeight
    bne     loopY

    inc     curX
    lda     curX
    cmp     tileWidth
    bne     loopX

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

color:      .byte   0
colorList:  .byte   PIXEL_BLACK,PIXEL_WHITE,PIXEL_BG_EVEN
tempX:      .byte   0
tempY:      .byte   0

.endproc

;-----------------------------------------------------------------------------
; Invert pixels
;-----------------------------------------------------------------------------

.proc invertPixels

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curX

loopX:
    lda     #0
    sta     curY

loopY:

    jsr     getPixel
    cmp     #PIXEL_WHITE
    bne     :+
    lda     #PIXEL_BLACK
    jsr     copyPixel  ; white -> black
    jmp     cont
:
    cmp     #PIXEL_BLACK
    bne     cont
    lda     #PIXEL_WHITE    
    jsr     copyPixel   ; black -> white
cont:

    inc     curY
    lda     curY
    cmp     tileHeight
    bne     loopY

    inc     curX
    lda     curX
    cmp     tileWidth
    bne     loopX

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

tempX:      .byte   0
tempY:      .byte   0

.endproc

;-----------------------------------------------------------------------------
; Flip vert
;-----------------------------------------------------------------------------
; Could be done much faster by using bytes, but was too lazy
.proc flipVert

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curX

loopX:
    lda     #0
    sta     indexY
loopY:

    ; swap pixels
    lda     indexY
    sta     curY
    jsr     getPixel
    sta     temp1

    lda     tileHeight
    clc     ; h-index-1
    sbc     indexY
    sta     curY
    jsr     getPixel
    sta     temp2
    lda     temp1
    jsr     copyPixel

    lda     indexY
    sta     curY
    lda     temp2
    jsr     copyPixel

    inc     indexY
    lda     indexY
    asl
    cmp     tileHeight
    bmi     loopY
    
    inc     curX
    lda     curX
    cmp     tileWidth
    bne     loopX

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

tempX:  .byte   0
tempY:  .byte   0
temp1:  .byte   0
temp2:  .byte   0
indexY: .byte   0

.endproc

;-----------------------------------------------------------------------------
; Flip hor
;-----------------------------------------------------------------------------
; Could be done much faster by using bytes, but was too lazy

.proc flipHor

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curY

loopY:
    lda     #0
    sta     indexX
loopX:

    ; swap pixels
    lda     indexX
    sta     curX
    jsr     getPixel
    sta     temp1

    lda     tileWidth
    clc
    sbc     indexX
    sta     curX
    jsr     getPixel
    sta     temp2
    lda     temp1
    jsr     copyPixel

    lda     indexX
    sta     curX
    lda     temp2
    jsr     copyPixel

    inc     indexX
    lda     indexX
    asl
    cmp     tileWidth
    bmi     loopX
    
    inc     curY
    lda     curY
    cmp     tileHeight
    bne     loopY

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

tempX:  .byte   0
tempY:  .byte   0
temp1:  .byte   0
temp2:  .byte   0
indexX: .byte   0

.endproc

;-----------------------------------------------------------------------------
; Rotate up 
;-----------------------------------------------------------------------------
.proc rotateUp

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curX

loopX:
    lda     #0
    sta     curY

    ; save first pixel
    jsr     getPixel
    sta     temp
    inc     curY

loopY:
    ; read pixel and write pixel above
    jsr     getPixel
    dec     curY
    jsr     copyPixel
    inc     curY
    inc     curY
    lda     curY
    cmp     tileHeight
    bne     loopY

    lda     temp
    dec     curY
    jsr     copyPixel
    
    inc     curX
    lda     curX
    cmp     tileWidth
    bne     loopX

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

tempX:  .byte   0
tempY:  .byte   0
temp:   .byte   0

.endproc

;-----------------------------------------------------------------------------
; Rotate down
;-----------------------------------------------------------------------------

.proc rotateDown

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curX

loopX:
    lda     tileHeight
    sta     curY
    dec     curY

    ; save first pixel
    jsr     getPixel
    sta     temp
    dec     curY

loopY:
    ; read pixel and write pixel above
    jsr     getPixel
    inc     curY
    jsr     copyPixel
    dec     curY
    dec     curY
    lda     curY
    bpl     loopY

    lda     temp
    inc     curY
    jsr     copyPixel
    
    inc     curX
    lda     curX
    cmp     tileWidth
    bne     loopX

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

tempX:  .byte   0
tempY:  .byte   0
temp:   .byte   0

.endproc

;-----------------------------------------------------------------------------
; Rotate Left 
;-----------------------------------------------------------------------------

.proc rotateLeft

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curY

loopY:
    lda     #0
    sta     curX

    ; save first pixel
    jsr     getPixel
    sta     temp
    inc     curX

loopX:
    ; read pixel and write pixel above
    jsr     getPixel
    dec     curX
    jsr     copyPixel
    inc     curX
    inc     curX
    lda     curX
    cmp     tileWidth
    bne     loopX

    lda     temp
    dec     curX
    jsr     copyPixel
    
    inc     curY
    lda     curY
    cmp     tileHeight
    bne     loopY

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

tempX:  .byte   0
tempY:  .byte   0
temp:   .byte   0

.endproc

;-----------------------------------------------------------------------------
; Rotate right 
;-----------------------------------------------------------------------------

.proc rotateRight

    lda     curX
    sta     tempX
    lda     curY
    sta     tempY

    lda     #0
    sta     curY

loopY:
    lda     tileWidth
    sta     curX
    dec     curX

    ; save first pixel
    jsr     getPixel
    sta     temp
    dec     curX

loopX:
    ; read pixel and write pixel above
    jsr     getPixel
    inc     curX
    jsr     copyPixel
    dec     curX
    dec     curX
    lda     curX
    bpl     loopX

    lda     temp
    inc     curX
    jsr     copyPixel
    
    inc     curY
    lda     curY
    cmp     tileHeight
    bne     loopY

    lda     tempX
    sta     curX
    lda     tempY
    sta     curY
    rts

tempX:  .byte   0
tempY:  .byte   0
temp:   .byte   0

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
; Load sheet
;
;   Load sheet using ProDOS
;-----------------------------------------------------------------------------
.proc loadSheet
    jsr     setFileParams

    ; open file
    jsr     MLI
    .byte   CMD_OPEN
    .word   open_params
    bcc     :+

    jsr    PRBYTE
    jsr    inline_print
    .byte  ":unable to open file",13,0
    rts
:
    ;jsr    inline_print
    ;.byte  "File open",13,0

    ; set reference number 
    lda     open_params+5
    sta     rw_params+1
    sta     close_params+1

    ; read data
    jsr    MLI
    .byte  CMD_READ
    .word  rw_params
    bcc    :+

    jsr    PRBYTE
    jsr    inline_print
    .byte  ":unable to read data",13,0
:
    ;jsr    inline_print
    ;.byte  "Data read",13,0

    jsr    MLI
    .byte  CMD_CLOSE
    .word  close_params
    bcc    :+
    jsr    PRBYTE
    jsr    inline_print
    .byte  ":unable to close file",13,0
:
    jsr    inline_print
    .byte  "Load complete",13,0

    rts

.endproc

;-----------------------------------------------------------------------------
; Save sheet
;
;   Use prodos to save tile data
;-----------------------------------------------------------------------------
.proc saveSheet

    jsr     setFileParams

    ; open file
    jsr     MLI
    .byte   CMD_OPEN
    .word   open_params
    bcc     open_good
    
    jsr    PRBYTE
    jsr    inline_print
    .byte  ":unable to open file, creating new",13,0

    ; create file
     jsr     MLI
    .byte   CMD_CREATE
    .word   create_params
    bcc     :+   

    jsr    PRBYTE
    jsr    inline_print
    .byte  ":unable to create file",13,0
    rts    ; give up!
:

    ; open file again!
    jsr     MLI
    .byte   CMD_OPEN
    .word   open_params
    bcc     open_good

    jsr    PRBYTE
    jsr    inline_print
    .byte  ":still unable to open file",13,0
    rts    ; give up

open_good:
    ;jsr    inline_print
    ;.byte  "File open",13,0

    ; set reference number 
    lda     open_params+5
    sta     rw_params+1
    sta     close_params+1

    ; write data
    jsr    MLI
    .byte  CMD_WRITE
    .word  rw_params
    bcc    :+
    jsr    PRBYTE
    jsr    inline_print
    .byte  ":unable to write data",13,0
:
    ;jsr    inline_print
    ;.byte  "Data written",13,0

    jsr    MLI
    .byte  CMD_CLOSE
    .word  close_params
    bcc    :+
    jsr    PRBYTE
    jsr    inline_print
    .byte  ":unable to close file",13,0
:
    jsr    inline_print
    .byte  "Save complete",13,0

    rts
    
.endproc

;-----------------------------------------------------------------------------
; Global ProDos parameters
;-----------------------------------------------------------------------------

open_params:
    .byte   $3
    .word   defaultPathname     ; *OVERWRITE* pathname     
    .word   FILEBUFFER
    .byte   $0                  ;             reference number

rw_params:
    .byte   $4
    .byte   $0                  ;             reference number
    .word   $0                  ; *OVERWRITE* address of data buffer
    .word   $0                  ; *OVERWRITE* number of bytes to read
    .word   $0                  ;             number of bytes read

; Note, not using the real address for the binary file load address as
; the data buffer may move around with re-compiles, so we don't
; want to rely on it.

create_params:
    .byte   $7
    .word   defaultPathname     ; *OVERWRITE* pathname
    .byte   $C3                 ;             access bits (full access)
    .byte   $6                  ;             file type (binary)
    .word   $6000               ;             binary file load address, default to $6000 (above hires pages)
    .byte   $1                  ;             storage type (standard)
    .word   $0                  ;             creation date
    .word   $0                  ;             creation time

close_params:
    .byte   $1
    .byte   $0                  ;             reference number

defaultPathname:
    .byte   7,"UNKNOWN"

;-----------------------------------------------------------------------------
; Global data
;-----------------------------------------------------------------------------

; Box routine   
boxLeft:            .byte   0
boxRight:           .byte   0
boxTop:             .byte   0
boxBottom:          .byte   0

.align      256
clipboardData:      .res    8*16*2