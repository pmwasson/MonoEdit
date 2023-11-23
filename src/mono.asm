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

BOX_HORZ        = $45
BOX_VERT        = BOX_HORZ+1
BOX_UPPER_LEFT  = BOX_HORZ+2
BOX_UPPER_RIGHT = BOX_HORZ+3
BOX_LOWER_LEFT  = BOX_HORZ+4
BOX_LOWER_RIGHT = BOX_HORZ+5

;------------------------------------------------

.segment "CODE"
.org    $4000

;=============================================================================
; Initial jump vector
;=============================================================================

.proc main

    ; set up 80 columns
    jsr     $c300       ; 80 column mode
    jsr     HOME        ; clear screen
    lda     #23         ; put cursor on last line
    sta     CV
    jsr     VTAB

    jsr     initMonochrome  ; Turn on monochrome dhgr
    jsr     clearScreen

    ; display a greeting
    jsr     inline_print
    StringCR    "DHGR Monochrome tile editor - ? for help"


    ; Draw letters

    lda     #0
    sta     tileX
    sta     tileY

    jsr     drawString
    String  "TILE:00 SIZE:7X8"

    ; Edit Box
    lda     #0
    sta     boxLeft
    lda     #8
    sta     boxRight
    lda     #1
    sta     boxTop
    lda     #10
    sta     boxBottom
    jsr     drawBox

    jsr     inline_print
    StringCR    "Exit to monitor"

    jmp     MONZ        ; enter monitor

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

    sta     CLR80COL        ; Use RAMWRT for aux mem

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

; Index passed in A
.proc setTilePointer_7x8

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
    and     #$3f
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
currentSheet_7x8:   .word   tileSheet_7x8

; Box routine   
boxLeft:            .byte   0
boxRight:           .byte   0
boxTop:             .byte   0
boxBottom:          .byte   0

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
tileSheet_7x8:

    .byte $1C, $22, $2A, $3A, $1A, $02, $3C, $00    ; 0x40 @ 
    .byte $08, $14, $22, $22, $3E, $22, $22, $00    ; 0x41 A 
    .byte $1E, $22, $22, $1E, $22, $22, $1E, $00    ; 0x42 B 
    .byte $1C, $22, $02, $02, $02, $22, $1C, $00    ; 0x43 C 
    .byte $1E, $22, $22, $22, $22, $22, $1E, $00    ; 0x44 D 
    .byte $3E, $02, $02, $1E, $02, $02, $3E, $00    ; 0x45 E 
    .byte $3E, $02, $02, $1E, $02, $02, $02, $00    ; 0x46 F 
    .byte $3C, $02, $02, $02, $32, $22, $3C, $00    ; 0x47 G 
    .byte $22, $22, $22, $3E, $22, $22, $22, $00    ; 0x48 H 
    .byte $1C, $08, $08, $08, $08, $08, $1C, $00    ; 0x49 I 
    .byte $20, $20, $20, $20, $20, $22, $1C, $00    ; 0x4A J 
    .byte $22, $12, $0A, $06, $0A, $12, $22, $00    ; 0x4B K 
    .byte $02, $02, $02, $02, $02, $02, $3E, $00    ; 0x4C L 
    .byte $22, $36, $2A, $2A, $22, $22, $22, $00    ; 0x4D M 
    .byte $22, $22, $26, $2A, $32, $22, $22, $00    ; 0x4E N 
    .byte $1C, $22, $22, $22, $22, $22, $1C, $00    ; 0x4F O 
    .byte $1E, $22, $22, $1E, $02, $02, $02, $00    ; 0x50 P 
    .byte $1C, $22, $22, $22, $2A, $12, $2C, $00    ; 0x51 Q 
    .byte $1E, $22, $22, $1E, $0A, $12, $22, $00    ; 0x52 R 
    .byte $1C, $22, $02, $1C, $20, $22, $1C, $00    ; 0x53 S 
    .byte $3E, $08, $08, $08, $08, $08, $08, $00    ; 0x54 T 
    .byte $22, $22, $22, $22, $22, $22, $1C, $00    ; 0x55 U 
    .byte $22, $22, $22, $22, $22, $14, $08, $00    ; 0x56 V 
    .byte $22, $22, $22, $2A, $2A, $36, $22, $00    ; 0x57 W 
    .byte $22, $22, $14, $08, $14, $22, $22, $00    ; 0x58 X 
    .byte $22, $22, $14, $08, $08, $08, $08, $00    ; 0x59 Y 
    .byte $3E, $20, $10, $08, $04, $02, $3E, $00    ; 0x5A Z 
    .byte $3E, $06, $06, $06, $06, $06, $3E, $00    ; 0x5B [ 
    .byte $00, $02, $04, $08, $10, $20, $00, $00    ; 0x5C \ 
    .byte $3E, $30, $30, $30, $30, $30, $3E, $00    ; 0x5D ] 
    .byte $00, $00, $08, $14, $22, $00, $00, $00    ; 0x5E ^ 
    .byte $00, $00, $00, $00, $00, $00, $00, $7F    ; 0x5F _ 
    .byte $00, $00, $00, $00, $00, $00, $00, $00    ; 0x20   
    .byte $08, $08, $08, $08, $08, $00, $08, $00    ; 0x21 ! 
    .byte $14, $14, $14, $00, $00, $00, $00, $00    ; 0x22 " 
    .byte $14, $14, $3E, $14, $3E, $14, $14, $00    ; 0x23 # 
    .byte $08, $3C, $0A, $1C, $28, $1E, $08, $00    ; 0x24 $ 
    .byte $06, $26, $10, $08, $04, $32, $30, $00    ; 0x25 % 
    .byte $04, $0A, $0A, $04, $2A, $12, $2C, $00    ; 0x26 & 
    .byte $08, $08, $08, $00, $00, $00, $00, $00    ; 0x27 ' 
    .byte $08, $04, $02, $02, $02, $04, $08, $00    ; 0x28 ( 
    .byte $08, $10, $20, $20, $20, $10, $08, $00    ; 0x29 ) 
    .byte $08, $2A, $1C, $08, $1C, $2A, $08, $00    ; 0x2A * 
    .byte $00, $08, $08, $3E, $08, $08, $00, $00    ; 0x2B + 
    .byte $00, $00, $00, $00, $08, $08, $04, $00    ; 0x2C , 
    .byte $00, $00, $00, $3E, $00, $00, $00, $00    ; 0x2D - 
    .byte $00, $00, $00, $00, $00, $00, $08, $00    ; 0x2E . 
    .byte $00, $20, $10, $08, $04, $02, $00, $00    ; 0x2F / 
    .byte $1C, $22, $32, $2A, $26, $22, $1C, $00    ; 0x30 0 
    .byte $08, $0C, $08, $08, $08, $08, $1C, $00    ; 0x31 1 
    .byte $1C, $22, $20, $18, $04, $02, $3E, $00    ; 0x32 2 
    .byte $3E, $20, $10, $18, $20, $22, $1C, $00    ; 0x33 3 
    .byte $10, $18, $14, $12, $3E, $10, $10, $00    ; 0x34 4 
    .byte $3E, $02, $1E, $20, $20, $22, $1C, $00    ; 0x35 5 
    .byte $38, $04, $02, $1E, $22, $22, $1C, $00    ; 0x36 6 
    .byte $3E, $20, $10, $08, $04, $04, $04, $00    ; 0x37 7 
    .byte $1C, $22, $22, $1C, $22, $22, $1C, $00    ; 0x38 8 
    .byte $1C, $22, $22, $3C, $20, $10, $0E, $00    ; 0x39 9 
    .byte $00, $00, $08, $00, $08, $00, $00, $00    ; 0x3A : 
    .byte $00, $00, $08, $00, $08, $08, $04, $00    ; 0x3B ; 
    .byte $10, $08, $04, $02, $04, $08, $10, $00    ; 0x3C < 
    .byte $00, $00, $3E, $00, $3E, $00, $00, $00    ; 0x3D = 
    .byte $04, $08, $10, $20, $10, $08, $04, $00    ; 0x3E > 
    .byte $1C, $22, $10, $08, $08, $00, $08, $00    ; 0x3F ? 

    ; Screen elements starting at 0x40 (no lower case)

    .byte $55, $2a, $55, $2a, $55, $2a, $55, $2a    ; Background pixel - even
    .byte $2a, $55, $2a, $55, $2a, $55, $2a, $55    ; Background pixel - odd
    .byte $00, $00, $00, $00, $00, $00, $00, $00    ; Black pixel
    .byte $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f    ; White pixel
    .byte $41, $22, $14, $08, $08, $14, $22, $41    ; X cursor
    .byte $00, $00, $00, $7F, $00, $00, $00, $00    ; Horizontal line 
    .byte $08, $08, $08, $08, $08, $08, $08, $08    ; Vertical line 
    .byte $00, $00, $00, $F8, $08, $08, $08, $08    ; Box NW Corner
    .byte $00, $00, $00, $0F, $08, $08, $08, $08    ; Box NE Corner 
    .byte $08, $08, $08, $F8, $00, $00, $00, $00    ; Box SW Corner 
    .byte $08, $08, $08, $0F, $00, $00, $00, $00    ; Box SE Corner
