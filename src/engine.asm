;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
;  Game engine

;------------------------------------------------
; Constants
;------------------------------------------------

.include "defines.asm"
.include "macros.asm"

.segment "CODE"
.org    $C00

; Jump table (in fixed locations)
    jmp     drawTest            ; Remove once game calls engine
    jmp     engineInit
    jmp     drawTile_7x8
    jmp     drawTile_28x8
    jmp     drawTileMask_28x8
    jmp     drawPixel4x4

; Variables (in fixed locations)
.align 32
tileSheet_7x8:          .word   $B000
tileSheet_28x8:         .word   $B000

;-----------------------------------------------------------------------------
; engineInit
;
; Set up aux memory
;-----------------------------------------------------------------------------
.proc engineInit

    ; copy code to aux memory
    lda     #<auxMemStart
    sta     A1
    sta     A4
    lda     #>auxMemStart
    sta     A1+1
    sta     A4+1
    lda     #<auxMemEnd
    sta     A2
    lda     #>auxMemEnd
    sta     A2+1
    sec                     ; copy from main to aux
    jsr     AUXMOVE

    ; init variables
    lda     #0
    sta     drawPage
    sta     invMask
    rts

.endproc

auxMemStart:
; This code is copied such that it exists in both main and aux memory.

;-----------------------------------------------------------------------------
; drawTile_7x8
;  Draw a tile that is 7 pixels wide (1 byte) by 8 pixels high, for a total
;    of 8 bytes.
; Can be either in aux (even) or main (odd) memory depending on X.
;  Assume 7x8, where 7 is 7*4 pixels = 28 -> 4 bytes
;
; Assumes tileSheet is page aligned in AUX memory
;
;-----------------------------------------------------------------------------
.proc drawTile_7x8

    ; tile index passes in A
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
    adc     tileSheet_7x8+1
    sta     tilePtr1


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

    sta     RAMRDON         ; Transfer to AUX memory to read data  

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

    sta     RAMRDOFF        ; Transfer back to main mem  
    sta     RAMWRTOFF       ; Restore writing to main mem

    rts    

.endproc

;-----------------------------------------------------------------------------
; drawTile (28x8)
;  Assume 28x8, where 28 is 28/7 = 4 bytes
;    4*8 = 32 (split main/aux), so 16 bytes per bank
;    Interleaved by 2
;
;  Storage:  00  02  01  03  ; line 0
;            04  06  05  07  ; line 1
;            ..
;            28  30  29  31  ; line 7
;
;-----------------------------------------------------------------------------
.proc drawTile_28x8
    bne     :+
    rts
:
    sta     tileIdx
    ; calculate tile pointer
    asl                     ; *16
    asl
    asl
    asl
    sta     tilePtr0
    sta     tilePtr0Copy
    lda     tileIdx
    lsr                     ; /16
    lsr
    lsr
    lsr
    clc
    adc     tileSheet_28x8+1
    sta     tilePtr1

    ; calculate screen pointer
    ldx     tileY
    lda     tileX
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    sta     screenPtr0Copy
    lda     linePage,x
    adc     drawPage
    sta     screenPtr1
    sta     screenPtr1Copy

    jsr     drawTile

    ; restore tile pointers (page byte doesn't change)
    lda     tilePtr0Copy
    sta     tilePtr0

    ; restore screen pointer
    lda     screenPtr0Copy
    sta     screenPtr0
    lda     screenPtr1Copy
    sta     screenPtr1

    ; transfer to aux memory
    sta     RAMWRTON  
    sta     RAMRDON  

    ; draw half of tile in aux mem
    jsr     drawTile    ; AUX

    sta     RAMWRTOFF   ; AUX
    sta     RAMRDOFF    ; AUX

    rts

drawTile:

    clc     ; no carry generated inside of loop
    ldx     #8  ; 8 lines

drawLoop:
    ldy     #0
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (tilePtr0),y
    sta     (screenPtr0),y

    ; assumes aligned such that there are no page crossing
    lda     tilePtr0
    adc     #2
    sta     tilePtr0

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    bne     drawLoop

    rts    

; locals
tilePtr0Copy:     .byte   0
screenPtr0Copy: .byte   0
screenPtr1Copy: .byte   0

.endproc

;-----------------------------------------------------------------------------
; Draw Tile w/Mask 28x8
;
; 4*8*2 = 32 bytes (16 main / 16 aux) + 32 bytes mask (16 main / 16 aux)
;   
;-----------------------------------------------------------------------------
.proc drawTileMask_28x8

    bne     :+
    rts                     ; tile 0 is skip
:

    sta     tileIdx
    ; calculate tile pointer
    asl                     ; *16
    asl
    asl
    asl
    sta     tilePtr0
    sta     tilePtr0Copy
    clc
    adc     #16
    sta     maskPtr0
    lda     tileIdx
    lsr                     ; /16
    lsr
    lsr
    lsr
    clc
    adc     tileSheet_28x8+1
    sta     tilePtr1
    sta     maskPtr1

    ; calculate screen pointer
    ldx     tileY
    lda     tileX
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    sta     screenPtr0Copy
    lda     linePage,x
    adc     drawPage
    sta     screenPtr1
    sta     screenPtr1Copy

    jsr     drawTile


    ; restore tile pointers (page byte doesn't change)
    lda     tilePtr0Copy
    sta     tilePtr0
    clc
    adc     #16
    sta     maskPtr0

    ; restore screen pointer
    lda     screenPtr0Copy
    sta     screenPtr0
    lda     screenPtr1Copy
    sta     screenPtr1

    ; transfer to aux memory
    sta     RAMWRTON  
    sta     RAMRDON  

    ; draw half of tile in aux mem
    jsr     drawTile    ; AUX

    sta     RAMWRTOFF   ; AUX
    sta     RAMRDOFF    ; AUX

    rts

drawTile:

    clc     ; no carry generated inside of loop
    ldx     #8  ; 8 lines

drawLoop:
    ldy     #0
    lda     (screenPtr0),y
    and     (maskPtr0),y
    ora     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (screenPtr0),y
    and     (maskPtr0),y
    ora     (tilePtr0),y
    sta     (screenPtr0),y

    ; assumes aligned such that there are no page crossing
    lda     tilePtr0
    adc     #2
    sta     tilePtr0

    lda     maskPtr0
    adc     #2
    sta     maskPtr0

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    bne     drawLoop

    rts    

; locals
tilePtr0Copy:     .byte   0
maskPtr0Copy:     .byte   0
screenPtr0Copy: .byte   0
screenPtr1Copy: .byte   0

.endproc

;-----------------------------------------------------------------------------
; drawPixel
;   X = pixel color (0=black, 1=white, 2=background)
;   Draw a 4x4 pixel (3x3 + boarder) using tileX, tileY for coordinates
;   tileX range 0..111
;   tileY range 0..31
;-----------------------------------------------------------------------------
.proc drawPixel4x4
    stx     colorIndex
    sta     CLR80COL        ; Use RAMWRT for aux mem

    ; calculate screen pointer
    lda     tileY
    lsr
    tax                     ; /2 since 4 pixel
    ldy     tileX
    clc
    lda     pixelDiv14,y    ; /14 * 4 (offset 4 = 8 bytes: aux + main)
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0
    lda     tileY
    and     #1
    beq     :+
    lda     #4*4            ; down 4 pixel for odd tile Y
:
    adc     linePage,x
    sta     screenPtr1
    sta     screenPtr1Copy

    ; set up mask
    ldy     tileX
    lda     pixelRem14,y
    clc
    adc     #<pixelMaskEven
    sta     tilePtr0
    lda     #>pixelMaskEven
    sta     tilePtr1

    ldx     colorIndex
    ; transfer to aux memory
    sta     RAMWRTON
    sta     RAMRDON

    ; draw half of tile in aux mem
    lda     colorTable0a,x
    jsr     drawLine
    lda     colorTable0b,x
    jsr     drawLine
    lda     colorTable0a,x
    jsr     drawLine
    lda     colorTable0c,x
    jsr     drawLine

    ; back to main
    sta     RAMWRTOFF
    sta     RAMRDOFF

    clc
    lda     tilePtr0
    adc     #(pixelMaskOdd-pixelMaskEven)
    sta     tilePtr0

    lda     screenPtr1Copy
    sta     screenPtr1

    lda     colorTable1a,x
    jsr     drawLine
    lda     colorTable1b,x
    jsr     drawLine
    lda     colorTable1a,x
    jsr     drawLine
    lda     colorTable1c,x
    jsr     drawLine

    rts

drawLine:
    sta     colorMask
    ldy     #0
:
    lda     (tilePtr0),y
    eor     #$ff
    and     (screenPtr0),y
    sta     temp
    lda     (tilePtr0),y
    and     colorMask
    ora     temp
    sta     (screenPtr0),y

    iny
    cpy     #4
    bne     :-

    lda     screenPtr1
    clc
    adc     #$4
    sta     screenPtr1

    rts

temp:   .byte   0
screenPtr1Copy:
        .byte   0

colorIndex: .byte   0
colorMask:  .byte   0

colorTable0a:
        .byte   $00, $ff, $55, $aa, $00
colorTable1a:
        .byte   $00, $ff, $aa, $55, $00
colorTable0b:
        .byte   $00, $ff, $aa, $55, $ff
colorTable1b:
        .byte   $00, $ff, $55, $aa, $ff
colorTable0c:
        .byte   $00, $00, $aa, $55, $00
colorTable1c:
        .byte   $00, $00, $55, $aa, $00

; Data need to be available in both main and aux mem

.align 256
; 8-byte mask per offset
; Even bytes (aux memory)
pixelMaskEven:
    .byte   $07,$00,$00,$00     ; 0
    .byte   $70,$00,$00,$00     ; 1
    .byte   $00,$00,$00,$00     ; 2
    .byte   $00,$01,$00,$00     ; 3
    .byte   $00,$1c,$00,$00     ; 4
    .byte   $00,$40,$00,$00     ; 5
    .byte   $00,$00,$00,$00     ; 6
    .byte   $00,$00,$07,$00     ; 7
    .byte   $00,$00,$70,$00     ; 8
    .byte   $00,$00,$00,$00     ; 9
    .byte   $00,$00,$00,$01     ; 10
    .byte   $00,$00,$00,$1c     ; 11
    .byte   $00,$00,$00,$40     ; 12
    .byte   $00,$00,$00,$00     ; 13

; Odd bytes (main memory)
pixelMaskOdd:
    .byte   $00,$00,$00,$00     ; 0
    .byte   $00,$00,$00,$00     ; 1
    .byte   $0e,$00,$00,$00     ; 2
    .byte   $60,$00,$00,$00     ; 3
    .byte   $00,$00,$00,$00     ; 4
    .byte   $00,$03,$00,$00     ; 5
    .byte   $00,$38,$00,$00     ; 6
    .byte   $00,$00,$00,$00     ; 7
    .byte   $00,$00,$00,$00     ; 8
    .byte   $00,$00,$0e,$00     ; 9
    .byte   $00,$00,$60,$00     ; 10
    .byte   $00,$00,$00,$00     ; 11
    .byte   $00,$00,$00,$03     ; 12
    .byte   $00,$00,$00,$38     ; 13

.endproc

auxMemEnd:

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

; Lookup table to divide by 14 then *4 for screen x-offset
pixelDiv14:
    .byte    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    .byte    4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4
    .byte    8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8
    .byte   12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12
    .byte   16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16
    .byte   20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20
    .byte   24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24
    .byte   28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28

; Remainder / 14 for 4-byte lookup  * 4
pixelRem14:
    .byte    0,  4,  8,  12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52
    .byte    0,  4,  8,  12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52
    .byte    0,  4,  8,  12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52
    .byte    0,  4,  8,  12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52
    .byte    0,  4,  8,  12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52
    .byte    0,  4,  8,  12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52
    .byte    0,  4,  8,  12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52
    .byte    0,  4,  8,  12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52

;-----------------------------------------------------------------------------
; drawTest
;
; Set up aux memory
;-----------------------------------------------------------------------------
.proc drawTest

    ; Set ctrl-y vector
    lda     #$4c        ; JMP
    sta     $3f8
    lda     #<quit
    sta     $3f9
    lda     #>quit
    sta     $3fa

    jsr     engineInit    ; init code

    jsr     clearScreen

    ; init DHGR (monochrome)


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

    sta     CLR80COL        ; Use RAMWRT for aux mem

    ; Testing 28x8 mask

TEST_XOFFSET := 1
TEST_YOFFSET := 1

TEST_WIDTH := 4
TEST_HEIGHT := 2

    ldx     #0
    stx     testIdx

    lda     #TEST_YOFFSET
    sta     tileY

loopY:
    lda     #TEST_XOFFSET
    sta     tileX

loopX1:
    ldx     testIdx
    lda     testMap0,x
    jsr     drawTile_28x8
    ldx     testIdx
    lda     testMap1,x
    jsr     drawTileMask_28x8
    inc     testIdx

    clc
    lda     tileX
    adc     #2
    sta     tileX
    cmp     #TEST_XOFFSET+TEST_WIDTH*4
    bmi     loopX1

    inc     tileY
    lda     tileY
    cmp     #TEST_YOFFSET+TEST_HEIGHT*2+2
    bmi     loopY

    ; Exit to monitor
    jmp     MONZ        ; enter monitor

testIdx:    .byte   0
testX:      .byte   0
testY:      .byte   0

testMap0:   ; 5x5
    .byte       $02,  $02,  $02,  $02,  $02,  $02,  $02,  $02
    .byte       $02,  $02,  $1c,  $1e,  $1c,  $1e,  $1c,  $1e
    .byte       $02,  $1c,  $1e,  $1c,  $1e,  $1c,  $1e,  $24
    .byte       $1c,  $1e,  $1c,  $1e,  $1c,  $1e,  $1c,  $1e
    .byte       $20,  $02,  $02,  $22,  $20,  $22,  $20,  $22
    .byte       $02,  $02,  $02,  $02,  $02,  $02,  $02,  $02

testMap1:   ; 5x5
    .byte       $00,  $00,  $18,  $1a,  $18,  $1a,  $18,  $1a
    .byte       $00,  $18,  $1a,  $18,  $1a,  $18,  $1a,  $00
    .byte       $18,  $1a,  $18,  $1a,  $18,  $1a,  $18,  $1a
    .byte       $00,  $28,  $2a,  $00,  $00,  $00,  $00,  $00
    .byte       $00,  $2c,  $2e,  $00,  $00,  $00,  $00,  $00
    .byte       $24,  $00,  $00,  $26,  $24,  $26,  $24,  $26


clearScreen:
    lda     #$00
    sta     screenPtr0
    lda     #$20
    sta     screenPtr1

    sta     CLR80COL        ; Use RAMWRT for aux mem

    lda     #$ff
    sta     pattern

loop:
    ldy     #0

    ; aux mem
    sta     RAMWRTON

    lda     #0
:
    lda     #$2a
    eor     pattern
    sta     (screenPtr0),y
    iny
    bne     :-

    sta     RAMWRTOFF

    ; main mem
:
    lda     #$55
    eor     pattern
    sta     (screenPtr0),y
    iny
    bne     :-

    inc     screenPtr1

    lda     screenPtr1
    and     #$3
    bne     :+
    lda     pattern
    eor     #$ff
    sta     pattern
:

    lda     #$40
    cmp     screenPtr1
    bne     loop
    rts

pattern:    .byte   0
.endproc

;-----------------------------------------------------------------------------
; Quit
;
;   Exit to ProDos
;-----------------------------------------------------------------------------
.proc quit

    jsr     MLI
    .byte   CMD_QUIT
    .word   quit_params

quit_params:
    .byte   4               ; 4 parameters
    .byte   0               ; 0 is the only quit type
    .word   0               ; Reserved pointer for future use (what future?)
    .byte   0               ; Reserved byte for future use (what future?)
    .word   0               ; Reserved pointer for future use (what future?)


.endproc