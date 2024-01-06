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
    jmp     drawTileBG_28x8
    jmp     drawPixel4x4
    jmp     scrollLine
    jmp     getPixelMask_28x8
    jmp     setPixel_28x8
    jmp     setMask_28x8
    jmp     dumpInit
    jmp     dumpByte
    jmp     setByte
    jmp     clearScreen
    jmp     drawImage
    jmp     drawString

; Variables (in fixed locations)
; xx40
.align 64
tileSheet_7x8:          .word   $B000
tileSheet_28x8:         .word   $8000

imageWidth:             .byte   10
imageHeight:            .byte   8
imageX:                 .byte   1       ; [0..39-width]
imageY:                 .byte   2       ; [0..23-height]

imageTable:             .word   $B000+640*0
                        .word   $B000+640*1
                        .word   $B000+640*2
                        .word   $B000+640*3
                        .word   $B000+640*4
                        .word   $B000+640*5
.align 64

; xx80
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

; x84
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

    ; background pattern
    lda     #$2a
    sta     bgPattern00
    lda         #$55    
    sta     bgPattern01
    lda     #$55
    sta     bgPattern10
    lda         #$2a
    sta     bgPattern11

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
    adc     drawPage
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
;    bne     :+
;    rts
;:
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
    sta     screen2Ptr0
    lda     linePage,x
    adc     drawPage
    sta     screenPtr1
    sta     screen2Ptr1

    jsr     drawTile

    ; restore tile pointers (page byte doesn't change)
    lda     tilePtr0Copy
    sta     tilePtr0

    ; restore screen pointer
    lda     screen2Ptr0
    sta     screenPtr0
    lda     screen2Ptr1
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

.endproc

;-----------------------------------------------------------------------------
; Draw Tile w/Mask 28x8
;
; 4*8*2 = 32 bytes (16 main / 16 aux) + 32 bytes mask (16 main / 16 aux)
;
;
; Assumes BG called previous to calculate screen pointer.
;   
;-----------------------------------------------------------------------------
.proc drawTileMask_28x8

    bne     :+
    rts                     ; tile 0 is skip
:

    sta     tileIdx
    ; calculate tile pointer
    asl                     ; *32
    asl
    asl
    asl
    asl
    sta     tilePtr0
    sta     tilePtr0Copy
    clc
    adc     #16
    sta     maskPtr0
    lda     tileIdx
    lsr                     ; /8
    lsr
    lsr
    clc
    adc     tileSheet_28x8+1
    sta     tilePtr1
    sta     maskPtr1

;    ; Reusing screen pointer from BG, so no need to recompute
    lda     screen2Ptr0
    sta     screenPtr0
    lda     screen2Ptr1
    sta     screenPtr1

    jsr     drawTile


    ; restore tile pointers (page byte doesn't change)
    lda     tilePtr0Copy
    sta     tilePtr0
    clc
    adc     #16
    sta     maskPtr0

    ; restore screen pointer

    ; Ptr0 not changed, so no need to restore
    lda     screen2Ptr1
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
; Draw Tile w/Mask 28x8 using background
;
; 4*8*2 = 32 bytes (16 main / 16 aux) + 32 bytes mask (16 main / 16 aux)
;   
;-----------------------------------------------------------------------------
.proc drawTileBG_28x8

    sta     tileIdx
    ; calculate tile pointer
    asl                     ; *32
    asl
    asl
    asl
    asl
    sta     tilePtr0
    sta     tilePtr0Copy
    clc
    adc     #16
    sta     maskPtr0
    lda     tileIdx
    lsr                     ; /8
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
    sta     screen2Ptr0
    lda     linePage,x
    adc     drawPage
    sta     screenPtr1
    sta     screen2Ptr1

    jsr     drawTile


    ; restore tile pointers (page byte doesn't change)
    lda     tilePtr0Copy
    sta     tilePtr0
    clc
    adc     #16
    sta     maskPtr0

    ; swap background
    ldy     bgPattern00
    lda     bgPattern01
    sta     bgPattern00
    sty     bgPattern01
    ldy     bgPattern10
    lda     bgPattern11
    sta     bgPattern10
    sty     bgPattern11

    ; restore screen pointer

    ; Ptr0 not changed, so no need to restore
    lda     screen2Ptr1
    sta     screenPtr1

    ; transfer to aux memory
    sta     RAMWRTON  
    sta     RAMRDON  

    ; draw half of tile in aux mem
    jsr     drawTile    ; AUX

    sta     RAMWRTOFF   ; AUX
    sta     RAMRDOFF    ; AUX

    ; swap background
    ldy     bgPattern00
    lda     bgPattern01
    sta     bgPattern00
    sty     bgPattern01
    ldy     bgPattern10
    lda     bgPattern11
    sta     bgPattern10
    sty     bgPattern11

    rts

drawTile:

    clc     ; no carry generated inside of loop
    ldx     #8  ; 8 lines

drawLoop:
    ldy     #0
    lda     bgPattern01
    and     (maskPtr0),y
    ora     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     bgPattern01
    and     (maskPtr0),y
    ora     (tilePtr0),y
    sta     (screenPtr0),y


    ; swap background
    ldy     bgPattern01
    lda     bgPattern11
    sta     bgPattern01
    sty     bgPattern11

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

.endproc


;-----------------------------------------------------------------------------
; get pixel
;   get pixel pointed to by tileX, tileY
;-----------------------------------------------------------------------------
.proc getPixelMask_28x8

    jsr     pixelTilePtr_28x8

    lda     #0
    sta     pixelResult

    ldx     tileX
    lda     pixelBank,x
    bne     :+
    sta     RAMRDON         ; aux if even
:

    ldx     tileX
    ldy     pixelOffset,x
    lda     pixelMask,x
    and     (maskPtr0),y
    beq     :+
    lda     #2
    sta     pixelResult
:
    lda     pixelMask,x
    and     (tilePtr0),y
    beq     :+
    lda     #1
    ora     pixelResult
    sta     pixelResult
:
    sta     RAMRDOFF        ; done
    rts

.endproc


.proc pixelTilePtr_28x8
    lda     tileIdx
    ; calculate tile pointer
    asl                     ; *32
    asl
    asl
    asl
    asl
    clc
    adc     tileY           ; + y*2
    adc     tileY
    sta     tilePtr0
    adc     #16
    sta     maskPtr0
    lda     tileIdx
    lsr                     ; /8
    lsr
    lsr
    clc
    adc     tileSheet_28x8+1
    sta     tilePtr1
    sta     maskPtr1
    rts
.endproc

pixelBank:
    .byte   $00,$00,$00,$00,$00,$00,$00     ;  0..6  -> Aux
    .byte   $01,$01,$01,$01,$01,$01,$01     ;  7..13 -> Main
    .byte   $00,$00,$00,$00,$00,$00,$00     ; 14..20 -> Aux
    .byte   $01,$01,$01,$01,$01,$01,$01     ; 21..27 -> Main

pixelOffset:
    .byte   $00,$00,$00,$00,$00,$00,$00     ;  0..6  -> Aux
    .byte   $00,$00,$00,$00,$00,$00,$00     ;  7..13 -> Main
    .byte   $01,$01,$01,$01,$01,$01,$01     ; 14..20 -> Aux
    .byte   $01,$01,$01,$01,$01,$01,$01     ; 21..27 -> Main

pixelMask:
    .byte   $01,$02,$04,$08,$10,$20,$40     ;  0..6  -> Aux
    .byte   $01,$02,$04,$08,$10,$20,$40     ;  7..13 -> Main
    .byte   $01,$02,$04,$08,$10,$20,$40     ; 14..20 -> Aux
    .byte   $01,$02,$04,$08,$10,$20,$40     ; 21..27 -> Main

;-----------------------------------------------------------------------------
; set pixel
;   set pixel pointed to by tileX, tileY and value passed in A
;   A      = 00    -- set pixel to 0 (black), don't change mask
;          = ff    -- set pixel to 1 (white), don't change mask
;          all other values reserved
;          
;-----------------------------------------------------------------------------
.proc setPixel_28x8
    sta     pixelResult

    jsr     pixelTilePtr_28x8

    ldx     tileX
    ldy     pixelOffset,x

    lda     pixelBank,x
    bne     :+
    sta     RAMRDON         ; aux if even
    sta     RAMWRTON        ; aux if even
:

    lda     pixelMask,x
    and     pixelResult
    sta     pixelResult

    lda     pixelMask,x
    eor     #$ff
    and     (tilePtr0),y
    ora     pixelResult
    sta     (tilePtr0),y

    sta     RAMRDOFF        ; back to main memory
    sta     RAMWRTOFF       ; back to main memory
    rts
.endproc

.proc setMask_28x8
    sta     pixelResult

    jsr     pixelTilePtr_28x8


    ldx     tileX
    ldy     pixelOffset,x

    lda     pixelBank,x
    bne     :+
    sta     RAMRDON         ; aux if even
    sta     RAMWRTON        ; aux if even
:

    lda     pixelMask,x
    and     pixelResult
    sta     pixelResult

    lda     pixelMask,x
    eor     #$ff
    and     (maskPtr0),y
    ora     pixelResult
    sta     (maskPtr0),y

    sta     RAMRDOFF        ; back to main memory
    sta     RAMWRTOFF       ; back to main memory
    rts
.endproc


;-----------------------------------------------------------------------------
; dumpInit
;   Set up pointers for tile passed in A
;-----------------------------------------------------------------------------
.proc dumpInit
    sta     tileIdx
    lda     #0
    sta     tileX
    sta     tileY  
    jsr     pixelTilePtr_28x8
    rts
.endproc

;-----------------------------------------------------------------------------
; dumpByte
;   Return the Yth byte of the tile
;
; Handles the interleave between aux/main
;-----------------------------------------------------------------------------
.proc dumpByte
    tya
    and     #1
    bne     :+              ; If bit 0 set, main memory
    sta     RAMRDON         ; aux
:
    tya
    lsr                     ; /2
    tay
    lda     (tilePtr0),y
    sta     RAMRDOFF
    rts

.endproc

;-----------------------------------------------------------------------------
; setByte
;   Set the Yth byte of the tile to A
;
; Note that this function maintains the 2-byte interleave, but since nothing
; is taking advantage of that, it may be better to go to a more straight forward
; 1 byte interleave.
;-----------------------------------------------------------------------------
.proc setByte
    sta     temp
    sta     CLR80COL        ; Use RAMWRT for aux mem
    tya
    and     #1
    bne     :+              ; If bit 1 set, main memory
    sta     RAMWRTON        ; aux
:
    tya
    lsr
    tay
    lda     temp
    sta     (tilePtr0),y
    sta     RAMWRTOFF
    rts

temp:       .byte   0
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


;-----------------------------------------------------------------------------
; scroll line
;   Scroll 8 lines up in box defined by tileX,tileY,tileX2,tileY2
;   Where x can range from 0..39 and y 0..23
;-----------------------------------------------------------------------------
.proc scrollLine

    sta     CLR80COL        ; Use RAMWRT for aux mem

    ldx     tileY           ; top line

loop:
    ; Initial screen pointer
    lda     lineOffset,x
    sta     screenPtr0
    lda     linePage,x
    clc
    adc     drawPage
    sta     screenPtr1

    inx                     ; next line

    lda     lineOffset,x
    sta     screen2Ptr0
    lda     linePage,x
    clc
    adc     drawPage
    sta     screen2Ptr1

    ldx     #0

loop8:
    ldy     tileX
:
    lda     (screen2Ptr0),y
    sta     (screenPtr0),y

    iny
    cpy     tileX2
    bcc     :-
    beq     :-          ; one more!

    ; transfer to aux memory
    sta     RAMWRTON  
    sta     RAMRDON  

    ldy     tileX
:
    lda     (screen2Ptr0),y
    sta     (screenPtr0),y

    iny
    cpy     tileX2
    bcc     :-
    beq     :-          ; one more!

    ; back to main
    sta     RAMWRTOFF
    sta     RAMRDOFF

    ; line line
    clc
    lda     screenPtr1
    adc     #$4
    sta     screenPtr1
    lda     screen2Ptr1
    adc     #$4
    sta     screen2Ptr1

    inx
    cpx     #8
    bne     loop8

    inc     tileY
    ldx     tileY
    cpx     tileY2
    bcc     loop

    rts

.endproc

auxMemEnd:

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
    lda     lineOffset,x
    clc
    adc     imageX
    sta     screenPtr0
    lda     linePage,x
    adc     drawPage
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
; clearScreen
;
;-----------------------------------------------------------------------------
; 00 00 -- black
; 00 00 
;
; 2a 55  - 25%
; 00 00
;
; 7f 7f -- horizontal
; 00 00 
;
; 2a 55 -- vertical
; 2a 55
;
; 2a 55 -- checkered
; 55 2a                  
;
; 7f 7f -- 75%      
; 2a 55                  
;
; 7f 7f -- white    
; 7f 7f                  
;

.proc clearScreen
    lda     #$00
    sta     screenPtr0
    lda     #$20
    clc
    adc     drawPage
    sta     screenPtr1

    sta     CLR80COL        ; Use RAMWRT for aux mem

loop:
    ldy     #0

    lda     bgPattern00
    ; aux mem
    sta     RAMWRTON
:
    sta     (screenPtr0),y
    iny
    bne     :-

    sta     RAMWRTOFF

    lda     bgPattern01
    ; main mem
:
    sta     (screenPtr0),y
    iny
    bne     :-

    inc     screenPtr1

    lda     screenPtr1
    and     #$3
    bne     :+

    ; swap colors on odd rows

    ldx     bgPattern10
    lda     bgPattern00
    sta     bgPattern10
    stx     bgPattern00

    ldx     bgPattern11
    lda     bgPattern01
    sta     bgPattern11
    stx     bgPattern01
:

    lda     screenPtr1
    and     #$1f
    bne     loop

    rts

.endproc


; Lookup tables
;-----------------------------------------------------------------------------

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