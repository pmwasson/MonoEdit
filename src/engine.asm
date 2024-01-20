;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
;  Game engine / loader
;-----------------------------------------------------------------------------
; Low level routines that deal with main/aux memory
; - The idea is that this code will remain in memory and load in
;   other assets and execuable code as needed.
; - Some of the code is copied into AUX memory such that the read
;   bank can be freely changed and the code will continue to execute allowing
;   the code to read & write both main and AUX memory.

; Proposed memory map (may change)
;------------------------------------------------
;
;               Main                Aux
;
;   0000-07FF   [ System usage / text pages     ]
;
;   0800-09FF   [ ProDos buffer ][ Unused?      ]
;   0A00-0BFF   [ ???                           ]
;
;   0C00-1FFF   [ Engine / Loader               ]
;
;   2000-3FFF   [ DGHR Page 1                   ]
;               [ Read data     ]
;
;   4000-5FFF   [ DGHR Page 2                   ]
;               [ Read data     ]
;
;   6000-7FFF   [ Game / Tools  ][ Level Data   ]
;        7FFF
;   8000-AFFF   [ Isometric Tiles (192)         ]
;
;   B000-B7FF   [ Level Map     ][ Font x2      ]
;   B800-BEFF   [ "             ]
;
;   ProDos says addresses D000-FFFF in AUX memory
;   are reserved, but probably could be used if needed.

.include "defines.asm"
.include "macros.asm"

.segment "CODE"
.org    $C00

;------------------------------------------------
; Constants
;------------------------------------------------

; Reuse string input buffer ($200) as copy buffer to save space
copyBuffer :=   $200

;------------------------------------------------
; Fixed location
;------------------------------------------------

; Jump table (in fixed locations)
    jmp     loader
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
    jmp     drawIndexImage
    jmp     drawImage
    jmp     drawImageAux
    jmp     drawString
    jmp     drawStringInline
    jmp     loaderMenu
    jmp     loadToolAsset
    jmp     storeToolAsset

; Variables (in fixed locations)
; xx60
.align 32
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

auxMemStart:
; This code is copied such that it exists in both main and aux memory.

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

    ; init drawing variables
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

    stx     tempZP
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
    ldx     tempZP

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
    stx     tempZP
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

    ldx     tempZP
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

;-----------------------------------------------------------------------------
; Draw Image Aux
;
;   Draw image stored in Aux memory
;-----------------------------------------------------------------------------
.proc drawImageAux
    sta     CLR80COL        ; Use RAMWRT for aux mem

    ; image parameters in main mem, but need results in AUX
    sta     RAMWRTON            ; read main, write aux
    lda     imageY
    tax
    clc
    adc     imageHeight
    sta     imageEnd

    ; copy parameters to AUX
    lda     imageX
    sta     imageXAux
    lda     imageWidth
    sta     imageWidthAux

    sta     RAMRDON             ; now read/write AUX

loopY:
    lda     lineOffset,x        ; AUX copy
    clc
    adc     imageXAux           ; AUX
    sta     screenPtr0          ; ZP
    lda     linePage,x          ; AUX copy
    adc     drawPage            ; ZP
    sta     screenPtr1          ; ZP

    lda     #8
    sta     lineCount           ; AUX
loop8:

    ldy     #0
loopX:
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    sta     RAMWRTOFF           ; main
    lda     (maskPtr0),y
    sta     (screenPtr0),y
    sta     RAMWRTON            ; aux
    iny
    cpy     imageWidthAux
    bne     loopX

    ; increment pointers

    clc
    lda     imageWidthAux
    adc     tilePtr0
    sta     tilePtr0
    lda     #0
    adc     tilePtr1
    sta     tilePtr1

    clc
    lda     imageWidthAux
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

    ; switch back to main
    sta     RAMWRTOFF           ; main
    sta     RAMRDOFF            ; main

    rts

lineCount:      .byte   0
imageEnd:       .byte   0
imageXAux:      .byte   0
imageWidthAux:  .byte   0
.endproc

;-----------------------------------------------------------------------------
; Read Script Byte
;   Read a byte pointed to by scriptPtr0 out of aux memory and
;   increment pointer.
;-----------------------------------------------------------------------------

.proc readScriptByte

    ldy     #0
    sta     RAMRDON     ; Read from AUX
    lda     (scriptPtr0),y
    inc     scriptPtr0
    bne     :+
    inc     scriptPtr1
:
    rts
.endProc

auxMemEnd:


;-----------------------------------------------------------------------------
; Draw Index Image
;-----------------------------------------------------------------------------
.proc drawIndexImage
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
    jmp     drawImage
.endproc

;-----------------------------------------------------------------------------
; Draw Image
;
;   Data is split between even and odd using tilePtr and maskPtr
;   imageHeight and imageWith define the image size.
;-----------------------------------------------------------------------------
.proc drawImage
    sta     CLR80COL        ; Use RAMWRT for aux mem

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
;   Use tileX and tileY for start
;-----------------------------------------------------------------------------

.proc drawString

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
    pha                     ; save Y
    lda     (stringPtr0),y
    bne     :+

    pla     ; clean up stack
    rts
:
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

offset: .byte   0
.endproc

;-----------------------------------------------------------------------------
; Draw String
;
;   Use tileX and tileY for start and string inlined
;-----------------------------------------------------------------------------

.proc drawStringInline

    ; Pop return address to find string
    pla
    sta     stringPtr0
    pla
    sta     stringPtr1
    ldy     #0

    jsr     drawString

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

;=============================================================================
; Loader
;=============================================================================

.proc loader

    ; Set up text screen
    jsr     $c300       ; 80 column mode
    jsr     HOME        ; clear screen
    lda     #23         ; put cursor on last line
    sta     CV
    jsr     VTAB

    ; Clear loader errors
    lda     #0
    sta     fileError

    jsr    inline_print
    StringCont "Welcome to 128k game loader. Press ESC for load options."
    StringCR   "Checking memory..."

    lda     $BF98
    bmi     :+

    jsr    inline_print
    StringCR "128K memory not detected, exiting"
    jmp     monitor
:

    jsr    inline_print
    StringCR "Initializing..."
    jsr     engineInit

loadAssets:
    jsr    inline_print
    StringCR "Loading game assets..."

    ldx     #assetFont0
    jsr     loadAsset
    ldx     #assetFont1
    jsr     loadAsset
    ldx     #assetISO
    jsr     loadAsset
    ldx     #assetImage
    jsr     loadAsset

    lda     fileError
    beq     :+

    jsr     inline_print
    StringCR "Error detected"
    jmp     monitor
:
    ; Link assets
    lda     #<ISOSTART
    sta     DHGR_TILE_28X8
    lda     #>ISOSTART
    sta     DHGR_TILE_28X8+1

    lda     #<FONT0START
    sta     DHGR_TILE_7X8
    lda     #>FONT0START
    sta     DHGR_TILE_7X8+1

    lda     KBD
    bpl     :+
    sta     KBDSTRB

    cmp     #KEY_ESC
    beq     loaderMenu
:
    jmp     loadGame
.endproc

.proc loaderMenu
    sta     TXTSET
    jsr     inline_print
    StringCont "Options:"
    StringCont " [RET] Game"
    StringCont " [0]   Font Editor"
    StringCont " [1]   Tile Editor"
    StringCont " [2]   Map Editor"
    StringCont " [3]   Display Image"
    StringCont " [7]   Reload assets"
    StringCont " [8]   Monitor"
    StringCont " [9]   ProDos"
    String "Select option:"

menuLoop:
    jsr     RDKEY
    and     #$7f
    cmp     #13
    bne     :+
    jmp     loadGame
:
    cmp     #'0'
    beq     loadTool
    cmp     #'1'
    beq     loadTool
    cmp     #'2'
    beq     loadTool
    cmp     #'3'
    beq     loadTool

    cmp     #'7'
    bne     :+
    jmp     loader::loadAssets
:

    cmp     #'8'
    bne     :+
    jmp     monitor
:

    cmp     #'9'
    bne     :+
    jmp     quit
:
    jmp     menuLoop

.endproc

.proc loadTool
    sta     fileNameToolEnd-1   ; Overwrite final digit

    ldx     #assetTool
    jsr     loadAsset

    lda     fileError
    beq     :+

    jsr     inline_print
    StringCR "Error detected"
    jmp     monitor
:

    jsr     inline_print
    StringCR "Launching tool..."

    ; Jump to executables
    jmp     EXECSTART

.endproc

.proc loadGame

    ldx     #assetTitle0
    jsr     loadAsset
    ldx     #assetTitle1
    jsr     loadAsset
    ldx     #assetGame
    jsr     loadAsset

    lda     fileError
    beq     :+

    jsr     inline_print
    StringCR "Error detected"
    jmp     monitor
:
    jsr     inline_print
    StringCR "Launching game..."

    ; Jump to executables
    jmp     EXECSTART

.endproc

;-----------------------------------------------------------------------------
; Monitor
;
;  Exit to monitor
;-----------------------------------------------------------------------------
.proc monitor

    ; Set ctrl-y vector
    lda     #$4c        ; JMP
    sta     $3f8
    lda     #<quit
    sta     $3f9
    lda     #>quit
    sta     $3fa

    jsr    inline_print
    StringCR "Enter ctrl-y to quit to ProDos"

    bit     TXTSET
    jmp     MONZ        ; enter monitor

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
.endproc

;-----------------------------------------------------------------------------
; Load Data
;   Load data using ProDOS
;-----------------------------------------------------------------------------
.proc loadData

    jsr    inline_print
    String "Reading "

    lda     open_params+1
    sta     stringPtr0
    lda     open_params+2
    sta     stringPtr1
    jsr     print_length

    lda     #13
    jsr     COUT

    ; open file
    jsr     MLI
    .byte   CMD_OPEN
    .word   open_params
    bcc     :+

    jsr    inline_print
    StringCR "File not found"
    inc     fileError
    rts
:

    ; set reference number
    lda     open_params+5
    sta     rw_params+1
    sta     close_params+1

    ; read data
    jsr    MLI
    .byte  CMD_READ
    .word  rw_params
    bcc    :+

    jsr    inline_print
    StringCR "Read Error"
    inc     fileError
    rts
:

    jsr    MLI
    .byte  CMD_CLOSE
    .word  close_params
    bcc    :+

    jsr    inline_print
    StringCR "File close error"
    inc     fileError
:
    rts

.endproc

;-----------------------------------------------------------------------------
; Store Data
;   Load data using ProDOS
;-----------------------------------------------------------------------------
.proc storeData

    jsr    inline_print
    String "Writing "

    lda     open_params+1
    sta     stringPtr0
    lda     open_params+2
    sta     stringPtr1
    jsr     print_length

    lda     #13
    jsr     COUT

    ; open file
    jsr     MLI
    .byte   CMD_OPEN
    .word   open_params
    bcc     openGood

    jsr    inline_print
    StringCR "File not found, creating new"

    ; create file
    jsr     MLI
    .byte   CMD_CREATE
    .word   create_params
    bcc     :+

    jsr    inline_print
    .byte  "Unable to create file"
    inc     fileError
    rts
:

    ; open file again!
    jsr     MLI
    .byte   CMD_OPEN
    .word   open_params
    bcc     openGood

    jsr    inline_print
    StringCR "Unable to open new file"
    inc     fileError
    rts

openGood:

    ; set reference number
    lda     open_params+5
    sta     rw_params+1
    sta     close_params+1

    ; write data
    jsr    MLI
    .byte  CMD_WRITE
    .word  rw_params
    bcc    :+

    jsr    inline_print
    StringCR "Write Error"
    inc     fileError
    rts
:

    jsr    MLI
    .byte  CMD_CLOSE
    .word  close_params
    bcc    :+

    jsr    inline_print
    StringCR "File close error"
    inc     fileError
:
    rts

.endproc


;-----------------------------------------------------------------------------
; Init Asset
;
;   Pass asset # * 16 in X
;-----------------------------------------------------------------------------
.proc initAsset

    ldx     assetNum

    lda     fileDescription+0,x
    sta     stringPtr0
    lda     fileDescription+1,x
    sta     stringPtr1
    jsr     print

    jsr    inline_print
    .byte  ":",13,"  ",0

    ldx     assetNum

    ; set pathname
    lda     fileDescription+2,x
    sta     open_params+1
    sta     create_params+1
    lda     fileDescription+3,x
    sta     open_params+2
    sta     create_params+2

    ; set address
    lda     fileDescription+4,x
    sta     rw_params+2
    lda     fileDescription+5,x
    sta     rw_params+3

    ; set size
    lda     fileDescription+6,x
    sta     rw_params+4
    lda     fileDescription+7,x
    sta     rw_params+5

    rts

.endproc

;-----------------------------------------------------------------------------
; Replace final character of filename with passed in value
;   Pass asset # * 16 in X
;   final character A
;   (X preserved)
;-----------------------------------------------------------------------------

.proc modifyFilename
    sta     finalCharacter

    ; modify filename
    lda     fileDescription+2,x
    sta     stringPtr0
    lda     fileDescription+3,x
    sta     stringPtr1
    ldy     #0
    lda     (stringPtr0),y
    tay
    lda     finalCharacter
    sta     (stringPtr0),y
    rts

finalCharacter:     .byte   0

.endproc

;-----------------------------------------------------------------------------
; Load Tool Asset
;
;   Pass asset # * 16 in X
;   Slot # in A
;-----------------------------------------------------------------------------
.proc loadToolAsset
    stx     assetNum
    clc
    adc     #'0'
    jsr     modifyFilename
    jsr     loadAsset
    rts
.endproc

;-----------------------------------------------------------------------------
; Store Tool Asset
;
;   Pass asset # * 16 in X
;   Slot # in A
;-----------------------------------------------------------------------------
.proc storeToolAsset
    stx     assetNum
    clc
    adc     #'0'
    jsr     modifyFilename
    jsr     storeAsset
    rts
.endproc

;-----------------------------------------------------------------------------
; Load Asset
;
;   Pass asset # * 16 in X
;-----------------------------------------------------------------------------

.proc loadAsset

    stx     assetNum
    jsr     initAsset
    jsr     loadData

    lda     fileError
    beq     :+
    ; Error -- abort load
    rts
:
    jsr     inline_print
    String "  Installing data to location "

    ldx     assetNum
    lda     fileDescription+12,x
    bne     :+

    ;       #INSTALL_MAIN

    jsr     inline_print
    String "(main) $"
    jsr     printDest

    rts     ; For main memory, just load to correct location
:

    cmp     #INSTALL_AUX
    bne     :+

    jsr     inline_print
    String "(aux) $"
    jsr     printDest

    jsr     setLoadCopyParam
    sec                     ; copy from main to aux
    jsr     AUXMOVE

    rts
:

    ; Note that install both is same as install aux just not using the read buffer
    cmp     #INSTALL_BOTH
    bne     :+

    jsr     inline_print
    String "(main/aux duplicated) $"
    jsr     printDest

    jsr     setLoadCopyParam
    sec                     ; copy from main to aux
    jsr     AUXMOVE

    rts
:

    cmp     #INSTALL_AUX_I1
    bne     :+
    jsr     inline_print
    String "(main/aux interleave 1) $"
    jsr     printDest

    jsr     setLoadCopyParam
    jsr     interleaveCopy1A

    jsr     setLoadCopyParamInterleave
    sec
    jsr     AUXMOVE

    jsr     setLoadCopyParam
    jsr     interleaveCopy1B


    rts
:
    brk     ; Unknown type

moveCopyBuffer:

    ldy     #0
:
    lda     copyBuffer,y
    sta     (A4),y
    dey
    bne     :-

    inc     A4+1

    rts

interleaveCopy1A:
    ldy     #0
    ldx     #0

copyLoop1A:
    ; copy 1 byte
    lda     (A1),y
    sta     copyBuffer,x
    inx
    iny
    ; skip 1 bytes
    iny
    bne     copyLoop1A

    ; inc source page
    inc     A1+1

    ; check if buffer full
    cpx     #0
    bne     copyLoop1A

    jsr     moveCopyBuffer

    ; check if done
    dec     copyLength
    bne     copyLoop1A

    rts

interleaveCopy1B:
    ldy     #0
    ldx     #0

copyLoop1B:
    ; skip 1 byte
    iny
    ; copy 1 bytes
    lda     (A1),y
    sta     copyBuffer,x
    inx
    iny
    bne     copyLoop1B

    ; inc source page
    inc     A1+1

    ; check if buffer full
    cpx     #0
    bne     copyLoop1B

    jsr     moveCopyBuffer

    ; check if done
    dec     copyLength
    bne     copyLoop1B
    rts

setLoadCopyParam:
    ldx     assetNum

    ; start
    lda     fileDescription+4,x
    sta     A1
    lda     fileDescription+5,x
    sta     A1+1

    ; end
    lda     fileDescription+8,x
    sta     A2
    lda     fileDescription+9,x
    sta     A2+1

    ; destination (aux)
    lda     fileDescription+10,x
    sta     A4
    lda     fileDescription+11,x
    sta     A4+1

    ; for interleave
    lda     fileDescription+7,x     ; length page
    lsr                             ; /2
    sta     copyLength

    rts

setLoadCopyParamInterleave:
    ldx     assetNum

    ; start
    lda     fileDescription+10,x
    sta     A1
    lda     fileDescription+11,x
    sta     A1+1

    ; end
    lda     fileDescription+14,x
    sta     A2
    lda     fileDescription+15,x
    sta     A2+1

    ; destination (aux)
    lda     fileDescription+10,x
    sta     A4
    lda     fileDescription+11,x
    sta     A4+1

    rts

copyLength:     .byte   0

.endProc

.proc printDest
    ldx     assetNum
    lda     fileDescription+10,x
    ldy     fileDescription+11,x
    tax
    jsr     PRINTXY
    lda     #13
    jsr     COUT
    rts

.endproc

;-----------------------------------------------------------------------------
; store Asset
;
;   Pass asset # * 16 in X
;-----------------------------------------------------------------------------

.proc storeAsset
    stx     assetNum
    jsr     initAsset

    jsr     inline_print
    String "  Retrieving data from location "

    ldx     assetNum
    lda     fileDescription+12,x
    bne     :+


    ;       #INSTALL_MAIN

    jsr     inline_print
    String "(main) $"
    jsr     printDest
    jsr     storeData
    rts     ; For main memory, just save from correct location
:

    cmp     #INSTALL_AUX
    bne     :+

    jsr     inline_print
    String "(aux) $"
    jsr     printDest

    jsr     setStoreCopyParam
    clc                     ; copy from aux to main
    jsr     AUXMOVE

    rts
:

    ; Note that install both is same as install aux
    cmp     #INSTALL_BOTH
    bne     :+

    jsr     inline_print
    String "(main/aux duplicated) $"
    jsr     printDest
    jsr     storeData
    rts     ; Assuming main data is sufficient
:

    cmp     #INSTALL_AUX_I1
    bne     :+
    jsr     inline_print
    String "(main/aux interleave 1) $"
    jsr     printDest

    jsr     setStoreCopyParam
    jsr     interleaveCopy1B    ; main

    jsr     setStoreCopyParamInterleave
    clc                     ; copy from aux to main
    jsr     AUXMOVE

    jsr     setStoreCopyParam
    jsr     interleaveCopy1A    ; aux

    jsr     storeData

    ; Need to restore data
    jsr     inline_print
    StringCR "Reloading stored data"
    ldx     assetNum
    jsr     loadAsset
    rts
:
    brk     ; Unknown type

moveCopyBuffer:                 ; store

    ldy     #0
:
    lda     (A1),y              ; Interleaved
    sta     copyBuffer,y
    dey
    bne     :-

    inc     A1+1

    rts

interleaveCopy1A:

    jsr     moveCopyBuffer
    ldy     #0
    ldx     #0

copyLoop1A:

    ; copy 1 bytes
    lda     copyBuffer,x
    inx
    sta     (A4),y
    iny
    ; skip 1 byte
    iny
    bne     copyLoop1A

    ; inc source page
    inc     A4+1

    ; check if buffer done
    cpx     #0
    bne     copyLoop1A

    ; check if done
    dec     copyLength
    bne     interleaveCopy1A
    rts

interleaveCopy1B:

    jsr     moveCopyBuffer

    ldy     #0
    ldx     #0

copyLoop1B:

    ; copy 1 bytes
    lda     copyBuffer,x
    inx
    ; skip 1 byte
    iny
    sta     (A4),y
    iny
    bne     copyLoop1B

    ; inc source page
    inc     A4+1

    ; check if buffer done
    cpx     #0
    bne     copyLoop1B

    ; check if done
    dec     copyLength
    bne     interleaveCopy1B
    rts

setStoreCopyParam:
    ldx     assetNum

    ; start (aux)
    lda     fileDescription+10,x
    sta     A1
    lda     fileDescription+11,x
    sta     A1+1

    ; end
    lda     fileDescription+14,x
    sta     A2
    lda     fileDescription+15,x
    sta     A2+1

    ; destination (main)
    lda     fileDescription+4,x
    sta     A4
    lda     fileDescription+5,x
    sta     A4+1

    ; for interleave
    lda     fileDescription+7,x     ; length page
    lsr                             ; /2
    sta     copyLength

    rts

setStoreCopyParamInterleave:
    ldx     assetNum

    ; start
    lda     fileDescription+10,x
    sta     A1
    lda     fileDescription+11,x
    sta     A1+1

    ; end
    lda     fileDescription+14,x
    sta     A2
    lda     fileDescription+15,x
    sta     A2+1

    ; destination (aux)
    lda     fileDescription+10,x
    sta     A4
    lda     fileDescription+11,x
    sta     A4+1

    rts

copyLength:     .byte   0

.endproc

;-----------------------------------------------------------------------------
; Global ProDos parameters
;-----------------------------------------------------------------------------

fileError:  .byte   0
assetNum:   .byte   0

open_params:
    .byte   $3              ; 3 parameters
    .word   $0              ; pathname*
    .word   FILEBUFFER      ; ProDos buffer
    .byte   $0              ; reference number

; Note, not using the real address for the binary file load address as
; the data buffer may move around with re-compiles, so we don't
; want to rely on it.
create_params:
    .byte   $7              ; 7 parameters
    .word   $0              ; pathname*
    .byte   $C3             ; access bits (full access)
    .byte   $6              ; file type (binary)
    .word   READBUFFER      ; binary file load address, default to $2000 (READBUFFER)
    .byte   $1              ; storage type (standard)
    .word   $0              ; creation date
    .word   $0              ; creation time

rw_params:
    .byte   $4
    .byte   $0              ; reference number*
    .word   $0              ; address of data buffer*
    .word   $0              ; number of bytes to read/write*
    .word   $0              ; number of bytes read/written

close_params:
    .byte   $1              ; 1 parameter
    .byte   $0              ; reference number*

quit_params:
    .byte   4               ; 4 parameters
    .byte   0               ; 0 is the only quit type
    .word   0               ; Reserved pointer for future use (what future?)
    .byte   0               ; Reserved byte for future use (what future?)
    .word   0               ; Reserved pointer for future use (what future?)

;-----------------------------------------------------------------------------
; Assets

READBUFFER          :=  $2000    ; Share read buffer with graphics memory

EXECSTART           :=  $6000
EXECLENGTH          =   $B800 - EXECSTART               ; FIXME: should end at $8000 to protect ISO tiles, but display image is bigger

ISOSTART            :=  $8000                           ; MAIN/AUX
ISOLENGTH           =   $4000                           ; 16k
ISOEND              :=  READBUFFER + ISOLENGTH - 1
ISOI1END            :=  ISOSTART + ISOLENGTH/2 - 1

IMAGESTART          :=  $6000                           ; Aux
IMAGELENGTH         =   3*1280                          ; 6000..6EFF : 20 bytes * 64 lines 1280(B) per image -> 3 images
IMAGEEND            :=  READBUFFER + IMAGELENGTH - 1
IMAGEENDAUX         :=  IMAGESTART + IMAGELENGTH - 1

FONT0START          :=  $B000                           ; AUX
FONT0LENGTH         =   8*128
FONT0END            :=  READBUFFER + FONT0LENGTH - 1
FONT0ENDAUX         :=  FONT0START + FONT0LENGTH - 1

FONT1START          :=  $B400                           ; AUX
FONT1LENGTH         =   8*128
FONT1END            :=  READBUFFER + FONT1LENGTH - 1
FONT1ENDAUX         :=  FONT1START + FONT1LENGTH - 1

TITLESTART          :=  $4000                           ; MAIN & AUX
TITLELENGTH         =   $2000
TITLEEND            :=  TITLESTART + TITLELENGTH - 1

;------------------------------------------------
; Constants
;------------------------------------------------

INSTALL_MAIN    = 0     ; Main memory
INSTALL_AUX     = 1     ; Aux memory
INSTALL_BOTH    = 2     ; Both main and aux
INSTALL_AUX_I1  = 3     ; Aux memory, interleave of 1

; Asset type
fileTypeFont:   String "Font Tilesheet"
fileTypeISO:    String "Isometric Tilesheet"
fileTypeImage:  String "Image Sheet"
fileTypeExe:    String "Executable"
fileTypeTitle:  String "Title Image"

; File names
fileNameFont0:      StringLen "DATA/FONT7X8.0"
fileNameFont1:      StringLen "DATA/FONT7X8.1"
fileNameISO:        StringLen "DATA/TILESHEET.0"
fileNameISOEnd:
fileNameImage:      StringLen "DATA/IMAGESHEET.0"
fileNameImageEnd:
fileNameTool:       StringLen "DATA/TOOL.0"
fileNameToolEnd:
fileNameTitle0:     StringLen "DATA/TITLE.0"
fileNameTitle1:     StringLen "DATA/TITLE.1"
fileNameGame:       StringLen "DATA/GAME"

; Asset List
fileDescription:    ; type, name, address, size, dest, interleave
    ;       TYPE            NAME              BUFFER          LENGTH          END(BUF)    STARTDEST       MODE            DESTEND (INT)   OFFSET
    ;       0               2                 4               6               8           10              12              14
    ;       --------------- ---------------   -----------     -----------     ----------- -----------     --------------- --------------- -------
    .word   fileTypeFont,   fileNameFont0,    READBUFFER,     FONT0LENGTH,    FONT0END,   FONT0START,     INSTALL_AUX,    FONT0ENDAUX     ; 0
    .word   fileTypeFont,   fileNameFont1,    READBUFFER,     FONT1LENGTH,    FONT1END,   FONT1START,     INSTALL_AUX,    FONT1ENDAUX     ; 16
    .word   fileTypeISO,    fileNameISO,      READBUFFER,     ISOLENGTH,      ISOEND,     ISOSTART,       INSTALL_AUX_I1, ISOI1END        ; 32
    .word   fileTypeImage,  fileNameImage,    READBUFFER,     IMAGELENGTH,    IMAGEEND,   IMAGESTART,     INSTALL_AUX,    IMAGEENDAUX     ; 48
    .word   fileTypeExe,    fileNameGame,     EXECSTART,      EXECLENGTH,     0,          EXECSTART,      INSTALL_MAIN,   0               ; 64
    .word   fileTypeExe,    fileNameTool,     EXECSTART,      EXECLENGTH,     0,          EXECSTART,      INSTALL_MAIN,   0               ; 80
    .word   fileTypeTitle,  fileNameTitle0,   TITLESTART,     TITLELENGTH,    TITLEEND,   TITLESTART,     INSTALL_AUX,    TITLEEND        ; 96
    .word   fileTypeTitle,  fileNameTitle1,   TITLESTART,     TITLELENGTH,    TITLEEND,   TITLESTART,     INSTALL_MAIN,   0               ; 112

assetFont0    =   16*0
assetFont1    =   16*1
assetISO      =   16*2
assetImage    =   16*3
assetGame     =   16*4
assetTool     =   16*5
assetTitle0   =   16*6
assetTitle1   =   16*7

;-----------------------------------------------------------------------------
; Utilies

.include "inline_print.asm"
