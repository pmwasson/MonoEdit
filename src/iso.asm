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


MAP_WIDTH   = 8
MAP_HEIGHT  = 12

; Deltas
MAP_NW      = 256 - MAP_WIDTH   ; ( 0,-1)
MAP_NE      = MAP_NW+1          ; ( 0,+1)
MAP_SW      = MAP_WIDTH         ; (-1,+1)
MAP_SE      = MAP_SW+1          ; (+1,+1)
MAP_S2W     = MAP_WIDTH*3       ; (-1,+3)
MAP_S2E     = MAP_S2W+1         ; (+1,+3)
MAP_S       = MAP_WIDTH*2       ; ( 0,+2)


;------------------------------------------------
; Draw isometric quarter
;------------------------------------------------
;  Using a 8 byte wide (56 pixels) by 16 byte high tile
;  Broken into an upper and lower half (8x8)
;  Where the grid is spaced based on the 8x8 tiles
;  But odd rows are offset 4x4
;  So the map index looks like:
;  Row  Array
;  00:  00  01  02  03  04  05  06  07
;  01:    08  09  10  11  12  13  14  15
;  02:  16  17 ...
;  ...
;
;  Only need to draw the odd rows as all the other tiles will overlap.
;  These means once the screen is draw, only the things that change
;  need to be updated without doing a full z-buffer redraw.
;
;  Draw 
;    1) odd row tile upper half left (4x4) then right (4x4) across screen
;       combine order: even row (0,-1) lower-right(4,12), odd row (0,0) lower-left (0,8), even-row (0,+1) upper-right(4,4), odd row (0,2) upper-left(0,0)
;       then           even row (1,-1) lower-left (0,12), odd row (0,0) lower-right(4,8), even-row (1,+1) upper-left (0,4), odd row (0,2) upper-left(4,0)
;    2) odd row tile lower half left (4x4) then right (4x4) across screen
;
;  The left->right can share the odd row and next odd row tiles
;  the right->left can share the even row (-1/+1) next even row (+1/) 
;    
; tile data stored mask,data interleaved in 4x4 tiles in the order:
;    0 1
;    2 3
;    4 5
;    6 7
;
;  single tile is 4x4x2x8 = 256 bytes, so easy to calc base.
;  However quite large, so may want to split in half so that some tiles (like the ground) can just use an empty upper half







draw4x4:

    ldx     #1
    ldy     #0

    clc                 ; assuming carry doesn't get set inside of loop

loop:
    ;                   ; main mem write
    lda     (ptrA),y
    and     (ptrB),x    ; mask 
    ora     (ptrB),y    ; data
    and     (ptrC),x    ; mask
    ora     (ptrC,y     ; data
    and     (ptrD),x    ; mask
    ora     (ptrD),y    ; data
    sta     (scrPtr0),y

    sta     RAMWRTON    ; aux mem write
    lda     (ptrA),y
    and     (ptrB),x    ; mask 
    ora     (ptrB),y    ; data
    and     (ptrC),x    ; mask
    ora     (ptrC),y    ; data
    and     (ptrD),x    ; mask
    ora     (ptrD),y    ; data
    sta     (scrPtr0),y

    inx
    inx
    iny
    iny
    dec     scrPtr0

    lda     (ptrA),y
    and     (ptrB),x    ; mask 
    ora     (ptrB),y    ; data
    and     (ptrC),x    ; mask
    ora     (ptrC),y    ; data
    and     (ptrD),x    ; mask
    ora     (ptrD),y    ; data
    sta     (scrPtr0),y

    sta     RAMWRTOFF   ; main mem write
    lda     (ptrA),y
    and     (ptrB),x    ; mask 
    ora     (ptrB),y    ; data
    and     (ptrC),x    ; mask
    ora     (ptrC),y    ; data
    and     (ptrD),x    ; mask
    ora     (ptrD),y    ; data
    sta     (scrPtr0),y

    inx
    inx
    iny
    iny
    dec     scrPtr0

    lda     scrPtr1
    adc     #4
    sta     scrPtr1
    lda     ptr0

    cpy     2*4     ; 4 rows
    bne     loop

    rts


;-----------------------------------------------------------------------------
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

;-----------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------

.align 256
mapData:
    .byte   00, 00, 00, 00, 00, 00, 00, 00      ; row  0
    .byte     00, 00, 00, 00, 00, 00, 00, 00    ;      1
    .byte   00, 00, 00, 00, 00, 00, 00, 00      ;      2
    .byte     00, 00, 00, 00, 00, 00, 00, 00    ;      3
    .byte   00, 00, 00, 00, 00, 00, 00, 00      ;      4
    .byte     00, 00, 00, 00, 00, 00, 00, 00    ;      5
    .byte   00, 00, 00, 00, 00, 00, 00, 00      ;      6
    .byte     00, 00, 00, 00, 00, 00, 00, 00    ;      7
    .byte   00, 00, 00, 00, 00, 00, 00, 00      ;      8
    .byte     00, 00, 00, 00, 00, 00, 00, 00    ;      9
    .byte   00, 00, 00, 00, 00, 00, 00, 00      ;     10
    .byte     00, 00, 00, 00, 00, 00, 00, 00    ;     11


.align 256
tileSheet:

    ; 56x16 isometric tile


    ; mask
    .byte $00,$00,$0F,$00,$00,$78,$00,$00,$00,$7C,$7F,$00,$00,$7F,$1F,$00           
    .byte $00,$7F,$7F,$3F,$7E,$7F,$7F,$00,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$00,$7F,$7F,$3F,$7E,$7F,$7F,$00           
    .byte $00,$7C,$7F,$00,$00,$7F,$1F,$00,$00,$00,$0F,$00,$00,$78,$00,$00           


    ; Textured Cube                                                                 
    .byte $00,$00,$0F,$00,$00,$78,$00,$00,$00,$7C,$77,$00,$00,$77,$1F,$00           
    .byte $00,$7B,$7F,$3F,$7E,$7F,$6F,$00,$7F,$7F,$7F,$5F,$7D,$7F,$7F,$7F           
    .byte $7F,$7F,$7F,$5F,$7D,$7F,$7F,$7F,$55,$7B,$7F,$3F,$7E,$7F,$6F,$40           
    .byte $2B,$7E,$77,$00,$55,$77,$1F,$40,$55,$55,$0F,$00,$2A,$7A,$00,$40           
    .byte $2B,$2A,$00,$00,$55,$55,$00,$40,$55,$55,$00,$00,$2A,$6A,$00,$40           
    .byte $2B,$2A,$00,$00,$55,$55,$00,$40,$55,$55,$00,$00,$2A,$6A,$00,$40           
    .byte $7F,$2A,$00,$40,$55,$55,$00,$7F,$00,$57,$00,$3F,$7E,$6A,$60,$00           
    .byte $00,$7C,$70,$00,$00,$57,$1F,$00,$00,$00,$0F,$00,$00,$78,$00,$00           

