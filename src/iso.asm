;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------
; Isometric drawing routines


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


mapOffset:  .byte   4+5*8

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
;       combine order: odd row (0,0) lower-left (0,12), even-row (0,+1) lower-right(4,8), odd row (0,2) upper-left(0,4), even row(0,+3) upper-right(4,0)
;       then           odd row (0,0) lower-right(4,12), even-row (1,+1) lower-left (0,8), odd row (0,2) upper-left(4,4), even row(1,+3) upper-left(0,0)
;
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


.proc isoDrawMap

    ; 1:  8  .. 15
    ; 3:  24 .. 31
    ; 5:  40 .. 47
    ;
    ; 11: 88 .. 95

    lda     #1*8
    sta     mapOffset
:
    jsr     isoDrawMapTile
    inc     mapOffset
    lda     mapOffset
    and     #$7         ; are the lower 3 bits zero?
    bne     :-

    clc
    lda     mapOffset
    adc     #8
    sta     mapOffset
    cmp     #15*8
    bne     :-



    rts

.endproc

.proc isoDrawMapTile

    ; set coordinates
    lda     mapOffset
    and     #$7         ; x lower 3 bits (range 0-7)
    sec
    rol
    asl
    sta     tileX       ; screen x = 14 pixels * (map_x*4 + 2)
    lda     mapOffset
    and     #$78        ; y next 4 bits (range 0-15)
    lsr
    lsr
    lsr
    lsr
    clc
    adc     #8
    sta     tileY

    jsr     isoDrawTile_oddRow
    rts

.endproc

.proc isoDrawTile_oddRow

    jsr     isoScreenPointer

    ;=====================
    ; draw upper-left 4x4
    ;=====================
    ; even row (0,-1) lower-right(4,12)
    lda     mapOffset
    clc
    adc     #MAP_NW
    tax
    lda     isoMap,x
    clc
    adc     currentSheet_56x16+1
    sta     ptrAA1
    sta     ptrAB1 
    lda     #2+12*8
    sta     ptrAA0
    ora     #$80
    sta     ptrAB0      

    ; odd row (0,0) lower-left (0,8)
    ldx     mapOffset
    lda     isoMap,x
    clc
    adc     currentSheet_56x16+1
    sta     ptrBA1
    sta     ptrBB1 
    lda     #0+8*8
    sta     ptrBA0
    ora     #$80
    sta     ptrBB0      

    ; even-row (0,+1) upper-right(4,4)
    lda     mapOffset
    clc
    adc     #MAP_SW
    tax
    lda     isoMap,x
    clc
    adc     currentSheet_56x16+1
    sta     ptrCA1
    sta     ptrCB1 
    lda     #2+4*8
    sta     ptrCA0
    ora     #$80
    sta     ptrCB0     

    ; odd row (0,2) upper-left(0,0)
    lda     mapOffset
    clc
    adc     #MAP_S
    tax
    lda     isoMap,x
    clc
    adc     currentSheet_56x16+1
    sta     ptrDA1
    sta     ptrDB1 
    lda     #0+0*8
    sta     ptrDA0
    ora     #$80
    sta     ptrDB0  

    jsr     isoDrawTile4x4

    ;=====================
    ; draw lower-left 4x4
    ;=====================

    ; screen pointer is ready for lower

    ; odd row (0,0) lower-left (0,12)
    ; Copy from B -> A
    lda     ptrBA1
    sta     ptrAA1
    lda     ptrBB1
    sta     ptrAB1
    lda     #0+12*8
    sta     ptrAA0
    ora     #$80
    sta     ptrAB0      

    ; even-row (0,+1) lower-right(4,8)
    ; Copy from C -> B
    lda     ptrCA1
    sta     ptrBA1
    lda     ptrCB1
    sta     ptrBB1
    lda     #2+8*8
    sta     ptrBA0
    ora     #$80
    sta     ptrBB0      

    ; odd row (0,2) upper-left(0,4)
    ; Copy from D -> C
    lda     ptrDA1
    sta     ptrCA1
    lda     ptrDB1
    sta     ptrCB1
    lda     #0+4*8
    sta     ptrCA0
    ora     #$80
    sta     ptrCB0   

    ; even row(0,+3) upper-right(4,0)

    lda     mapOffset
    clc
    adc     #MAP_S2W
    tax
    lda     isoMap,x
    clc
    adc     currentSheet_56x16+1
    sta     ptrDA1
    sta     ptrDB1 
    lda     #2+0*8
    sta     ptrDA0
    ora     #$80
    sta     ptrDB0     

    jsr     isoDrawTile4x4

    ;=====================
    ; draw upper-right 4x4
    ;=====================

    ; reset screen pointers
    inc     tileX
    inc     tileX
    jsr     isoScreenPointer   

    ; odd row (0,0) lower-right(4,8)
    ; Copy from A -> B
    lda     ptrAA1
    sta     ptrBA1
    lda     ptrAB1
    sta     ptrBB1
    lda     #2+8*8
    sta     ptrBA0
    ora     #$80
    sta     ptrBB0      

    ; odd row (0,2) upper-left(4,0)
    ; Copy from C -> D
    lda     ptrCA1
    sta     ptrDA1
    lda     ptrCB1
    sta     ptrDB1
    lda     #2+0*8
    sta     ptrDA0
    ora     #$80
    sta     ptrDB0  

    ; even row (1,-1) lower-left (0,12)
    lda     mapOffset
    clc
    adc     #MAP_NE
    tax
    lda     isoMap,x
    clc
    adc     currentSheet_56x16+1
    sta     ptrAA1
    sta     ptrAB1 
    lda     #0+12*8
    sta     ptrAA0
    ora     #$80
    sta     ptrAB0

    ; even-row (1,+1) upper-left (0,4)
    lda     mapOffset
    clc
    adc     #MAP_SE
    tax
    lda     isoMap,x
    clc
    adc     currentSheet_56x16+1
    sta     ptrCA1
    sta     ptrCB1 
    lda     #0+4*8
    sta     ptrCA0
    ora     #$80
    sta     ptrCB0     

    jsr     isoDrawTile4x4

    ;=====================
    ; draw lower-right 4x4
    ;=====================

    ; screen pointer is ready

    ; odd row (0,0) lower-right(4,12)
    ; Copy from B -> A
    lda     ptrBA1
    sta     ptrAA1
    lda     ptrBB1
    sta     ptrAB1
    lda     #2+12*8
    sta     ptrAA0
    ora     #$80
    sta     ptrAB0      

    ; even-row (1,+1) lower-left (0,8)
    ; Copy from C -> B
    lda     ptrCA1
    sta     ptrBA1
    lda     ptrCB1
    sta     ptrBB1
    lda     #0+8*8
    sta     ptrBA0
    ora     #$80
    sta     ptrBB0      

    ; odd row (0,2) upper-left(4,4)
    ; Copy from D -> C
    lda     ptrDA1
    sta     ptrCA1
    lda     ptrDB1
    sta     ptrCB1
    lda     #2+4*8
    sta     ptrCA0
    ora     #$80
    sta     ptrCB0   

    ; even row(1,+3) upper-left(0,0)
    lda     mapOffset
    clc
    adc     #MAP_S2E
    tax
    lda     isoMap,x
    clc
    adc     currentSheet_56x16+1
    sta     ptrDA1
    sta     ptrDB1 
    lda     #0+0*8
    sta     ptrDA0
    ora     #$80
    sta     ptrDB0     

    jsr     isoDrawTile4x4

    rts

.endproc

.proc isoScreenPointer
    ; calculate screen pointer
    ldx     tileY
    lda     tileX           ; interpreting x as x*2
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    lda     linePage,x
    sta     screenPtr1
    rts
.endproc

.proc isoDrawTile4x4
    ; Need 8 pointers (4 pixels + 4 masks)

    lda     #0
    sta     offset

loop:
    ldy     offset

    ; +0 in aux
    lda     (ptrAA0),y
    and     (ptrBB0),y          ; mask 
    ora     (ptrBA0),y          ; data
    and     (ptrCB0),y          ; mask
    ora     (ptrCA0),y          ; data
    and     (ptrDB0),y          ; mask
    ora     (ptrDA0),y          ; data
    sta     byte0

    iny                         ; y = +1

    lda     (ptrAA0),y
    and     (ptrBB0),y          ; mask 
    ora     (ptrBA0),y          ; data
    and     (ptrCB0),y          ; mask
    ora     (ptrCA0),y          ; data
    and     (ptrDB0),y          ; mask
    ora     (ptrDA0),y          ; data
    sta     byte1

    iny                         
    iny     
    iny                         ; y = +4

    lda     (ptrAA0),y
    and     (ptrBB0),y          ; mask 
    ora     (ptrBA0),y          ; data
    and     (ptrCB0),y          ; mask
    ora     (ptrCA0),y          ; data
    and     (ptrDB0),y          ; mask
    ora     (ptrDA0),y          ; data
    sta     byte2

    iny                         ; y = +5

    lda     (ptrAA0),y
    and     (ptrBB0),y          ; mask 
    ora     (ptrBA0),y          ; data
    and     (ptrCB0),y          ; mask
    ora     (ptrCA0),y          ; data
    and     (ptrDB0),y          ; mask
    ora     (ptrDA0),y          ; data
    sta     byte3

    iny                         
    iny     
    iny                         ; y = +8
    sty     offset

    sta     RAMWRTON            ; aux mem write
    ldy     #0
    lda     byte0
    sta     (screenPtr0),y      ; aux+0
    ldy     #1
    lda     byte1
    sta     (screenPtr0),y      ; aux+1

    sta     RAMWRTOFF           ; main mem write
    ldy     #0
    lda     byte2
    sta     (screenPtr0),y      ; main+0
    ldy     #1
    lda     byte3
    sta     (screenPtr0),y      ; main+1

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    lda     offset
    cmp     #8*4
    beq     :+
    jmp     loop
:
    rts

offset:     .byte   0
byte0:      .byte   0
byte1:      .byte   0
byte2:      .byte   0
byte3:      .byte   0

.endproc

;-----------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------

.align 256
isoMap:

    .byte    0,  0,  0,  0,  0,  0,  0,  0      ; row  0
    .byte      0,  0,  0, 01, 01,  0,  0,  0    ;      1
    .byte    0,  0,  0, 01, 01, 00,  0,  0      ;      2
    .byte      0,  0, 01, 01, 00,  1,  0,  0    ;      3
    .byte    0,  0, 01, 01, 00, 12,  0,  0      ;      4
    .byte      0, 01, 01, 00, 12, 09,  0,  0    ;      5
    .byte    0,  0, 01, 11, 12, 11,  0, 0       ;      6
    .byte      0, 07, 00,  1, 01, 01,  0,  0    ;      7
    .byte    0,  0, 08,  1, 01, 01, 01,  0      ;      8
    .byte      0,  0, 00, 01, 01, 01,  0,  0    ;      9
    .byte    0,  0,  0, 07, 01, 01, 09,  0      ;     10
    .byte      0,  0,  0, 07, 01, 09,  0,  0    ;     11
    .byte    0,  0,  0,  0, 07, 09,  0,  0      ;     12
    .byte      0,  0,  0,  0, 08,  0,  0,  0    ;     13
    .byte    0,  0,  0,  0,  0,  0,  0,  0      ;     14
    .byte      0,  0,  0,  0,  0,  0,  0,  0    ;     15


    .byte  01, 01, 01, 01, 01, 01, 01, 01     ; row  0  
    .byte    01, 01, 01, 01, 01, 01, 01, 01   ;      1 <--      
    .byte  01, 01, 01, 01, 01, 01, 01, 01     ;      2  
    .byte    01, 01, 01, 01, 01, 01, 01, 01   ;      3      
    .byte  01, 01, 01, 01, 01, 01, 01, 01     ;      4  
    .byte    01, 01, 01, 01, 01, 01, 01, 01   ;      5      
    .byte  01, 01, 01, 01, 01, 01, 01, 01     ;      6  
    .byte    01, 01, 01, 01, 01, 01, 01, 01   ;      7      
    .byte  01, 01, 01, 01, 01, 01, 01, 01     ;      8  
    .byte    01, 01, 01, 01, 01, 01, 01, 01   ;      9      
    .byte  01, 01, 01, 01, 01, 01, 01, 01     ;     10  
    .byte    01, 01, 01, 01, 01, 01, 01, 01   ;     11 <--      
    .byte  01, 01, 01, 01, 01, 01, 01, 01     ;     12
    .byte    01, 01, 01, 01, 01, 01, 01, 01   ;     13
    .byte  01, 01, 01, 01, 01, 01, 01, 01     ;     14
    .byte    01, 01, 01, 01, 01, 01, 01, 01   ;     15




    .byte  06, 06, 06, 06, 06, 06, 06, 06
    .byte    06, 06, 06, 06, 06, 06, 06, 06
    .byte  06, 06, 06, 06, 06, 06, 06, 06
    .byte    06, 06, 06, 06, 06, 06, 06, 06
    .byte  06, 06, 06, 06, 06, 06, 06, 06
    .byte    06, 06, 06, 06, 06, 06, 06, 06
    .byte  06, 06, 06, 06, 06, 06, 06, 06
    .byte    06, 06, 06, 06, 06, 06, 06, 06
    .byte  06, 06, 06, 06, 06, 06, 06, 06
    .byte    06, 06, 06, 06, 06, 06, 06, 06
    .byte  06, 06, 06, 06, 06, 06, 06, 06
    .byte    06, 06, 06, 06, 06, 06, 06, 06


