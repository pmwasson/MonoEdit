;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------


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
; clearScreen
;
;-----------------------------------------------------------------------------
.proc clearScreen
    lda     #$00
    sta     screenPtr0
    lda     #$20
    sta     screenPtr1

    sta     CLR80COL        ; Use RAMWRT for aux mem

loop:
    ldy     #0

    lda     clearColor+0
    ; aux mem
    sta     RAMWRTON
:
    sta     (screenPtr0),y
    iny
    bne     :-

    sta     RAMWRTOFF

    lda     clearColor+1
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
    ldx     clearColor+1
    lda     clearColor+0
    sta     clearColor+1
    stx     clearColor+0
:

    lda     #$40
    cmp     screenPtr1
    bne     loop
    rts

.endproc

clearColor:     .byte   0,0

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
