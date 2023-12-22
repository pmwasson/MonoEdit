;-----------------------------------------------------------------------------
; Paul Wasson - 2023
;-----------------------------------------------------------------------------
; Isometric tile set
;-----------------------------------------------------------------------------

; Void
tileSheet_iso_start:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Gray Background
.byte $55,$55,$2A,$2A,$2A,$2A,$55,$55,$55,$55,$2A,$2A,$2A,$2A,$55,$55
.byte $55,$55,$2A,$2A,$2A,$2A,$55,$55,$55,$55,$2A,$2A,$2A,$2A,$55,$55
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; White Background
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Black Background
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Cube (middle)
.byte $00,$00,$00,$60,$00,$00,$00,$1E,$00,$70,$00,$01,$00,$0F,$00,$00
.byte $00,$00,$78,$00,$40,$00,$07,$00,$3C,$00,$00,$00,$03,$00,$00,$00
.byte $7F,$7F,$7F,$1F,$7F,$7F,$7F,$01,$7F,$0F,$7F,$00,$7F,$00,$7F,$00
.byte $7F,$00,$07,$00,$3F,$00,$00,$00,$03,$00,$00,$00,$00,$00,$00,$00
.byte $03,$00,$00,$00,$3C,$00,$00,$00,$40,$00,$07,$00,$00,$00,$78,$00
.byte $00,$0F,$00,$00,$00,$70,$00,$01,$00,$00,$00,$1E,$00,$00,$00,$60
.byte $7C,$7F,$7F,$7F,$40,$7F,$7F,$7F,$00,$7F,$78,$7F,$00,$7F,$00,$7F
.byte $00,$70,$00,$7F,$00,$00,$00,$7E,$00,$00,$00,$60,$00,$00,$00,$00
.byte $03,$00,$00,$00,$3D,$00,$00,$00,$41,$00,$07,$00,$01,$00,$78,$00
.byte $01,$0F,$00,$00,$01,$70,$00,$01,$01,$00,$00,$1E,$01,$00,$00,$60
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$60,$00,$00,$00,$5E,$00,$70,$00,$41,$00,$0F,$00,$40
.byte $00,$00,$78,$40,$40,$00,$07,$40,$3C,$00,$00,$40,$03,$00,$00,$40
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; ; Cube (bottom) [BG]
; .byte $01,$00,$00,$40,$01,$00,$00,$40,$01,$00,$00,$40,$01,$00,$00,$40
; .byte $01,$00,$00,$40,$01,$00,$00,$40,$01,$00,$00,$40,$01,$00,$00,$40
; .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; .byte $00,$00,$00,$40,$00,$00,$00,$40,$00,$00,$00,$40,$00,$00,$00,$40
; .byte $00,$00,$00,$40,$00,$00,$00,$40,$00,$00,$00,$40,$00,$00,$00,$40
; .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; .byte $03,$00,$00,$40,$3C,$00,$00,$40,$40,$00,$07,$40,$00,$00,$78,$40
; .byte $00,$0F,$00,$40,$00,$70,$00,$41,$00,$00,$00,$5E,$00,$00,$00,$60
; .byte $00,$00,$00,$00,$03,$00,$00,$00,$3F,$00,$00,$00,$7F,$00,$07,$00
; .byte $7F,$00,$7F,$00,$7F,$0F,$7F,$00,$7F,$7F,$7F,$01,$7F,$7F,$7F,$1F
; .byte $00,$00,$00,$60,$00,$00,$00,$1E,$00,$70,$00,$01,$00,$0F,$00,$00
; .byte $00,$00,$78,$00,$40,$00,$07,$00,$3C,$00,$00,$00,$03,$00,$00,$00
; .byte $00,$00,$00,$00,$00,$00,$00,$60,$00,$00,$00,$7E,$00,$70,$00,$7F
; .byte $00,$7F,$00,$7F,$00,$7F,$78,$7F,$40,$7F,$7F,$7F,$7C,$7F,$7F,$7F

; Cube (bottom) [Gray BG]
.byte $01,$00,$00,$40,$01,$00,$00,$40,$01,$00,$00,$40,$01,$00,$00,$40
.byte $01,$00,$00,$40,$01,$00,$00,$40,$01,$00,$00,$40,$01,$00,$00,$40
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$40,$00,$00,$00,$40,$00,$00,$00,$40,$00,$00,$00,$40
.byte $00,$00,$00,$40,$00,$00,$00,$40,$00,$00,$00,$40,$00,$00,$00,$40
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $03,$00,$00,$40,$3E,$00,$00,$40,$55,$00,$07,$40,$2A,$00,$7D,$40
.byte $55,$0F,$2A,$40,$2A,$7A,$55,$41,$55,$55,$2A,$5E,$2A,$2A,$55,$75
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$60,$00,$00,$00,$5E,$00,$70,$00,$2B,$00,$2F,$00,$55
.byte $00,$55,$78,$2A,$40,$2A,$57,$55,$7C,$55,$2A,$2A,$2B,$2A,$55,$55
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Grass (middle)
.byte $00,$00,$00,$60,$00,$00,$00,$7E,$00,$70,$00,$7F,$00,$7F,$00,$7F
.byte $00,$7F,$78,$7F,$40,$7F,$7F,$5F,$7C,$7F,$7F,$3F,$7F,$7F,$7B,$7F
.byte $7F,$7F,$7F,$1F,$7F,$7F,$7F,$01,$7F,$0F,$7F,$00,$7F,$00,$7F,$00
.byte $7F,$00,$07,$00,$3F,$00,$00,$00,$03,$00,$00,$00,$00,$00,$00,$00
.byte $03,$00,$00,$00,$3F,$00,$00,$00,$7E,$00,$07,$00,$7F,$00,$7F,$00
.byte $7F,$0F,$7F,$00,$7E,$7F,$7F,$01,$7F,$7E,$75,$1F,$7F,$7F,$7B,$7F
.byte $7C,$7F,$7F,$7F,$40,$7F,$7F,$7F,$00,$7F,$78,$7F,$00,$7F,$00,$7F
.byte $00,$70,$00,$7F,$00,$00,$00,$7E,$00,$00,$00,$60,$00,$00,$00,$00
.byte $7F,$57,$7F,$7F,$7D,$6F,$7F,$7F,$49,$7F,$7F,$7F,$48,$7F,$7D,$7F
.byte $4C,$7F,$75,$7F,$44,$71,$64,$7F,$00,$00,$60,$7E,$10,$08,$00,$60
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $7F,$77,$7F,$7F,$7F,$7F,$7F,$1F,$7F,$7F,$6F,$0F,$7F,$7F,$7F,$46
.byte $7E,$1D,$7F,$00,$7F,$11,$17,$08,$7F,$01,$04,$00,$43,$08,$04,$04
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; ; Grass (bottom) [BG]
; .byte $00,$00,$00,$00,$01,$02,$00,$40,$00,$00,$00,$00,$00,$00,$01,$02
; .byte $00,$00,$00,$00,$00,$00,$00,$40,$02,$01,$00,$00,$00,$00,$01,$04
; .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; .byte $00,$08,$00,$00,$00,$00,$00,$00,$00,$00,$40,$08,$10,$00,$00,$00
; .byte $00,$00,$00,$00,$00,$40,$00,$00,$00,$00,$08,$08,$00,$00,$00,$00
; .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; .byte $03,$00,$00,$00,$3C,$02,$00,$00,$40,$00,$07,$00,$00,$00,$78,$04
; .byte $00,$0F,$00,$40,$00,$70,$00,$01,$00,$00,$00,$1E,$00,$00,$00,$60
; .byte $00,$00,$00,$00,$03,$00,$00,$00,$3F,$00,$00,$00,$7F,$00,$07,$00
; .byte $7F,$00,$7F,$00,$7F,$0F,$7F,$00,$7F,$7F,$7F,$01,$7F,$7F,$7F,$1F
; .byte $00,$02,$00,$60,$20,$00,$00,$1E,$00,$70,$00,$01,$08,$0F,$00,$00
; .byte $00,$00,$78,$00,$40,$00,$07,$00,$3C,$00,$00,$00,$03,$00,$00,$00
; .byte $00,$00,$00,$00,$00,$00,$00,$60,$00,$00,$00,$7E,$00,$70,$00,$7F
; .byte $00,$7F,$00,$7F,$00,$7F,$78,$7F,$40,$7F,$7F,$7F,$7C,$7F,$7F,$7F

; Grass (bottom) [Gray BG]
.byte $00,$00,$00,$00,$01,$02,$00,$40,$00,$00,$00,$00,$00,$00,$01,$02
.byte $00,$00,$00,$00,$00,$00,$00,$40,$02,$01,$00,$00,$00,$00,$01,$04
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$08,$00,$00,$00,$00,$00,$00,$00,$00,$40,$08,$10,$00,$00,$00
.byte $00,$00,$00,$00,$00,$40,$00,$00,$00,$00,$08,$08,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $03,$00,$40,$00,$3E,$00,$00,$00,$55,$08,$07,$00,$2A,$00,$7D,$10
.byte $55,$0F,$2A,$00,$2A,$7A,$55,$01,$55,$55,$2A,$1E,$2A,$2A,$55,$75
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$01,$60,$00,$02,$00,$5E,$20,$70,$00,$2B,$00,$2F,$00,$55
.byte $00,$55,$78,$2A,$40,$2A,$57,$55,$7C,$55,$2A,$2A,$2B,$2A,$55,$55
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Water (middle)
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F
.byte $7F,$7F,$7F,$1F,$7F,$7F,$7F,$01,$7F,$0F,$7F,$00,$7F,$00,$7F,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F
.byte $7C,$7F,$7F,$7F,$40,$7F,$7F,$7F,$00,$7F,$78,$7F,$00,$7F,$00,$7F
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $7F,$00,$07,$00,$3F,$00,$00,$00,$03,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$70,$00,$7F,$00,$00,$00,$7E,$00,$00,$00,$60,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Water (bottom)
.byte $01,$00,$00,$00,$21,$00,$20,$00,$01,$00,$00,$00,$20,$08,$00,$02
.byte $00,$00,$20,$02,$21,$00,$20,$00,$06,$08,$20,$00,$7D,$00,$06,$02
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$01,$00,$08,$00,$10,$00,$00,$40,$01,$00,$00,$00,$01
.byte $01,$08,$04,$11,$00,$00,$44,$10,$00,$48,$00,$14,$01,$10,$00,$6F
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $57,$00,$0B,$02,$7E,$00,$7B,$00,$55,$17,$6F,$03,$2A,$6A,$7D,$07
.byte $55,$4F,$2A,$5F,$2A,$7A,$55,$35,$55,$55,$2A,$1E,$2A,$2A,$55,$75
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$62,$04,$69,$00,$5E,$20,$5F,$01,$77,$79,$2B,$49,$2F,$6F,$55
.byte $7F,$55,$7F,$2A,$5F,$2A,$57,$55,$7D,$55,$2A,$2A,$2B,$2A,$55,$55
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Cursor / Floor Tile
.byte $00,$00,$00,$60,$00,$00,$00,$1E,$00,$70,$00,$61,$00,$0F,$00,$7E
.byte $00,$70,$78,$61,$40,$7F,$07,$1F,$3C,$70,$78,$61,$43,$0F,$7F,$7E
.byte $7F,$7F,$7F,$1F,$7F,$7F,$7F,$01,$7F,$0F,$7F,$00,$7F,$00,$7F,$00
.byte $7F,$00,$07,$00,$3F,$00,$00,$00,$03,$00,$00,$00,$00,$00,$00,$00
.byte $03,$00,$00,$00,$3C,$00,$00,$00,$43,$00,$07,$00,$3F,$00,$78,$00
.byte $43,$0F,$07,$00,$7C,$70,$7F,$01,$43,$0F,$07,$1E,$3F,$7F,$78,$61
.byte $7C,$7F,$7F,$7F,$40,$7F,$7F,$7F,$00,$7F,$78,$7F,$00,$7F,$00,$7F
.byte $00,$70,$00,$7F,$00,$00,$00,$7E,$00,$00,$00,$60,$00,$00,$00,$00
.byte $43,$0F,$7F,$7E,$3C,$70,$78,$61,$40,$7F,$07,$1F,$00,$70,$78,$61
.byte $00,$0F,$00,$7E,$00,$70,$00,$61,$00,$00,$00,$1E,$00,$00,$00,$60
.byte $00,$00,$00,$00,$03,$00,$00,$00,$3F,$00,$00,$00,$7F,$00,$07,$00
.byte $7F,$00,$7F,$00,$7F,$0F,$7F,$00,$7F,$7F,$7F,$01,$7F,$7F,$7F,$1F
.byte $3F,$7F,$78,$61,$43,$0F,$07,$1E,$7C,$70,$7F,$01,$43,$0F,$07,$00
.byte $3F,$00,$78,$00,$43,$00,$07,$00,$3C,$00,$00,$00,$03,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$60,$00,$00,$00,$7E,$00,$70,$00,$7F
.byte $00,$7F,$00,$7F,$00,$7F,$78,$7F,$40,$7F,$7F,$7F,$7C,$7F,$7F,$7F

; Brick Wall (above)
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$60,$00,$00,$00,$7E           
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$1F,$7F,$7F,$7F,$01,$7F,$0F,$7F,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$03,$00,$00,$00,$3F,$00,$00,$00           
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
.byte $7F,$7F,$7F,$7F,$7C,$7F,$7F,$7F,$40,$7F,$7F,$7F,$00,$7F,$78,$7F           
.byte $00,$70,$00,$7F,$00,$70,$00,$7F,$00,$00,$78,$1E,$40,$0F,$7F,$00           
.byte $7C,$7F,$7F,$01,$7C,$7F,$7F,$1F,$43,$7F,$7F,$1F,$3F,$7F,$78,$01           
.byte $7F,$00,$7F,$00,$7F,$00,$07,$00,$3F,$00,$00,$00,$03,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $3F,$00,$00,$00,$03,$00,$78,$00,$40,$0F,$7F,$00,$7C,$7F,$7F,$01           
.byte $7C,$7F,$7F,$1F,$40,$7F,$7F,$07,$00,$3F,$78,$60,$3C,$03,$00,$7E           
.byte $00,$7F,$00,$7F,$00,$70,$00,$7F,$00,$00,$00,$7E,$00,$00,$00,$60           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           

; Brick Wall (floor)
.byte $3F,$0C,$00,$60,$3F,$00,$78,$7E,$3C,$0F,$78,$60,$00,$7F,$78,$01           
.byte $3C,$7F,$78,$1F,$7C,$7F,$07,$7F,$7C,$70,$1F,$7F,$7C,$0C,$1F,$7E           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $7F,$00,$07,$7E,$3F,$0F,$00,$7E,$03,$0F,$78,$1E,$00,$0F,$78,$00           
.byte $3C,$0F,$78,$1E,$3F,$70,$78,$1F,$3F,$7C,$00,$1F,$3F,$7C,$18,$1F           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $43,$7C,$1F,$61,$3F,$7C,$18,$1F,$3F,$7C,$00,$1F,$3F,$70,$78,$1F           
.byte $3C,$0F,$78,$1E,$00,$0F,$78,$00,$3C,$0F,$78,$1E,$7C,$0F,$07,$7E           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $43,$7C,$1F,$61,$7C,$0C,$1F,$7E,$7C,$00,$1F,$7E,$7C,$0F,$07,$7E           
.byte $3C,$0F,$78,$1E,$00,$0F,$78,$00,$3C,$0F,$78,$1E,$3F,$70,$78,$1F           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           

; Brick Wall (below)
.byte $7C,$00,$1F,$7E,$7C,$0C,$1F,$7E,$7C,$7C,$1F,$61,$43,$7C,$1F,$1F           
.byte $3F,$7C,$18,$1F,$3F,$7C,$00,$1F,$3F,$70,$78,$1F,$3F,$0F,$78,$1E           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $3F,$7C,$00,$1F,$3F,$7C,$18,$1F,$43,$7C,$1F,$1F,$7C,$7C,$1F,$61           
.byte $7C,$0C,$1F,$7E,$7C,$00,$1F,$7E,$7C,$0F,$07,$7E,$3C,$0F,$78,$7E           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $3C,$0F,$78,$00,$02,$0F,$78,$1E,$15,$0F,$78,$7E,$2A,$0F,$05,$7E           
.byte $55,$00,$2A,$7E,$2A,$2A,$55,$7E,$55,$55,$2A,$60,$2A,$2A,$55,$15           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$0F,$78,$1E,$3C,$0F,$78,$40,$3F,$0F,$78,$2A,$3F,$20,$78,$55           
.byte $3F,$55,$00,$2A,$3F,$2A,$55,$55,$43,$55,$2A,$2A,$28,$2A,$55,$55           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           

; Goofy (top)
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$7C,$00,$00,$00,$7E
.byte $20,$70,$00,$63,$28,$78,$04,$61,$30,$7E,$02,$77,$30,$7F,$01,$7F
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$03,$7F,$7F,$7F,$01,$0F,$0F,$7F,$00
.byte $07,$07,$7B,$00,$03,$01,$71,$00,$07,$00,$78,$00,$03,$00,$3C,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$1F,$00,$00,$00,$3F,$00,$00,$00
.byte $71,$00,$00,$00,$71,$00,$00,$00,$7B,$00,$00,$00,$7B,$00,$03,$00
.byte $7F,$7F,$7F,$7F,$60,$7F,$7F,$7F,$00,$7F,$7F,$7F,$00,$7F,$7F,$7F
.byte $00,$7F,$7E,$7F,$00,$7F,$7E,$7F,$00,$7F,$7C,$7F,$00,$7F,$70,$7F
.byte $7C,$7F,$41,$03,$60,$7F,$61,$03,$40,$77,$63,$07,$00,$73,$77,$3F
.byte $00,$71,$7E,$7F,$00,$70,$7C,$7F,$00,$70,$00,$6F,$00,$70,$00,$7F
.byte $01,$00,$1C,$00,$03,$00,$0C,$00,$1F,$00,$08,$00,$3F,$00,$00,$00
.byte $7F,$04,$00,$00,$7F,$06,$01,$00,$7F,$07,$03,$00,$7F,$07,$7F,$00
.byte $78,$00,$0F,$00,$78,$70,$1F,$0F,$78,$70,$3F,$00,$3E,$70,$3F,$0F
.byte $3F,$7F,$7E,$01,$3F,$7F,$7C,$0F,$3F,$1F,$78,$00,$3F,$00,$00,$00
.byte $00,$0F,$60,$70,$00,$07,$40,$60,$00,$07,$00,$70,$00,$00,$00,$60
.byte $00,$00,$00,$70,$00,$00,$01,$60,$00,$00,$03,$70,$00,$40,$07,$7F

; Goofy (middle)
.byte $00,$60,$00,$7F,$00,$70,$00,$07,$00,$70,$00,$03,$00,$78,$00,$03
.byte $00,$70,$00,$03,$00,$70,$00,$03,$00,$7E,$00,$03,$00,$2A,$00,$03
.byte $7F,$0F,$7F,$00,$7F,$07,$7F,$00,$7F,$07,$7F,$78,$7F,$03,$7F,$78
.byte $7F,$07,$7F,$78,$7F,$01,$7F,$78,$7F,$00,$7F,$78,$7F,$00,$7F,$78
.byte $1F,$00,$00,$00,$3F,$00,$00,$00,$3E,$00,$00,$00,$3E,$00,$00,$00
.byte $3E,$00,$00,$00,$3E,$00,$00,$00,$7E,$00,$07,$00,$7E,$00,$07,$00
.byte $40,$7F,$7F,$7F,$00,$7F,$7F,$7F,$00,$7F,$7F,$7F,$00,$7F,$7F,$7F
.byte $00,$7F,$7F,$7F,$00,$7F,$78,$7F,$00,$7F,$70,$7F,$00,$7F,$70,$7F
.byte $00,$2A,$00,$03,$00,$2A,$00,$03,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $7F,$00,$7F,$78,$7F,$00,$7F,$78,$7F,$01,$7F,$7C,$7F,$7F,$7F,$7F
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F
.byte $36,$00,$05,$00,$36,$00,$05,$00,$36,$00,$01,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$7F,$70,$7F,$00,$7F,$70,$7F,$00,$7F,$7C,$7F,$01,$7F,$7E,$7F
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F
tileSheet_iso_end:

.res    4096-(tileSheet_iso_end-tileSheet_iso_start)




