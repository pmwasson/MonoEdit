;-----------------------------------------------------------------------------
; Paul Wasson - 2023
;-----------------------------------------------------------------------------
; Isometric tile set
;-----------------------------------------------------------------------------
; Void
tileSheet_iso_start:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

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

.byte $03,$00,$00,$40,$3C,$00,$00,$40,$40,$00,$07,$40,$00,$00,$78,$40           
.byte $00,$0F,$00,$40,$00,$70,$00,$41,$00,$00,$00,$5E,$00,$00,$00,$60           
.byte $00,$00,$00,$00,$03,$00,$00,$00,$3F,$00,$00,$00,$7F,$00,$07,$00           
.byte $7F,$00,$7F,$00,$7F,$0F,$7F,$00,$7F,$7F,$7F,$01,$7F,$7F,$7F,$1F           
.byte $00,$00,$00,$60,$00,$00,$00,$1E,$00,$70,$00,$01,$00,$0F,$00,$00           
.byte $00,$00,$78,$00,$40,$00,$07,$00,$3C,$00,$00,$00,$03,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$60,$00,$00,$00,$7E,$00,$70,$00,$7F           
.byte $00,$7F,$00,$7F,$00,$7F,$78,$7F,$40,$7F,$7F,$7F,$7C,$7F,$7F,$7F           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           ; Not used
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           ;
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           ;
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           ;
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           ;
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           ;
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           ;
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           ;

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

; Grass (bottom)
.byte $03,$00,$40,$00,$3C,$00,$00,$00,$40,$08,$07,$00,$00,$00,$78,$10           
.byte $00,$0F,$00,$00,$00,$70,$00,$01,$00,$00,$00,$1E,$00,$00,$00,$60           
.byte $00,$00,$00,$00,$03,$00,$00,$00,$3F,$00,$00,$00,$7F,$00,$07,$00           
.byte $7F,$00,$7F,$00,$7F,$0F,$7F,$00,$7F,$7F,$7F,$01,$7F,$7F,$7F,$1F           
.byte $00,$00,$01,$60,$00,$02,$00,$1E,$20,$70,$00,$01,$00,$0F,$00,$00           
.byte $00,$00,$78,$00,$40,$00,$07,$00,$3C,$00,$00,$00,$03,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$60,$00,$00,$00,$7E,$00,$70,$00,$7F           
.byte $00,$7F,$00,$7F,$00,$7F,$78,$7F,$40,$7F,$7F,$7F,$7C,$7F,$7F,$7F           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           ; Not used
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           ;
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           ;
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           ;
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           ;
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           ;
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           ;
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           ;

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
.byte $57,$00,$0B,$02,$7C,$00,$7B,$00,$40,$17,$6F,$03,$00,$6A,$78,$07           
.byte $00,$4F,$00,$5F,$00,$70,$00,$35,$00,$00,$00,$1E,$00,$00,$00,$60           
.byte $00,$00,$00,$00,$03,$00,$00,$00,$3F,$00,$00,$00,$7F,$00,$07,$00           
.byte $7F,$00,$7F,$00,$7F,$0F,$7F,$00,$7F,$7F,$7F,$01,$7F,$7F,$7F,$1F           
.byte $00,$62,$04,$69,$00,$5E,$20,$1F,$01,$77,$79,$01,$49,$0F,$6F,$00           
.byte $7F,$00,$7F,$00,$5F,$00,$07,$00,$3D,$00,$00,$00,$03,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$60,$00,$00,$00,$7E,$00,$70,$00,$7F           
.byte $00,$7F,$00,$7F,$00,$7F,$78,$7F,$40,$7F,$7F,$7F,$7C,$7F,$7F,$7F           

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

; Brick Wall 1
.byte $00,$00,$00,$00,$00,$00,$00,$40,$00,$00,$00,$54,$00,$20,$00,$55           
.byte $00,$2A,$00,$55,$00,$20,$50,$55,$00,$0A,$55,$14,$28,$2A,$55,$41           
.byte $7F,$7F,$7F,$1F,$7F,$7F,$7F,$01,$7F,$0F,$7F,$00,$7F,$00,$7F,$00           
.byte $7F,$00,$07,$00,$3F,$00,$00,$00,$03,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$02,$00,$00,$00,$2A,$00,$00,$00,$2A,$00,$05,$00           
.byte $2A,$00,$50,$00,$02,$0A,$55,$00,$28,$2A,$55,$01,$2A,$2A,$55,$15           
.byte $7C,$7F,$7F,$7F,$40,$7F,$7F,$7F,$00,$7F,$78,$7F,$00,$7F,$00,$7F           
.byte $00,$70,$00,$7F,$00,$00,$00,$7E,$00,$00,$00,$60,$00,$00,$00,$00           
.byte $28,$2A,$55,$15,$02,$2A,$55,$55,$2A,$2A,$50,$55,$2A,$2A,$05,$55           
.byte $28,$20,$55,$55,$02,$0A,$55,$54,$2A,$0A,$50,$40,$2A,$0A,$05,$14           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $28,$2A,$55,$15,$02,$2A,$55,$41,$2A,$0A,$50,$54,$2A,$00,$05,$54           
.byte $2A,$0A,$05,$14,$2A,$0A,$50,$40,$02,$0A,$50,$54,$28,$20,$50,$55           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           

; Brick Wall 2
.byte $2A,$00,$15,$54,$28,$08,$15,$54,$02,$28,$15,$41,$2A,$28,$10,$15           
.byte $2A,$28,$00,$15,$2A,$20,$50,$15,$2A,$0A,$50,$14,$28,$0A,$50,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $2A,$28,$00,$55,$2A,$28,$10,$15,$02,$28,$15,$41,$28,$08,$15,$54           
.byte $28,$00,$15,$55,$28,$0A,$05,$55,$28,$0A,$50,$55,$00,$0A,$50,$15           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$0A,$50,$14,$00,$0A,$50,$54,$00,$0A,$00,$54,$00,$00,$00,$54           
.byte $00,$00,$00,$54,$00,$00,$00,$40,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $03,$00,$00,$00,$3F,$00,$00,$00,$7F,$00,$07,$00,$7F,$00,$7F,$00           
.byte $7F,$0F,$7F,$00,$7F,$7F,$7F,$01,$7F,$7F,$7F,$1F,$7F,$7F,$7F,$7F           
.byte $28,$0A,$50,$01,$2A,$0A,$50,$00,$2A,$00,$50,$00,$2A,$00,$00,$00           
.byte $2A,$00,$00,$00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$60,$00,$00,$00,$7E,$00,$70,$00,$7F,$00,$7F,$00,$7F           
.byte $00,$7F,$78,$7F,$40,$7F,$7F,$7F,$7C,$7F,$7F,$7F,$7F,$7F,$7F,$7F           

; Padding
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

; Tree (top)
.byte $00,$00,$00,$00,$00,$00,$00,$7F,$00,$30,$00,$76,$00,$5F,$40,$4D           
.byte $00,$69,$60,$3D,$00,$77,$38,$7F,$00,$7F,$58,$3F,$00,$17,$7C,$37           
.byte $7F,$7F,$7F,$00,$7F,$0F,$7F,$00,$7F,$00,$3F,$00,$7F,$00,$0F,$00           
.byte $7F,$00,$07,$00,$7F,$00,$03,$00,$7F,$00,$03,$00,$3F,$00,$00,$00           
.byte $00,$00,$00,$00,$1F,$00,$00,$00,$7F,$00,$01,$00,$6F,$00,$1F,$00           
.byte $6B,$00,$3F,$00,$7F,$00,$32,$00,$6F,$05,$6F,$00,$7B,$1B,$7E,$00           
.byte $60,$7F,$7F,$7F,$00,$7F,$7E,$7F,$00,$7F,$60,$7F,$00,$7F,$40,$7F           
.byte $00,$7F,$00,$7F,$00,$78,$00,$7F,$00,$60,$00,$7F,$00,$40,$00,$7F           
.byte $40,$7F,$5F,$7D,$70,$7B,$3F,$3F,$70,$3F,$7D,$77,$70,$77,$76,$7F           
.byte $3C,$6F,$7F,$75,$34,$5D,$75,$7E,$7C,$6F,$7D,$7F,$78,$5F,$7E,$7F           
.byte $0F,$00,$00,$00,$07,$00,$00,$00,$07,$00,$00,$00,$03,$00,$00,$00           
.byte $01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$03,$00,$00,$00           
.byte $4E,$37,$25,$00,$7F,$7E,$77,$00,$7F,$6F,$7B,$01,$71,$73,$6D,$01           
.byte $3F,$7E,$7F,$03,$7F,$7F,$7D,$07,$7E,$1F,$32,$07,$79,$63,$5B,$06           
.byte $00,$00,$00,$7F,$00,$00,$00,$7E,$00,$00,$00,$7C,$00,$00,$00,$7C           
.byte $00,$00,$00,$78,$00,$00,$00,$70,$00,$00,$00,$70,$00,$00,$00,$70           

; Tree (middle)
.byte $30,$5F,$7F,$7F,$70,$7F,$7F,$7F,$60,$7F,$7B,$7F,$60,$3F,$3F,$7D           
.byte $00,$7F,$7F,$7E,$00,$7F,$78,$7F,$7C,$00,$07,$78,$7F,$3F,$7F,$00           
.byte $07,$00,$00,$00,$07,$00,$00,$00,$0F,$00,$00,$00,$0F,$00,$00,$00           
.byte $3F,$00,$00,$00,$7F,$00,$00,$00,$03,$00,$00,$00,$00,$00,$00,$00           
.byte $7F,$5F,$7F,$07,$7F,$7F,$3F,$00,$7F,$3F,$7D,$00,$7E,$0D,$7F,$00           
.byte $7F,$07,$7B,$00,$7F,$70,$3E,$01,$5F,$7F,$43,$1F,$00,$7F,$70,$7F           
.byte $00,$00,$00,$70,$00,$00,$00,$7C,$00,$00,$00,$7E,$00,$00,$00,$7F           
.byte $00,$60,$00,$7F,$00,$00,$00,$7E,$00,$00,$00,$60,$00,$00,$00,$00           
.byte $7F,$3F,$7F,$55,$7D,$1F,$7F,$45,$49,$1F,$7F,$45,$48,$7F,$7D,$50           
.byte $4C,$7F,$75,$0F,$44,$71,$64,$7F,$00,$00,$60,$7E,$10,$08,$00,$60           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $08,$7F,$7F,$7F,$0A,$7F,$7F,$1F,$0A,$7F,$7E,$0F,$08,$7F,$7F,$46           
.byte $70,$1D,$7F,$00,$7F,$11,$17,$08,$7F,$01,$04,$00,$43,$08,$04,$04           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           

; Reeds (above)
.byte $00,$00,$00,$00,$00,$00,$40,$10,$40,$00,$40,$10,$40,$00,$40,$10           
.byte $40,$00,$40,$10,$00,$01,$01,$10,$00,$01,$01,$10,$00,$01,$01,$12           
.byte $7F,$7F,$7F,$7F,$7F,$7E,$3F,$4F,$1F,$7E,$3F,$4F,$1F,$7E,$3F,$4F           
.byte $1F,$7E,$3F,$0F,$3F,$7C,$7E,$01,$3F,$0C,$7E,$00,$3F,$00,$7E,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$20,$00,$04,$00,$20,$10,$04,$00           
.byte $20,$10,$08,$00,$20,$10,$08,$00,$20,$10,$08,$00,$20,$10,$08,$00           
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$1F,$7F,$73,$7F,$1F,$4F,$73,$7F           
.byte $1C,$4F,$67,$7F,$00,$4F,$67,$7F,$00,$4F,$60,$7F,$00,$4F,$00,$7F           
.byte $00,$01,$01,$12,$00,$01,$01,$12,$00,$01,$01,$12,$00,$01,$01,$12           
.byte $00,$01,$01,$02,$00,$00,$01,$02,$00,$00,$01,$02,$00,$00,$01,$02           
.byte $3F,$00,$06,$00,$3F,$00,$00,$00,$03,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $20,$10,$04,$00,$00,$10,$04,$00,$00,$10,$04,$00,$00,$10,$04,$00           
.byte $00,$08,$00,$00,$00,$08,$00,$00,$00,$08,$00,$00,$00,$00,$00,$00           
.byte $00,$40,$00,$7F,$00,$00,$00,$7E,$00,$00,$00,$60,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           

; Chair Left
.byte $00,$00,$00,$00,$00,$00,$00,$7C,$00,$40,$00,$77,$00,$60,$00,$7F           
.byte $00,$30,$00,$77,$00,$70,$00,$7F,$00,$30,$00,$1F,$00,$70,$00,$61           
.byte $7F,$7F,$7F,$03,$7F,$3F,$7F,$00,$7F,$1F,$7F,$00,$7F,$07,$7F,$00           
.byte $7F,$07,$7F,$00,$7F,$07,$7F,$00,$7F,$07,$7F,$00,$7F,$07,$7F,$00           
.byte $00,$00,$00,$00,$01,$00,$00,$00,$0E,$00,$00,$00,$0F,$00,$00,$00           
.byte $0E,$00,$00,$00,$0B,$00,$00,$00,$0C,$00,$00,$00,$1F,$00,$00,$00           
.byte $7E,$7F,$7F,$7F,$70,$7F,$7F,$7F,$60,$7F,$7F,$7F,$60,$7F,$7F,$7F           
.byte $60,$7F,$7F,$7F,$60,$7F,$7F,$7F,$60,$7F,$7F,$7F,$00,$7F,$78,$7F           
.byte $00,$10,$00,$7E,$00,$70,$00,$5F,$00,$70,$00,$7D,$00,$10,$00,$5E           
.byte $00,$10,$00,$60,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $7F,$07,$7F,$00,$7F,$07,$7F,$00,$7F,$07,$7F,$00,$7F,$07,$7F,$00           
.byte $7F,$47,$7F,$01,$7F,$6F,$7F,$1F,$7F,$7F,$7F,$3F,$7F,$7F,$7F,$3F           
.byte $7B,$00,$07,$00,$3F,$00,$0F,$00,$7B,$00,$09,$00,$0F,$00,$08,$00           
.byte $01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$7F,$60,$7F,$00,$7F,$60,$7F,$00,$7F,$60,$7F,$00,$7F,$62,$7F           
.byte $70,$7F,$77,$7F,$7C,$7F,$7F,$7F,$7C,$7F,$7F,$7F,$7C,$7F,$7F,$7F      

; Chair Right
.byte $00,$00,$00,$00,$00,$00,$00,$40,$00,$00,$00,$38,$00,$00,$00,$78           
.byte $00,$00,$00,$38,$00,$00,$00,$68,$00,$00,$00,$18,$00,$00,$00,$7C           
.byte $7F,$7F,$7F,$3F,$7F,$7F,$7F,$07,$7F,$7F,$7F,$03,$7F,$7F,$7F,$03           
.byte $7F,$7F,$7F,$03,$7F,$7F,$7F,$03,$7F,$7F,$7F,$03,$7F,$0F,$7F,$00           
.byte $00,$00,$00,$00,$1F,$00,$00,$00,$77,$00,$01,$00,$7F,$00,$03,$00           
.byte $77,$00,$06,$00,$7F,$00,$07,$00,$7C,$00,$06,$00,$43,$00,$07,$00           
.byte $60,$7F,$7F,$7F,$00,$7F,$7E,$7F,$00,$7F,$7C,$7F,$00,$7F,$70,$7F           
.byte $00,$7F,$70,$7F,$00,$7F,$70,$7F,$00,$7F,$70,$7F,$00,$7F,$70,$7F           
.byte $00,$70,$00,$6F,$00,$78,$00,$7E,$00,$48,$00,$6F,$00,$08,$00,$78           
.byte $00,$00,$00,$40,$00,$00,$00,$40,$00,$00,$00,$40,$00,$00,$00,$00           
.byte $7F,$03,$7F,$00,$7F,$03,$7F,$00,$7F,$03,$7F,$00,$7F,$23,$7F,$00           
.byte $7F,$77,$7F,$07,$7F,$7F,$7F,$1F,$7F,$7F,$7F,$1F,$7F,$7F,$7F,$1F           
.byte $3F,$00,$04,$00,$7D,$00,$07,$00,$5F,$00,$07,$00,$3D,$00,$04,$00           
.byte $03,$00,$04,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$7F,$70,$7F,$00,$7F,$70,$7F,$00,$7F,$70,$7F,$00,$7F,$70,$7F           
.byte $40,$7F,$71,$7F,$7C,$7F,$7B,$7F,$7E,$7F,$7F,$7F,$7E,$7F,$7F,$7F           

; cube -- patterned (floor)
.byte $00,$00,$00,$60,$00,$00,$00,$1E,$00,$70,$00,$01,$00,$0F,$00,$20           
.byte $00,$00,$78,$00,$40,$10,$07,$00,$3C,$00,$00,$00,$03,$00,$08,$00           
.byte $7F,$7F,$7F,$1F,$7F,$7F,$7F,$01,$7F,$0F,$7F,$00,$7F,$00,$7F,$00           
.byte $7F,$00,$07,$00,$3F,$00,$00,$00,$03,$00,$00,$00,$00,$00,$00,$00           
.byte $03,$00,$00,$00,$3C,$00,$00,$00,$40,$00,$07,$00,$00,$00,$78,$00           
.byte $00,$0F,$08,$00,$00,$70,$00,$01,$04,$00,$00,$1E,$00,$01,$00,$60           
.byte $7C,$7F,$7F,$7F,$40,$7F,$7F,$7F,$00,$7F,$78,$7F,$00,$7F,$00,$7F           
.byte $00,$70,$00,$7F,$00,$00,$00,$7E,$00,$00,$00,$60,$00,$00,$00,$00           
.byte $03,$00,$00,$02,$3D,$00,$00,$00,$41,$01,$07,$00,$05,$00,$78,$20           
.byte $41,$0F,$00,$00,$05,$70,$08,$01,$41,$01,$00,$1E,$05,$10,$08,$60           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$60,$40,$00,$00,$5E,$00,$70,$00,$61,$00,$0F,$00,$42           
.byte $00,$10,$78,$40,$40,$01,$07,$60,$3C,$00,$08,$42,$43,$10,$00,$40           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           

; cube -- patterned (below)
.byte $41,$01,$00,$42,$05,$10,$08,$60,$41,$01,$00,$42,$05,$10,$08,$60           
.byte $41,$01,$00,$42,$05,$10,$08,$60,$41,$01,$00,$42,$05,$10,$08,$60           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $04,$01,$00,$60,$00,$00,$08,$42,$40,$10,$00,$40,$04,$01,$00,$60           
.byte $00,$00,$08,$42,$40,$10,$00,$40,$04,$01,$00,$60,$00,$00,$08,$42           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $43,$01,$00,$42,$3C,$10,$08,$60,$40,$01,$07,$42,$00,$10,$78,$60           
.byte $00,$0F,$00,$42,$00,$70,$00,$61,$00,$00,$00,$5E,$00,$00,$00,$60           
.byte $00,$00,$00,$00,$03,$00,$00,$00,$3F,$00,$00,$00,$7F,$00,$07,$00           
.byte $7F,$00,$7F,$00,$7F,$0F,$7F,$00,$7F,$7F,$7F,$01,$7F,$7F,$7F,$1F           
.byte $40,$10,$00,$60,$04,$01,$00,$1E,$00,$70,$08,$01,$40,$0F,$00,$00           
.byte $04,$00,$78,$00,$40,$00,$07,$00,$3C,$00,$00,$00,$03,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$60,$00,$00,$00,$7E,$00,$70,$00,$7F           
.byte $00,$7F,$00,$7F,$00,$7F,$78,$7F,$40,$7F,$7F,$7F,$7C,$7F,$7F,$7F           

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

; Wizard (top)                                                                   
.byte $00,$00,$00,$00,$00,$60,$00,$3D,$00,$6C,$00,$7E,$00,$5A,$00,$7F           
.byte $00,$7F,$00,$1F,$00,$7D,$40,$47,$00,$7F,$20,$33,$00,$7D,$60,$59           
.byte $7F,$1F,$7F,$00,$7F,$03,$7F,$00,$7F,$01,$7F,$00,$7F,$00,$7F,$00           
.byte $7F,$00,$3F,$00,$7F,$00,$1F,$00,$7F,$00,$0F,$00,$7F,$00,$0F,$00           
.byte $00,$00,$00,$00,$6F,$00,$00,$00,$7F,$00,$07,$00,$7F,$00,$0F,$00           
.byte $00,$00,$18,$00,$7F,$00,$27,$00,$3C,$00,$4C,$00,$5B,$00,$5B,$00           
.byte $00,$7F,$7F,$7F,$00,$7F,$78,$7F,$00,$7F,$70,$7F,$00,$7F,$60,$7F           
.byte $00,$7F,$40,$7F,$00,$7F,$00,$7F,$00,$7E,$00,$7F,$00,$7E,$00,$7F           
.byte $00,$7F,$20,$7D,$00,$7D,$60,$7C,$00,$3F,$20,$76,$00,$1F,$60,$3B           
.byte $00,$4F,$20,$7B,$00,$6F,$40,$5E,$00,$66,$40,$7E,$00,$4D,$00,$6F           
.byte $7F,$00,$0F,$00,$7F,$00,$0F,$00,$7F,$00,$0F,$00,$7F,$00,$0F,$00           
.byte $7F,$00,$0F,$00,$7F,$00,$1F,$00,$7F,$00,$1F,$00,$7F,$00,$3F,$00           
.byte $7F,$00,$5F,$00,$0F,$00,$5F,$00,$77,$00,$5E,$00,$7F,$00,$5F,$00           
.byte $01,$00,$57,$00,$7F,$00,$4D,$00,$5A,$00,$6E,$00,$77,$00,$6A,$00           
.byte $00,$7E,$00,$7F,$00,$7E,$00,$7F,$00,$7E,$00,$7F,$00,$7E,$00,$7F           
.byte $00,$7E,$00,$7F,$00,$7E,$00,$7F,$00,$7E,$00,$7F,$00,$7E,$00,$7F           

; Wizard (bottom)
.byte $00,$1E,$00,$6F,$00,$3C,$00,$7C,$00,$78,$00,$63,$00,$36,$00,$1F           
.byte $00,$4E,$00,$7E,$00,$2A,$00,$7D,$00,$06,$00,$3A,$00,$60,$00,$01           
.byte $7F,$00,$7F,$00,$7F,$01,$7F,$00,$7F,$01,$7F,$00,$7F,$00,$7F,$00           
.byte $7F,$00,$7F,$00,$7F,$00,$7F,$00,$7F,$00,$7F,$00,$7F,$01,$7F,$00           
.byte $75,$00,$26,$00,$5D,$00,$3B,$00,$5F,$00,$3D,$00,$38,$01,$56,$00           
.byte $47,$01,$4B,$00,$79,$02,$55,$00,$7E,$03,$03,$00,$00,$00,$0C,$00           
.byte $00,$7F,$00,$7F,$00,$7F,$00,$7F,$00,$7E,$00,$7F,$00,$7C,$00,$7F           
.byte $00,$7C,$00,$7F,$00,$78,$00,$7F,$00,$78,$00,$7F,$00,$7C,$00,$7F           
.byte $00,$30,$00,$7F,$00,$78,$00,$7F,$00,$18,$00,$6C,$00,$60,$00,$03           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $7F,$03,$7F,$00,$7F,$03,$7F,$00,$7F,$03,$7F,$00,$7F,$07,$7F,$00           
.byte $7F,$1F,$7F,$7C,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
.byte $7F,$00,$1F,$00,$7F,$00,$3F,$00,$75,$00,$30,$00,$00,$00,$0F,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$7F,$40,$7F,$00,$7F,$00,$7F,$00,$7F,$00,$7F,$00,$7F,$00,$7F           
.byte $7F,$7F,$70,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           

; Robot (top)
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$61,$00,$00,$00,$79           
.byte $00,$00,$00,$7D,$00,$00,$00,$47,$00,$0C,$30,$47,$00,$18,$18,$7E           
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$1E,$7F,$3F,$7F,$04,$7F,$3F,$7F,$00           
.byte $7F,$3F,$7F,$00,$7F,$33,$4F,$00,$7F,$21,$07,$00,$7F,$43,$43,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$03,$00,$00,$00,$0F,$00,$00,$00           
.byte $1F,$00,$00,$00,$31,$00,$00,$00,$31,$00,$00,$00,$3F,$00,$00,$00           
.byte $7F,$7F,$7F,$7F,$7C,$7F,$7F,$7F,$70,$7F,$7F,$7F,$60,$7F,$7F,$7F           
.byte $40,$7F,$7F,$7F,$00,$7F,$7F,$7F,$00,$7F,$7F,$7F,$00,$7F,$7F,$7F           
.byte $00,$0C,$30,$7E,$00,$07,$60,$4E,$00,$03,$40,$7C,$00,$03,$00,$00           
.byte $00,$04,$00,$70,$00,$08,$00,$5A,$00,$10,$00,$29,$00,$60,$00,$58           
.byte $7F,$60,$07,$00,$7F,$70,$0F,$00,$7F,$78,$1F,$01,$7F,$78,$3F,$03           
.byte $7F,$70,$7F,$01,$7F,$63,$7F,$00,$7F,$07,$7F,$00,$7F,$0F,$7F,$00           
.byte $3F,$00,$00,$00,$39,$00,$00,$00,$1F,$00,$00,$00,$00,$00,$00,$00           
.byte $07,$00,$00,$00,$2A,$00,$00,$00,$4D,$03,$60,$00,$0A,$04,$31,$00           
.byte $00,$7F,$7F,$7F,$00,$7F,$7F,$7F,$40,$7F,$7F,$7F,$60,$7F,$7F,$7F           
.byte $40,$7F,$7F,$7F,$00,$7C,$1F,$7F,$00,$78,$0E,$7F,$00,$70,$00,$7F           

; Robot (bottom)
.byte $00,$00,$00,$28,$00,$00,$00,$58,$00,$00,$00,$78,$00,$00,$00,$70           
.byte $00,$00,$00,$04,$00,$00,$00,$06,$00,$00,$00,$0C,$00,$00,$00,$0C           
.byte $7F,$1F,$7F,$03,$7F,$7F,$7F,$03,$7F,$7F,$7F,$03,$7F,$7F,$7F,$01           
.byte $7F,$7F,$7F,$01,$7F,$7F,$7F,$70,$7F,$7F,$7F,$61,$7F,$7F,$7F,$61           
.byte $0D,$00,$3E,$00,$0A,$04,$30,$00,$0F,$03,$60,$00,$07,$00,$00,$00           
.byte $10,$00,$00,$00,$30,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00           
.byte $60,$78,$00,$7F,$60,$70,$01,$7F,$60,$78,$0F,$7F,$40,$7C,$1F,$7F           
.byte $40,$7F,$7F,$7F,$07,$7F,$7F,$7F,$43,$7F,$7F,$7F,$43,$7F,$7F,$7F           
.byte $00,$00,$00,$0C,$00,$40,$00,$01,$00,$70,$00,$0F,$00,$70,$00,$0F           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $7F,$3F,$7F,$60,$7F,$0F,$7F,$60,$7F,$07,$7F,$60,$7F,$07,$7F,$60           
.byte $7F,$0F,$7F,$60,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
.byte $18,$00,$00,$00,$40,$00,$01,$00,$78,$00,$07,$00,$78,$00,$07,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $03,$7F,$7E,$7F,$03,$7F,$78,$7F,$03,$7F,$70,$7F,$03,$7F,$70,$7F           
.byte $03,$7F,$78,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           

; Pond Rock
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$7F,$00,$7E,$00,$7B,$00,$75,$00,$7F           
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
.byte $7F,$7F,$7F,$00,$7F,$01,$7F,$00,$7F,$00,$7F,$00,$7F,$00,$3F,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$1F,$00,$00,$00,$7F,$00,$0F,$00,$7F,$00,$77,$00           
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
.byte $60,$7F,$7F,$7F,$00,$7F,$70,$7F,$00,$7F,$00,$7F,$00,$7C,$00,$7F           
.byte $00,$5F,$40,$7F,$00,$7B,$20,$7F,$00,$5F,$60,$77,$00,$75,$44,$7F           
.byte $00,$5E,$08,$36,$00,$60,$20,$7F,$00,$07,$00,$00,$00,$70,$00,$5F           
.byte $7F,$00,$07,$00,$3F,$00,$00,$00,$03,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $7F,$03,$7F,$00,$7F,$06,$6F,$00,$7F,$0F,$3F,$00,$7D,$4D,$6B,$00           
.byte $2F,$27,$7F,$00,$7F,$10,$0F,$00,$00,$07,$40,$00,$7F,$00,$07,$00           
.byte $00,$70,$00,$7F,$00,$00,$00,$7E,$00,$00,$00,$60,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           

; water (animated)
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$1F,$7F,$7F,$7F,$01,$7F,$0F,$7F,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
.byte $7F,$7F,$7F,$7F,$7C,$7F,$7F,$7F,$40,$7F,$7F,$7F,$00,$7F,$78,$7F           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $7F,$00,$7F,$00,$7F,$00,$07,$00,$3F,$00,$00,$00,$03,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
.byte $00,$7F,$00,$7F,$00,$70,$00,$7F,$00,$00,$00,$7E,$00,$00,$00,$60           
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           

tileSheet_iso_end:

.res    8192-(tileSheet_iso_end-tileSheet_iso_start)




