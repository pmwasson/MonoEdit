;-----------------------------------------------------------------------------
; Paul Wasson - 2023
;-----------------------------------------------------------------------------
; Apple2e Font (7x8)
;-----------------------------------------------------------------------------

.align 256

; Mouse text
.byte $10, $08, $36, $7F, $3F, $3F, $7E, $36    ; $00 ^@ 
.byte $10, $08, $36, $41, $21, $21, $4A, $36    ; $01 ^A 
;.byte $00, $00, $02, $06, $0E, $1E, $36, $42    ; $02 ^B
.byte $00, $08, $08, $36, $08, $08, $00, $00 	; cross-hair

.byte $7F, $22, $14, $08, $08, $14, $2A, $7F    ; $03 ^C 
.byte $00, $40, $20, $11, $0A, $04, $04, $00    ; $04 ^D 
.byte $7F, $3F, $5F, $6C, $75, $7B, $7B, $7F    ; $05 ^E 
.byte $70, $60, $7E, $31, $79, $30, $3F, $02    ; $06 ^F 
.byte $00, $18, $07, $00, $07, $0C, $08, $70    ; $07 ^G 
.byte $08, $04, $02, $7F, $02, $04, $08, $00    ; $08 ^H 
.byte $00, $00, $00, $00, $00, $00, $00, $2A    ; $09 ^I 
.byte $08, $08, $08, $08, $49, $2A, $1C, $08    ; $0A ^J 
.byte $08, $1C, $2A, $49, $08, $08, $08, $08    ; $0B ^K 
.byte $7F, $00, $00, $00, $00, $00, $00, $00    ; $0C ^L 
; .byte $40, $40, $40, $44, $46, $7F, $06, $04    ; $0D ^M 
.byte $08, $08, $1C, $F7, $1C, $08, $08, $08                                           

; Modify white box
; .byte $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F    ; $0E ^N
.byte $3F, $3F, $3F, $3F, $3F, $3F, $3F, $00    ; white-pixel

;.byte $13, $18, $1C, $7E, $1C, $18, $10, $6F    ; $0F ^O 
;.byte $64, $0C, $1C, $3F, $1C, $0C, $04, $7B    ; $10 ^P 
;.byte $40, $48, $08, $7F, $3E, $1C, $48, $40    ; $11 ^Q 
;.byte $40, $48, $1C, $3E, $7E, $08, $48, $40    ; $12 ^R 
.byte  $08, $08, $1C, $F4, $1C, $08, $08, $08    ; tee 
.byte  $00, $00, $1C, $F7, $1C, $08, $08, $08    ; tee 
.byte  $08, $08, $1C, $97, $1C, $08, $08, $08    ; tee 
.byte  $08, $08, $1C, $F7, $1C, $00, $00, $00    ; tee 

.byte $00, $00, $00, $7F, $00, $00, $00, $00    ; $13 ^S 
.byte $01, $01, $01, $01, $01, $01, $01, $7F    ; $14 ^T 
.byte $08, $10, $20, $7F, $20, $10, $08, $00    ; $15 ^U 
.byte $2A, $55, $2A, $55, $2A, $55, $2A, $55    ; $16 ^V 
.byte $55, $2A, $55, $2A, $55, $2A, $55, $2A    ; $17 ^W 
.byte $00, $3E, $41, $01, $01, $01, $7F, $00    ; $18 ^X 
.byte $00, $00, $3F, $40, $40, $40, $7F, $00    ; $19 ^Y 
.byte $40, $40, $40, $40, $40, $40, $40, $40    ; $1A ^Z 
.byte $08, $1C, $3E, $7F, $3E, $1C, $08, $00    ; $1B ^[ 

; Replace the last 4 mouse-text with box corners
;
; .byte $7F, $00, $00, $00, $00, $00, $00, $7F    ; $1C ^\ 
; .byte $14, $14, $77, $00, $77, $14, $14, $00    ; $1D ^] 
; .byte $7F, $40, $40, $4C, $4C, $40, $40, $7F    ; $1E ^^ 
; .byte $01, $01, $01, $01, $01, $01, $01, $01    ; $1F ^_
.byte $00, $00, $1C, $F4, $1C, $08, $08, $08    ; Box NW Corner                        
.byte $00, $00, $1C, $17, $1C, $08, $08, $08    ; Box NE Corner                        
.byte $08, $08, $1C, $F4, $1C, $00, $00, $00    ; Box SW Corner                        
.byte $08, $08, $1C, $17, $1C, $00, $00, $00    ; Box SE Corner                        


; Numeric
.byte $00, $00, $00, $00, $00, $00, $00, $00    ; $20   
.byte $08, $08, $08, $08, $08, $00, $08, $00    ; $21 ! 
.byte $14, $14, $14, $00, $00, $00, $00, $00    ; $22 " 
.byte $14, $14, $3E, $14, $3E, $14, $14, $00    ; $23 # 
.byte $08, $3C, $0A, $1C, $28, $1E, $08, $00    ; $24 $ 
.byte $06, $26, $10, $08, $04, $32, $30, $00    ; $25 % 
.byte $04, $0A, $0A, $04, $2A, $12, $2C, $00    ; $26 & 
.byte $08, $08, $08, $00, $00, $00, $00, $00    ; $27 ' 
.byte $08, $04, $02, $02, $02, $04, $08, $00    ; $28 ( 
.byte $08, $10, $20, $20, $20, $10, $08, $00    ; $29 ) 
.byte $08, $2A, $1C, $08, $1C, $2A, $08, $00    ; $2A * 
.byte $00, $08, $08, $3E, $08, $08, $00, $00    ; $2B + 
.byte $00, $00, $00, $00, $08, $08, $04, $00    ; $2C , 
.byte $00, $00, $00, $3E, $00, $00, $00, $00    ; $2D - 
.byte $00, $00, $00, $00, $00, $00, $08, $00    ; $2E . 
.byte $00, $20, $10, $08, $04, $02, $00, $00    ; $2F / 
.byte $1C, $22, $32, $2A, $26, $22, $1C, $00    ; $30 0 
.byte $08, $0C, $08, $08, $08, $08, $1C, $00    ; $31 1 
.byte $1C, $22, $20, $18, $04, $02, $3E, $00    ; $32 2 
.byte $3E, $20, $10, $18, $20, $22, $1C, $00    ; $33 3 
.byte $10, $18, $14, $12, $3E, $10, $10, $00    ; $34 4 
.byte $3E, $02, $1E, $20, $20, $22, $1C, $00    ; $35 5 
.byte $38, $04, $02, $1E, $22, $22, $1C, $00    ; $36 6 
.byte $3E, $20, $10, $08, $04, $04, $04, $00    ; $37 7 
.byte $1C, $22, $22, $1C, $22, $22, $1C, $00    ; $38 8 
.byte $1C, $22, $22, $3C, $20, $10, $0E, $00    ; $39 9 
.byte $00, $00, $08, $00, $08, $00, $00, $00    ; $3A : 
.byte $00, $00, $08, $00, $08, $08, $04, $00    ; $3B ; 
.byte $10, $08, $04, $02, $04, $08, $10, $00    ; $3C < 
.byte $00, $00, $3E, $00, $3E, $00, $00, $00    ; $3D = 
.byte $04, $08, $10, $20, $10, $08, $04, $00    ; $3E > 
.byte $1C, $22, $10, $08, $08, $00, $08, $00    ; $3F ? 

; Upper Case  
.byte $1C, $22, $2A, $3A, $1A, $02, $3C, $00    ; $40 @ 
.byte $08, $14, $22, $22, $3E, $22, $22, $00    ; $41 A 
.byte $1E, $22, $22, $1E, $22, $22, $1E, $00    ; $42 B 
.byte $1C, $22, $02, $02, $02, $22, $1C, $00    ; $43 C 
.byte $1E, $22, $22, $22, $22, $22, $1E, $00    ; $44 D 
.byte $3E, $02, $02, $1E, $02, $02, $3E, $00    ; $45 E 
.byte $3E, $02, $02, $1E, $02, $02, $02, $00    ; $46 F 
.byte $3C, $02, $02, $02, $32, $22, $3C, $00    ; $47 G 
.byte $22, $22, $22, $3E, $22, $22, $22, $00    ; $48 H 
.byte $1C, $08, $08, $08, $08, $08, $1C, $00    ; $49 I 
.byte $20, $20, $20, $20, $20, $22, $1C, $00    ; $4A J 
.byte $22, $12, $0A, $06, $0A, $12, $22, $00    ; $4B K 
.byte $02, $02, $02, $02, $02, $02, $3E, $00    ; $4C L 
.byte $22, $36, $2A, $2A, $22, $22, $22, $00    ; $4D M 
.byte $22, $22, $26, $2A, $32, $22, $22, $00    ; $4E N 
.byte $1C, $22, $22, $22, $22, $22, $1C, $00    ; $4F O 
.byte $1E, $22, $22, $1E, $02, $02, $02, $00    ; $50 P 
.byte $1C, $22, $22, $22, $2A, $12, $2C, $00    ; $51 Q 
.byte $1E, $22, $22, $1E, $0A, $12, $22, $00    ; $52 R 
.byte $1C, $22, $02, $1C, $20, $22, $1C, $00    ; $53 S 
.byte $3E, $08, $08, $08, $08, $08, $08, $00    ; $54 T 
.byte $22, $22, $22, $22, $22, $22, $1C, $00    ; $55 U 
.byte $22, $22, $22, $22, $22, $14, $08, $00    ; $56 V 
.byte $22, $22, $22, $2A, $2A, $36, $22, $00    ; $57 W 
.byte $22, $22, $14, $08, $14, $22, $22, $00    ; $58 X 
.byte $22, $22, $14, $08, $08, $08, $08, $00    ; $59 Y 
.byte $3E, $20, $10, $08, $04, $02, $3E, $00    ; $5A Z 
.byte $3E, $06, $06, $06, $06, $06, $3E, $00    ; $5B [ 
.byte $00, $02, $04, $08, $10, $20, $00, $00    ; $5C \ 
.byte $3E, $30, $30, $30, $30, $30, $3E, $00    ; $5D ] 
.byte $00, $00, $08, $14, $22, $00, $00, $00    ; $5E ^ 
.byte $00, $00, $00, $00, $00, $00, $00, $7F    ; $5F _ 

; Lower Case
.byte $04, $08, $10, $00, $00, $00, $00, $00    ; $60 ` 
.byte $00, $00, $1C, $20, $3C, $22, $3C, $00    ; $61 a 
.byte $02, $02, $1E, $22, $22, $22, $1E, $00    ; $62 b 
.byte $00, $00, $3C, $02, $02, $02, $3C, $00    ; $63 c 
.byte $20, $20, $3C, $22, $22, $22, $3C, $00    ; $64 d 
.byte $00, $00, $1C, $22, $3E, $02, $3C, $00    ; $65 e 
.byte $18, $24, $04, $1E, $04, $04, $04, $00    ; $66 f 
.byte $00, $00, $1C, $22, $22, $3C, $20, $1C    ; $67 g 
.byte $02, $02, $1E, $22, $22, $22, $22, $00    ; $68 h 
.byte $08, $00, $0C, $08, $08, $08, $1C, $00    ; $69 i 
.byte $10, $00, $18, $10, $10, $10, $12, $0C    ; $6A j 
.byte $02, $02, $22, $12, $0E, $12, $22, $00    ; $6B k 
.byte $0C, $08, $08, $08, $08, $08, $1C, $00    ; $6C l 
.byte $00, $00, $36, $2A, $2A, $2A, $22, $00    ; $6D m 
.byte $00, $00, $1E, $22, $22, $22, $22, $00    ; $6E n 
.byte $00, $00, $1C, $22, $22, $22, $1C, $00    ; $6F o 
.byte $00, $00, $1E, $22, $22, $1E, $02, $02    ; $70 p 
.byte $00, $00, $3C, $22, $22, $3C, $20, $20    ; $71 q 
.byte $00, $00, $3A, $06, $02, $02, $02, $00    ; $72 r 
.byte $00, $00, $3C, $02, $1C, $20, $1E, $00    ; $73 s 
.byte $04, $04, $1E, $04, $04, $24, $18, $00    ; $74 t 
.byte $00, $00, $22, $22, $22, $32, $2C, $00    ; $75 u 
.byte $00, $00, $22, $22, $22, $14, $08, $00    ; $76 v 
.byte $00, $00, $22, $22, $2A, $2A, $36, $00    ; $77 w 
.byte $00, $00, $22, $14, $08, $14, $22, $00    ; $78 x 
.byte $00, $00, $22, $22, $22, $3C, $20, $1C    ; $79 y 
.byte $00, $00, $3E, $10, $08, $04, $3E, $00    ; $7A z 
.byte $38, $0C, $0C, $06, $0C, $0C, $38, $00    ; $7B { 
.byte $08, $08, $08, $08, $08, $08, $08, $08    ; $7C | 
.byte $0E, $18, $18, $30, $18, $18, $0E, $00    ; $7D } 
.byte $2C, $1A, $00, $00, $00, $00, $00, $00    ; $7E ~ 
.byte $00, $2A, $14, $2A, $14, $2A, $00, $00    ; $7F  
