;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; Loader
;
;   Load game assets and install them into proper memory location
;   which could either be main, auxiliary or both.

.include "defines.asm"
.include "macros.asm"

; Proposed memory map (may change)
;------------------------------------------------
;
;               Main                Aux
;
;   0000-07FF   [ System usage / text pages     ]
;
;   0800-09FF   [ ProDos buffer ][ Unused       ]
;
;   0A00-0AFF   [ Unused                        ]
;   0B00-0BFF   [ Map Buffer    ][ Unused       ]
;   0C00-1FFF   [ Engine routines               ]
;
;   2000-3FFF   [ DGHR Page 1                   ]
;               [ Loader        ]
;
;   4000-5FFF   [ DGHR Page 2                   ]
;               [ Read data     ]
;               [ Tool(s)       ]
;
;   6000-7FFF   [ Game          ][ Map 64x64x2  ]
;   8000-8FFF   [               ][ Dialog       ]
;
;   9000-AFFF   [ Isometric Tiles (64)          ]
;
;   B000-B7FF   [ Font Tiles (128)              ]
;
;   Is B800-BEFF available?  (BFxx is used by ProDos)
;
;   ProDos says addresses D000-FFFF in AUX memory
;   are reserved, but probably could be used if needed.
;
;   ProDos buffer only needed if load/save supported
;

READBUFFER          :=  $4000    ; Share read buffer with page2

MAPSTART            :=  $6000
MAPLENGTH           =   64*64*2
MAPEND              :=  READBUFFER + MAPLENGTH - 1

; Not yet implemented
; DIALOGSTART       :=  $8000
; DIALOGLENGTH      =   $1000
; DIALOGEND         :=  READBUFFER + DIALOGLENGTH - 1

; BGSTART           :=  $9000
; BGLENGTH          =   128*64
; BGEND             :=  READBUFFER + BGLENGTH - 1
; BGI4END           :=  BGSTART + BGLENGTH/2 - 1

ISOSTART            :=  $A000
ISOLENGTH           =   256*16
ISOEND              :=  READBUFFER + ISOLENGTH - 1
ISOI2END            :=  ISOSTART + ISOLENGTH/2 - 1

FONT0START          :=  $B000
FONT0LENGTH         =   8*128
FONT0END            :=  READBUFFER + FONT0LENGTH - 1
FONT0I2END          :=  FONT0START + FONT0LENGTH/2 - 1

; FONT2START        :=  $B400
; FONT2LENGTH       =   32*64
; FONT2END          :=  READBUFFER + FONT2LENGTH - 1
; FONT2I2END        :=  FONT2START + FONT2LENGTH/2 - 1

ENGINESTART         :=  $C00
ENGINELENGTH        =   $2000 - ENGINESTART

GAMESTART           :=  $6000
GAMELENGTH          =   $9000 - GAMESTART

FONTEDITSTART       :=  $4000
FONTEDITLENGTH      =   $6000 - FONTEDITSTART

MAPEDITSTART        :=  $4000
MAPEDITLENGTH       =   $6000 - MAPEDITSTART

TILEEDITSTART       :=  $6000
TILEEDITLENGTH      =   $9000 - TILEEDITSTART

;------------------------------------------------
; Constants
;------------------------------------------------

INSTALL_MAIN    = 0     ; Main memory
INSTALL_AUX     = 1     ; Aux memory
INSTALL_BOTH    = 3     ; Both main and aux
INSTALL_AUX_I2  = 2     ; Aux memory, interleave of 2
INSTALL_AUX_I4  = 4     ; Aux memory, interleave of 4

;------------------------------------------------
; Constants
;------------------------------------------------

;------------------------------------------------

.segment "CODE"
.org    $2000

;=============================================================================
; Main program
;=============================================================================

; Main
;------------------------------------------------
.proc main

    jsr     init

    jsr    inline_print
    StringCont "Welcome to 128k game loader."
    StringCR   "Checking memory..."

    lda     $BF98
    bmi     :+

    jsr    inline_print
    StringCR "128K memory not detected, exiting"
    jmp     monitor
:

    jsr    inline_print
    StringCR "Loading game assets..."


    ldx     #assetFont0
    jsr     loadAsset
    ldx     #assetISO
    jsr     loadAsset
    ldx     #assetEngine
    jsr     loadAsset
    ldx     #assetTileEdit
    jsr     loadAsset

    ; Font edit lives in read buffer, so must be last!
;    ldx     #assetFontEdit
;    jsr     loadAsset
    ldx     #assetMapEdit
    jsr     loadAsset

    lda     fileError
    beq     :+

    jsr     inline_print
    StringCR "Error detected"
    jmp     monitor

:

    ; Initialize engine
    lda     #<ISOSTART
    sta     DHGR_TILE_28X8
    lda     #>ISOSTART
    sta     DHGR_TILE_28X8+1

    lda     #<FONT0START
    sta     DHGR_TILE_7X8
    lda     #>FONT0START
    sta     DHGR_TILE_7X8+1

    jsr     DHGR_INIT

    lda     KBD
    bpl     :+
    sta     KBDSTRB

    cmp     #KEY_ESC
    bne     :+

    jsr     inline_print
    StringCR "ESC key press, exiting"
    jmp     monitor

:

    jsr     inline_print
    StringCont "Launching executable..."
    .byte   13,0

    ; Jump to executables
    jmp     MAPEDITSTART
    ;jmp     TILEEDITSTART
    ;jmp     DHGR_TEST
.endproc


;-----------------------------------------------------------------------------
; Init
;-----------------------------------------------------------------------------
.proc init

    ; Set up text screen
    jsr     $c300       ; 80 column mode
    jsr     HOME        ; clear screen
    lda     #23         ; put cursor on last line
    sta     CV
    jsr     VTAB

    ; Set ctrl-y vector
    lda     #$4c        ; JMP
    sta     $3f8
    lda     #<quit
    sta     $3f9
    lda     #>quit
    sta     $3fa

    ; Clear errors
    lda     #0
    sta     fileError

    rts
.endproc

;-----------------------------------------------------------------------------
; Monitor
;
;  Exit to monitor
;-----------------------------------------------------------------------------
.proc monitor

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

quit_params:
    .byte   4               ; 4 parameters
    .byte   0               ; 0 is the only quit type
    .word   0               ; Reserved pointer for future use (what future?)
    .byte   0               ; Reserved byte for future use (what future?)
    .word   0               ; Reserved pointer for future use (what future?)

.endproc


;-----------------------------------------------------------------------------
; Load Asset
;
;   Pass asset # * 16 in X
;-----------------------------------------------------------------------------

.proc loadAsset

    stx     assetNum

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
    lda     fileDescription+3,x
    sta     open_params+2

    ; set address
    lda     fileDescription+4,x
    sta     read_params+2
    lda     fileDescription+5,x
    sta     read_params+3

    ; set size
    lda     fileDescription+6,x
    sta     read_params+4
    lda     fileDescription+7,x
    sta     read_params+5


    jsr     loadData

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

    jsr     setCopyParam
    sec                     ; copy from main to aux
    jsr     AUXMOVE

    rts
:

    ; Note that install both is same as install aux
    cmp     #INSTALL_BOTH
    bne     :+

    jsr     inline_print
    String "(main/aux duplicated) $"
    jsr     printDest

    jsr     setCopyParam
    sec                     ; copy from main to aux
    jsr     AUXMOVE

    rts
:

    cmp     #INSTALL_AUX_I2
    bne     :+
    jsr     inline_print
    String "(main/aux interleave 2) $"
    jsr     printDest

    jsr     setCopyParam
    jsr     interleaveCopy2A

    jsr     setCopyParamInterleave
    sec
    jsr     AUXMOVE

    jsr     setCopyParam
    jsr     interleaveCopy2B


    rts
:

    ;       #INSTALL_AUX_I4
    jsr     inline_print
    String "(main/aux interleave 4) $"
    jsr     printDest

    jsr     setCopyParam
    jsr     interleaveCopy4A

    jsr     setCopyParamInterleave
    sec
    jsr     AUXMOVE

    jsr     setCopyParam
    jsr     interleaveCopy4B

    rts

moveCopyBuffer:

    ldy     #0
:
    lda     copyBuffer,y
    sta     (A4),y
    dey
    bne     :-

    inc     A4+1

    rts

interleaveCopy2A:
    ldy     #0
    ldx     #0

copyLoop2A:
    ; copy 2 bytes
    lda     (A1),y
    sta     copyBuffer,x
    inx
    iny
    lda     (A1),y
    sta     copyBuffer,x
    inx
    iny
    ; skip 2 bytes
    iny
    iny
    bne     copyLoop2A

    ; inc source page
    inc     A1+1

    ; check if buffer full
    cpx     #0
    bne     copyLoop2A

    jsr     moveCopyBuffer

    ; check if done
    dec     copyLength
    bne     copyLoop2A

    rts

interleaveCopy2B:
    ldy     #0
    ldx     #0

copyLoop2B:
    ; skip 2 bytes
    iny
    iny
    ; copy 2 bytes
    lda     (A1),y
    sta     copyBuffer,x
    inx
    iny
    lda     (A1),y
    sta     copyBuffer,x
    inx
    iny
    bne     copyLoop2B

    ; inc source page
    inc     A1+1

    ; check if buffer full
    cpx     #0
    bne     copyLoop2B

    jsr     moveCopyBuffer

    ; check if done
    dec     copyLength
    bne     copyLoop2B
    rts

interleaveCopy4A:
    ldy     #0
    ldx     #0

copyLoop4A:
    ; copy 4 bytes
    lda     (A1),y
    sta     copyBuffer,x
    inx
    iny
    lda     (A1),y
    sta     copyBuffer,x
    inx
    iny
    lda     (A1),y
    sta     copyBuffer,x
    inx
    iny
    lda     (A1),y
    sta     copyBuffer,x
    inx
    iny
    ; skip 4 bytes
    iny
    iny
    iny
    iny
    bne     copyLoop4A

    ; inc source page
    inc     A1+1

    ; check if buffer full
    cpx     #0
    bne     copyLoop4A

    jsr     moveCopyBuffer

    ; check if done
    dec     copyLength
    bne     copyLoop4A

    rts

interleaveCopy4B:
    ldy     #0
    ldx     #0

copyLoop4B:
    ; skip 4 bytes
    iny
    iny
    iny
    iny
    ; copy 4 bytes
    lda     (A1),y
    sta     copyBuffer,x
    inx
    iny
    lda     (A1),y
    sta     copyBuffer,x
    inx
    iny
    lda     (A1),y
    sta     copyBuffer,x
    inx
    iny
    lda     (A1),y
    sta     copyBuffer,x
    inx
    iny
    bne     copyLoop4B

    ; inc source page
    inc     A1+1

    ; check if buffer full
    cpx     #0
    bne     copyLoop4B

    jsr     moveCopyBuffer

    ; check if done
    dec     copyLength
    bne     copyLoop4B
    rts


setCopyParam:
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

setCopyParamInterleave:
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

printDest:
    ldx     assetNum
    lda     fileDescription+10,x
    ldy     fileDescription+11,x
    tax
    jsr     PRINTXY
    lda     #13
    jsr     COUT
    rts

assetNum:       .byte   0
copyLength:     .byte   0

.align  256
copyBuffer:     .res    256

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
    sta     read_params+1
    sta     close_params+1

    ; read data
    jsr    MLI
    .byte  CMD_READ
    .word  read_params
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
; Global ProDos parameters
;-----------------------------------------------------------------------------

fileError:  .byte   0

open_params:
    .byte   $3
    .word   $0                  ; *OVERWRITE* pathname
    .word   FILEBUFFER
    .byte   $0                  ;             reference number

read_params:
    .byte   $4
    .byte   $0                  ;             reference number
    .word   $0                  ; *OVERWRITE* address of data buffer
    .word   $0                  ; *OVERWRITE* number of bytes to read
    .word   $0                  ;             number of bytes read

close_params:
    .byte   $1
    .byte   $0                  ;             reference number


;-----------------------------------------------------------------------------
; Assets

; Asset type
fileTypeFont:   String "Font Tilesheet"
fileTypeISO:    String "Isometric Tilesheet"
fileTypeExe:    String "Executable"

; File names
fileNameFont0:      StringLen "/DHGR/DATA/FONT7X8.0"
fileNameISO:        StringLen "/DHGR/DATA/TILESHEET.0"
fileNameEngine:     StringLen "/DHGR/DATA/ENGINE"
fileNameGame:       StringLen "/DHGR/DATA/GAME"
fileNameFontEdit:   StringLen "/DHGR/DATA/FONTEDIT"
fileNameMapEdit:    StringLen "/DHGR/DATA/MAPEDIT"
fileNameTileEdit:   StringLen "/DHGR/DATA/TILEEDIT"

; Asset List
fileDescription:    ; type, name, address, size, dest, interleave
    ;       TYPE            NAME              BUFFER          LENGTH          END         STARTDEST       MODE            DESTEND (INT)   OFFSET
    ;       0               2                 4               6               8           10              12              14
    ;       --------------- ---------------   -----------     -----------     ----------- -----------     --------------- --------------- -------
    .word   fileTypeFont,   fileNameFont0,    FONT0START,     FONT0LENGTH,    FONT0END,   FONT0START,     INSTALL_BOTH,   0               ; 0
    .word   fileTypeISO,    fileNameISO,      READBUFFER,     ISOLENGTH,      ISOEND,     ISOSTART,       INSTALL_AUX_I2, ISOI2END        ; 16
    .word   fileTypeExe,    fileNameEngine,   ENGINESTART,    ENGINELENGTH,   0,          ENGINESTART,    INSTALL_MAIN,   0               ; 32
    .word   fileTypeExe,    fileNameGame,     GAMESTART,      GAMELENGTH,     0,          GAMESTART,      INSTALL_MAIN,   0               ; 48
    .word   fileTypeExe,    fileNameFontEdit, FONTEDITSTART,  FONTEDITLENGTH, 0,          FONTEDITSTART,  INSTALL_MAIN,   0               ; 64
    .word   fileTypeExe,    fileNameTileEdit, TILEEDITSTART,  TILEEDITLENGTH, 0,          TILEEDITSTART,  INSTALL_MAIN,   0               ; 80
    .word   fileTypeExe,    fileNameMapEdit,  MAPEDITSTART,   MAPEDITLENGTH,  0,          MAPEDITSTART,   INSTALL_MAIN,   0               ; 96

assetFont0    =   16*0
assetISO      =   16*1
assetEngine   =   16*2
assetGame     =   16*3
assetFontEdit =   16*4
assetTileEdit =   16*5
assetMapEdit  =   16*6

;-----------------------------------------------------------------------------
; Utilies

.include "inline_print.asm"
