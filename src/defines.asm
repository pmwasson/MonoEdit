;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------
; Predefined memory/ROM locations
;
; Mostly added as needed
; Tried to use standard names if known
;-----------------------------------------------------------------------------

; Grab ca65 defines to start with and then add missing ones
.include "apple2.inc"


; Zero Page
;---------------------------------------------------------
; Safe zero page locations from Inside the Apple IIe (table 2-5):
;
;                         $06 $07 
; $08 $09
;     $19 $1A $1B $1C $1D $1E
;                         $CE $CF
;                             $D7
;             $E3
;             $EB $EC $ED $EE $EF
;         $FA $FB $FC $FD $FE $FF 

; ROM defined
;-------------
A1              :=  $3c
A2              :=  $3e
A4              :=  $42

; User defined
;--------------
; Pointers
tilePtr0        :=  $06     ; Tile pointer
tilePtr1        :=  $07
maskPtr0        :=  $19     ; mask pointer
maskPtr1        :=  $1a
screenPtr0      :=  $1b     ; Screen pointer
screenPtr1      :=  $1c
screen2Ptr0     :=  $1d
screen2Ptr1     :=  $1e
stringPtr0      :=  $fe
stringPtr1      :=  $ff
mapPtr0         :=  $ec
mapPtr1         :=  $ed

; Indexes
tileIdx         :=  $1d
tileX           :=  $e3
tileY           :=  $e8
tileX2 			:=  $fc
tileY2 			:=  $fd
mapWindowX      :=  $ef
mapWindowY      :=  $fa

; Controls
drawPage        :=  $d7
invMask         :=  $ee

; following are available to use
;               :=  $08
;               :=  $09
;               :=  $19
;               :=  $1a
;               :=  $ce
;               :=  $cf
;               :=  $ee
;               :=  $ef
;               :=  $fa
;               :=  $fb



; Memory map
;---------------------------------------------------------
FILEBUFFER      := $800     ; User PRODOS filebuffer, 512 bytes
HGRPAGE1        := $2000
HGRPAGE2        := $4000

; Soft switches
;---------------------------------------------------------
RAMRDOFF        := $C002    ; read main memory  $200 - $BFFF
RAMRDON         := $C003    ; read aux memory   $200 - $BFFF
RAMWRTOFF       := $C004    ; write main memory $200 - $BFFF
RAMWRTON        := $C005    ; write aux memory  $200 - $BFFF
CLR80VID        := $C00C
SET80VID        := $C00D
SPEAKER         := $C030
TEXTMODE        := $C01A    ; Bit 7 is 1 if text mode
ALTCHARSETOFF   := $C00E    ; Write to turn off alternate characters
ALTCHARSETON    := $C00F    ; Write to turn on alternate characters
PAGE2           := $C01C    ; Bit 7 set if displaying page 2
BUTTON0         := $C061    ; Bit 7 set if paddle button 0 is pressed
BUTTON1         := $C062    ; Bit 7 set if paddle button 1 is pressed
BUTTON2         := $C063    ; Bit 7 set if paddle button 2 is pressed

; ROM routines
;---------------------------------------------------------
AUXMOVE         := $C311    ; Aux memory copy
GR              := $F390    ; Low-res mixed graphics mode
TEXT            := $F399    ; Text-mode
HGR             := $F3E2    ; Turn on hi-res mode, page 1 mixed mode, clear    
HGR2            := $F3D8    ; Turn on hi-res mode, page 2, clear
PRBYTE          := $FDDA    ; Print A as a 2-digit hex
PRINTXY         := $F940    ; Print X(low) Y(high) as 4-digit hex
VTAB            := $FC22    ; Move the cursor to line CV
HOME            := $FC58    ; Clear text screen
CR              := $FC62    ; Output carriage return
RDKEY           := $FD0C    ; Read 1 char
GETLN           := $FD6A    ; Read a line of characters
COUT            := $FDED    ; Output a character
MON             := $FF65    ; Enter monitor (BRK)
MONZ            := $FF69    ; Enter monitor
WAIT            := $FCA8    ; Wait 0.5*(26 + 27*A + 5*A*A) microseconds
PREAD           := $FB1E    ; Read paddle X (0=hor,1=vert on joystick), result in Y

; PRODOS
;---------------------------------------------------------
MLI             := $BF00    ; PRODOS MLI call
CMD_QUIT        = $65
CMD_CREATE      = $C0
CMD_OPEN        = $C8
CMD_READ        = $CA
CMD_WRITE       = $CB
CMD_CLOSE       = $CC

; Keyboard
;---------------------------------------------------------
KEY_CTRL_A      = $81
KEY_CTRL_B      = $82
KEY_CTRL_C      = $83
KEY_CTRL_D      = $84
KEY_CTRL_E      = $85
KEY_CTRL_F      = $86
KEY_LEFT        = $88       ; CTRL_H
KEY_TAB         = $89       ; CTRL_I
KEY_DOWN        = $8A       ; CTRL_J
KEY_UP          = $8B       ; CTRL_K
KEY_CTRL_L      = $8C
KEY_RETURN      = $8D       ; CTRL_M
KEY_CTRL_O      = $8F
KEY_CTRL_P      = $90
KEY_CTRL_Q      = $91
KEY_CTRL_R      = $92
KEY_CTRL_S      = $93
KEY_CTRL_T      = $94
KEY_RIGHT       = $95       ; CTRL_U
KEY_CTRL_V      = $96
KEY_CTRL_W      = $97
KEY_CTRL_X      = $98
KEY_CTRL_Y      = $99
KEY_CTRL_Z      = $9A
KEY_ESC         = $9B
KEY_SPACE       = $A0
KEY_0           = $B0
KEY_9           = $B9
KEY_A           = $C1
KEY_B           = $C2
KEY_C           = $C3
KEY_D           = $C4
KEY_E           = $C5
KEY_F           = $C6
KEY_Z           = $DA
KEY_DEL         = $FF

; Constants
;---------------------------------------------------------
DIR_LEFT        =   0
DIR_RIGHT       =   1
DIR_UP          =   2
DIR_DOWN        =   3


; DHGR Engine
;---------------------------------------------------------

DHGR_TEST               := $C00
DHGR_INIT               := $C03
DHGR_DRAW_7X8           := $C06
DHGR_DRAW_28X8          := $C09
DHGR_DRAW_MASK_28X8     := $C0C
DHGR_DRAW_PIXEL_4X4     := $C0F
DHGR_SCROLL_LINE        := $C12

DHGR_TILE_7X8           := $C20
DHGR_TILE_28X8          := $C22

