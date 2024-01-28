;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------
; Map edit
;------------------------------------------------

;------------------------------------------------
.include "defines.asm"
.include "macros.asm"
.include "script.asm"

;-----------------------------------------------------------------------------
; Constants
;-----------------------------------------------------------------------------

; map-width     map-height  map-length <= 256
; ---------     ----------  ----------
; 8*            32          256
; 10            24          240
; 12            20          240
; 14            18          252
; 16*           16          256
; 18            14          252
; 20            12          240
; * = power of 2

MAP_WIDTH       = 13
MAP_HEIGHT      = 19

MAP_OFFSET_X    = 12
MAP_OFFSET_Y    = 2
MAP_RIGHT_EDGE  = MAP_OFFSET_X + MAP_WIDTH*2
MAP_BOTTOM_EDGE = MAP_OFFSET_Y + MAP_HEIGHT

BOX_HORZ        = $13
BOX_VERT        = $7c
BOX_UPPER_LEFT  = $1c
BOX_UPPER_RIGHT = $1d
BOX_LOWER_LEFT  = $1e
BOX_LOWER_RIGHT = $1f
BOX_LEFT_T      = $0f
BOX_RIGHT_T     = $11

DIALOG_LEFT     = 2
DIALOG_RIGHT    = 20
DIALOG_TOP      = 11
DIALOG_BOTTOM   = 20

KEY_NW          = 'W'|$80
KEY_NE          = 'E'|$80
KEY_SW          = 'S'|$80
KEY_SE          = 'D'|$80

MOVE_NW         = 256-MAP_WIDTH-1
MOVE_NE         = 256-MAP_WIDTH+1
MOVE_SW         = MAP_WIDTH-1
MOVE_SE         = MAP_WIDTH+1

LEVEL_DATA      := $6000    ; AUX memory
IMAGE_TABLE     := $6100    ; AUX memory

;                       ; Vector
eventEnter      = $01   ; $00  - Entered a map
eventTimer      = $02   ; $02  - Game timer event
eventAction     = $04   ; $04  - Player tried to do an action
eventMoved      = $08   ; $06  - Player moved to a new location
eventBlocked    = $10   ; $08  - Player tried to move but was blocked
;                       ; $0A  - Reserved
;                       ; $0C  - Reserved
;                       ; $0E  - Reserved

;------------------------------------------------
.segment "CODE"
.org    $6000

;=============================================================================
; Main
;=============================================================================

.proc main

    ;------------------------
    ; Setup
    ;------------------------

    jsr     initMonochrome
    sta     CLR80COL

    jsr     initScript

    ; Display Title
    sta     HISCR

    ; Set starting position before drawing map
    ldx     #$32
    jsr     updatePlayerMap

    ; Clear screen 0 and draw map
    lda     #$00
    jsr     drawGameScreen

;    ; wait for key (ignore key)
;:
;    lda     KBD
;    bpl     :-
;    bit     KBDSTRB     ; clean up

    ; display 0
    sta     LOWSCR

    ; Clear screen 1 and draw map
    lda     #$20
    jsr     drawGameScreen

    ; Set init event (clearing others)
    lda     #eventEnter
    sta     scriptEvents

    ; Cycle through map for animation
    lda     #255
    sta     mapCursor

    lda     #0
    sta     update

    ;------------------------
    ; Game loop
    ;------------------------

displayLoop:

    lda     drawPage
    beq     display1

    ; we just finished drawing on page 2, so display it and now drawing on page 1
    sta     HISCR
    lda     #$00
    beq     :+

    ; we just finished drawing on page 1, so display it and now drawing on page 2
display1:
    sta     LOWSCR
    lda     #$20
:
    sta     drawPage

    ; if updated last time, need to draw again on this page
    lda     update
    beq     :+
    lda     mapCursor
    jsr     setCoordinate
    jsr     drawQuarter
:

    lda     playerUpdate
    beq     :+
    lda     oldPlayerIdx
    jsr     drawPlayer
    lda     playerIdx
    jsr     drawPlayer
:

    lda     #0
    sta     update
    sta     playerUpdate

    ; check next tile
    inc     mapCursor
    bne     doneTime

    lda     scriptTimer
    beq     :+
    dec     scriptTimer
    bne     :+
    lda     scriptEvents
    ora     #eventTimer
    sta     scriptEvents
:
    inc     gameTime0
    bne     doneTime
    inc     gameTime1
doneTime:

    ;------------------------
    ; Animate Tiles
    ;------------------------
    ldx     mapCursor

    ; Map 2
    lda     isoMap2,x
    tay
    lda     animateMap,y
    beq     :+
    sta     isoMap2,x
    inc     update
:
    ; Map 1
    lda     isoMap1,x
    tay
    lda     animateMap,y
    beq     :+
    sta     isoMap1,x
    inc     update
:
    ; Map 0
    lda     isoMap0,x
    tay
    lda     animateMap,y
    beq     :+
    sta     isoMap0,x
    inc     update
:

    lda     update
    beq     :+
    lda     mapCursor
    jsr     setCoordinate
    jsr     drawQuarter
:

    ;------------------------
    ; Debug
    ;------------------------
    lda     #0
    sta     tileX
    lda     #23
    sta     tileY
    lda     gameTime1
    jsr     drawNumber
    lda     gameTime0
    jsr     drawNumber
    lda     mapCursor
    jsr     drawNumber

    inc     tileX
    lda     scriptTimer
    jsr     drawNumber

    lda     #40
    sta     tileX
    lda     scriptPtr1
    jsr     drawNumber
    lda     scriptPtr0
    jsr     drawNumber

    lda     #72
    sta     tileX
    lda     oldPlayerIdx
    jsr     drawNumber

    lda     #75
    sta     tileX
    lda     playerIdx
    jsr     drawNumber

    lda     #78
    sta     tileX
    lda     newPlayerIdx
    jsr     drawNumber

    ;------------------------
    ; Display Dialog
    ;------------------------
    lda     scriptDialogActive
    beq     checkScript

    jsr     processDialog
    cmp     #0
    bne     :+
    sta     scriptDialogActive
:
    jmp     displayLoop

    ;------------------------
    ; Execute Script
    ;------------------------
checkScript:
    lda     scriptActive        ; Are we still processing script?
    bne     :+
    jmp     checkEvents
:
    jsr     DHGR_READ_SCRIPT_BYTE

checkDone:
    cmp     #INST_DONE
    bne     checkGoto
    jsr     initScript          ; reset script pointer
    jmp     displayLoop

checkGoto:
    cmp     #INST_GOTO
    bne     checkBranch
branchTrue:
    jsr     DHGR_READ_SCRIPT_BYTE   ; read offset
    ; FIXME: handle negative
    clc
    adc     scriptPtr0
    sta     scriptPtr0          ; Right now, only forward!
    bcc     :+
    inc     scriptPtr1
:
    jmp     displayLoop

checkBranch:
    cmp     #INST_BRANCH
    bne     checkImage
    lda     scriptCondition
    bne     branchTrue
    jsr     DHGR_READ_SCRIPT_BYTE   ; discard offset
    jmp     displayLoop


checkImage:
    cmp     #INST_IMAGE
    bne     checkDialog
    jsr     DHGR_READ_SCRIPT_BYTE
    sta     scriptArg0
    jsr     DHGR_READ_SCRIPT_BYTE
    sta     scriptArg1
    ; Since drawing an image is slow, go it for both pages
    lda     drawPage
    sta     temp
    lda     #$00
    jsr     scriptDrawImage
    lda     #$20
    jsr     scriptDrawImage
    lda     temp
    sta     drawPage
    jmp     displayLoop

checkDialog:
    cmp     #INST_DIALOG
    bne     checkClear
    jsr     DHGR_READ_SCRIPT_BYTE
    sta     stringPtr0
    jsr     DHGR_READ_SCRIPT_BYTE
    sta     stringPtr1
    inc     scriptDialogActive
    lda     #0
    sta     dialogCount
    ; Use alternate font
    lda     #>$B400
    sta     DHGR_TILE_7X8+1
    jmp     displayLoop

checkClear:
    cmp     #INST_CLEAR
    bne     checkSet
    jsr     DHGR_READ_SCRIPT_BYTE   ; state
    tax
    lda     #0
    sta     gameState,x
    jmp     displayLoop

checkSet:
    cmp     #INST_SET
    bne     checkRead
    jsr     DHGR_READ_SCRIPT_BYTE   ; state
    tax
    lda     #1
    sta     gameState,x
    jmp     displayLoop

checkRead:
    cmp     #INST_READ
    bne     checkAdjacent
    jsr     DHGR_READ_SCRIPT_BYTE   ; state
    tax
    lda     gameState,x
    sta     scriptCondition
    jmp     displayLoop

checkAdjacent:
    cmp     #INST_ADJACENT
    bne     checkSetTimer
    jsr     DHGR_READ_SCRIPT_BYTE   ; location
    sta     temp
    clc
    adc     #MOVE_NE
    cmp     playerIdx
    beq     adjacentMatch
    lda     temp
    clc
    adc     #MOVE_NW
    cmp     playerIdx
    beq     adjacentMatch
    lda     temp
    clc
    adc     #MOVE_SE
    cmp     playerIdx
    beq     adjacentMatch
    lda     temp
    clc
    adc     #MOVE_SW
    cmp     playerIdx
    beq     adjacentMatch
    lda     #0
    sta     scriptCondition
    jmp     displayLoop
adjacentMatch:
    lda     #1
    sta     scriptCondition
    jmp     displayLoop


checkSetTimer:
    cmp     #INST_SET_TIMER
    bne     checkNext
    jsr     DHGR_READ_SCRIPT_BYTE   ; timer
    sta     scriptTimer
    jmp     displayLoop

checkNext:
    brk

checkEvents:
    lda     scriptEvents
    beq     checkKeyboard

    lsr
    bcc     :+
    ldx     #0
    lda     #%11111110
    jmp     setEvent
:
    lsr
    bcc     :+
    ldx     #2
    lda     #%11111100
    jmp     setEvent
:
    lsr
    bcc     :+
    ldx     #4
    lda     #%11111000
    jmp     setEvent
:
    lsr
    bcc     :+
    ldx     #6
    lda     #%11110000
    jmp     setEvent
:
    lsr
    bcc     :+
    ldx     #8
    lda     #%11100000
    jmp     setEvent
:
    brk     ; unexpected event

setEvent:
    and     scriptEvents
    sta     scriptEvents
    stx     scriptPtr0
    inc     scriptActive    ; must have been 0, so set to 1
    jmp     displayLoop

checkKeyboard:
    ;------------------------
    ; Poll keyboard
    ;------------------------
    ;   check for keypress
    lda     KBD
    bmi     :+
    jmp     noKeyPress
:
    sta     KBDSTRB

    ;-----------------------
    cmp     #KEY_ESC
    bne     :+
    ; Exit
    ; Make sure to display and draw on page 1 on exit
    lda     #$00
    sta     drawPage
    sta     LOWSCR
    sta     MIXSET  ; mixed screen
    jmp     DHGR_LOADER_MENU
:
    ;-----------------------
    cmp     #KEY_NW
    bne     :+
    lda     #MOVE_NW
    jmp     move
:
    ;-----------------------
    cmp     #KEY_NE
    bne     :+
    lda     #MOVE_NE
    jmp     move
:
    ;-----------------------
    cmp     #KEY_SW
    bne     :+
    lda     #MOVE_SW
    jmp     move
:
    ;-----------------------
    cmp     #KEY_SE
    bne     :+
    lda     #MOVE_SE
    jmp     move
:
    ;-----------------------
    cmp     #KEY_SPACE
    bne     :+

    lda     #eventAction
    sta     scriptEvents    ; assume no other events
    jmp     displayLoop

:

noKeyPress:
    jmp     displayLoop

move:
    clc
    adc     playerIdx
    sta     newPlayerIdx
    tax
    lda     isoMapLevel,x
    bpl     noMove

    lda     playerIdx
    sta     oldPlayerIdx
    jsr     erasePlayer
    ldx     newPlayerIdx
    jsr     updatePlayerMap

    lda     oldPlayerIdx
    jsr     drawPlayer
    lda     playerIdx
    jsr     drawPlayer
    inc     playerUpdate

    lda     #eventMoved
    sta     scriptEvents    ; assume no other events
    sta     SPEAKER
    jmp     displayLoop

noMove:
    lda     #eventBlocked
    sta     scriptEvents    ; assume no other events
    jmp     displayLoop

playerUpdate:   .byte   0
update:         .byte   0
storeWidth:     .byte   0
storeOffsetX:   .byte   0
temp:           .byte   0
.endproc

;------------------------------------------------
; Init Script
;------------------------------------------------
.proc initScript
    lda     #<LEVEL_DATA
    sta     scriptPtr0
    lda     #>LEVEL_DATA
    sta     scriptPtr1
    lda     #0
    sta     scriptActive
    rts
.endproc

;------------------------------------------------
; Script Draw Image
;------------------------------------------------
.proc scriptDrawImage
    sta     drawPage
    lda     scriptArg0
    sta     tilePtr0
    clc
    adc     #$80
    sta     maskPtr0
    lda     scriptArg1
    sta     tilePtr1
    adc     #$2
    sta     maskPtr1
    jsr     DHGR_DRAW_IMAGE_AUX
    rts
.endproc

;------------------------------------------------
; Draw Game Screen
;
;   Clear screen and draw all elements
;------------------------------------------------
.proc drawGameScreen
    sta     drawPage

    ldx     #0
    jsr     setBackground
    jsr     DHGR_CLEAR_SCREEN
    jsr     drawFrame

    jsr     drawTitles

    ldx     bgColor
    jsr     setBackground

    lda     #MAP_OFFSET_X
    sta     tileX
    lda     #MAP_OFFSET_Y
    sta     tileY
    lda     #MAP_RIGHT_EDGE
    sta     tileX2
    lda     #MAP_BOTTOM_EDGE
    sta     tileY2
    lda     #0              ; starting index
    jsr     isoDrawMap

    rts

.endproc

;------------------------------------------------
; Draw Frame
;------------------------------------------------
.proc drawFrame

    lda     #<$B000
    sta     DHGR_TILE_7X8
    lda     #>$B000
    sta     DHGR_TILE_7X8+1

    lda     #1
    sta     boxLeft
    lda     #1
    sta     boxTop
    lda     #22
    sta     boxRight
    lda     #10
    sta     boxBottom
    clc
    jsr     drawBox

    lda     #1
    sta     boxLeft
    lda     #10
    sta     boxTop
    lda     #22
    sta     boxRight
    lda     #21
    sta     boxBottom
    sec
    jsr     drawBox

    lda     #23
    sta     boxLeft
    lda     #1
    sta     boxTop
    lda     #76
    sta     boxRight
    lda     #21
    sta     boxBottom
    clc
    jsr     drawBox
    rts

.endproc

;------------------------------------------------
; Draw Titles
;------------------------------------------------
.proc drawTitles
    lda     #2
    sta     tileX
    lda     #0
    sta     tileY
    jsr     DHGR_DRAW_STRING_INLINE

    .byte   $93,$93,$93,$93,$93," Merlin-8 ",$93,$93,$93,$93,$93,0

    lda     #24
    sta     tileX
    lda     #0
    sta     tileY
    jsr     DHGR_DRAW_STRING_INLINE
    .byte   $93,$93,$93,$93,$93,$93,$93,$93,$93,$93,$93,$93,$93
    .byte   " Somewhere in the forest "
    .byte   $93,$93,$93,$93,$93,$93,$93,$93,$93,$93,$93,$93,$93,$93
    .byte   0
    rts
.endproc

;------------------------------------------------
; processDialog
;------------------------------------------------
.proc processDialog

    sta     SPEAKER         ; noisy

    ; set drawing coordinate
    lda     dialogX
    sta     tileX
    lda     dialogY
    sta     tileY

    lda     dialogCount     ; need to process everything twice
    and     #1
    bne     :+
    jsr     DHGR_READ_STRING_BYTE
    sta     lastByte
:
    inc     dialogCount
    lda     lastByte
    bne     :+
    lda     dialogCount
    and     #1
    rts                     ; return with zero value == done
:
    cmp     #13             ; return
    beq     carriageReturn

    and     #$7f
    jsr     DHGR_DRAW_7X8

    lda     dialogCount     ; only advance the second time
    and     #1
    bne     :+
    inc     dialogX
:
    lda     #1              ; not done
    rts

carriageReturn:

    ; scroll if on the last line
    lda     dialogY
    cmp     #DIALOG_BOTTOM
    bne     :+

    lda     #DIALOG_LEFT/2
    sta     tileX
    lda     #DIALOG_RIGHT/2
    sta     tileX2
    lda     #DIALOG_TOP
    sta     tileY
    lda     #DIALOG_BOTTOM
    sta     tileY2
    jsr     DHGR_SCROLL_LINE
:

    lda     dialogCount     ; only advance the second time
    and     #1
    bne     :+
    inc     dialogY
    lda     #DIALOG_LEFT
    sta     dialogX
    lda     dialogY
    cmp     #DIALOG_BOTTOM+1
    bne     :+
    dec     dialogY
:
    lda     #1
    rts

lastByte:   .byte   0

.endproc

;-----------------------------------------------------------------------------
; Init
;
;-----------------------------------------------------------------------------

.proc initMonochrome
    ; // GS B&W
    lda     #$21
    sta     NEWVIDEO    ; B&W mode
    lda     CLOCKCTL    ; RMW to set border color
    and     #$f0
    ora     #$05        ; Gray
    sta     CLOCKCTL

    ; // E B&W
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
    sta     MIXCLR      ; Mixed
    rts
.endproc

;-----------------------------------------------------------------------------
; Draw box
;
;-----------------------------------------------------------------------------

.proc drawBox

    bcc     topCorners

    ;   draw Ts for tops

    ; Draw corners
    lda     boxLeft
    sta     tileX
    lda     boxTop
    sta     tileY
    lda     #BOX_LEFT_T
    jsr     DHGR_DRAW_7X8

    lda     boxRight
    sta     tileX
    lda     #BOX_RIGHT_T
    jsr     DHGR_DRAW_7X8
    jmp     cont

topCorners:
    ; Draw corners
    lda     boxLeft
    sta     tileX
    lda     boxTop
    sta     tileY
    lda     #BOX_UPPER_LEFT
    jsr     DHGR_DRAW_7X8

    lda     boxRight
    sta     tileX
    lda     #BOX_UPPER_RIGHT
    jsr     DHGR_DRAW_7X8

cont:
    lda     boxBottom
    sta     tileY
    lda     #BOX_LOWER_RIGHT
    jsr     DHGR_DRAW_7X8

    lda     boxLeft
    sta     tileX
    lda     #BOX_LOWER_LEFT
    jsr     DHGR_DRAW_7X8

    ; Draw horizontal

    inc     tileX
:
    lda     boxTop
    sta     tileY
    lda     #BOX_HORZ
    jsr     DHGR_DRAW_7X8

    lda     boxBottom
    sta     tileY
    lda     #BOX_HORZ
    jsr     DHGR_DRAW_7X8

    inc     tileX
    lda     boxRight
    cmp     tileX
    bne     :-

    ; Draw vertical

    lda     boxTop
    sta     tileY
    inc     tileY

:
    lda     boxLeft
    sta     tileX
    lda     #BOX_VERT
    jsr     DHGR_DRAW_7X8

    lda     boxRight
    sta     tileX
    lda     #BOX_VERT
    jsr     DHGR_DRAW_7X8

    inc     tileY
    lda     boxBottom
    cmp     tileY
    bne     :-

    rts

.endproc

;------------------------------------------------
; Update Player Map
;
;   Clear player map and withdraw from screen
;------------------------------------------------
.proc updatePlayerMap
    stx     playerIdx
    ; fixed offsets
    lda     playerTiles+0
    sta     isoMapP-MAP_WIDTH*2,x
    lda     playerTiles+1
    sta     isoMapP-MAP_WIDTH*2+1,x
    lda     playerTiles+2
    sta     isoMapP-MAP_WIDTH,x
    lda     playerTiles+3
    sta     isoMapP-MAP_WIDTH+1,x
    lda     playerTiles+4
    sta     isoMapP,x
    lda     playerTiles+5
    sta     isoMapP+1,x
    lda     playerTiles+6
    sta     isoMapP+MAP_WIDTH,x
    lda     playerTiles+7
    sta     isoMapP+MAP_WIDTH+1,x

    ; update player line
    lda     playerIdx
    jsr     setCoordinate
    ldy     tileY
    lda     #$20
    sta     playerLine-2,y
    lda     #$40
    sta     playerLine-1,y
    lda     #$60
    sta     playerLine,y
    lda     #$80
    sta     playerLine+1,y

    rts
.endproc

;------------------------------------------------
; Erase Player
;
;   Clear player map and withdraw from screen
;------------------------------------------------
.proc erasePlayer

    ; First clear map
    ldx     oldPlayerIdx
    lda     #0
    sta     isoMapP-MAP_WIDTH*2,x
    sta     isoMapP-MAP_WIDTH*2+1,x
    sta     isoMapP-MAP_WIDTH,x
    sta     isoMapP-MAP_WIDTH+1,x
    sta     isoMapP,x
    sta     isoMapP+1,x
    sta     isoMapP+MAP_WIDTH,x
    sta     isoMapP+MAP_WIDTH+1,x

    ; clear player line
    ldx     #23
    lda     #0
:
    sta     playerLine,x
    dex
    bpl     :-

    rts
.endproc

;------------------------------------------------
; Draw Player
;
;   Draw map of player size at index passed in A
;------------------------------------------------
.proc drawPlayer
    ; copy map cursor
    ldx     mapCursor
    stx     mapCursorCopy

    sta     mapCursor
    jsr     setCoordinate
    jsr     drawQuarter
    inc     mapCursor
    inc     tileX
    inc     tileX
    jsr     drawQuarter

    lda     mapCursor
    clc
    adc     #MAP_WIDTH-1
    sta     mapCursor
    jsr     setCoordinate
    jsr     drawQuarter
    inc     mapCursor
    inc     tileX
    inc     tileX
    jsr     drawQuarter

    lda     mapCursor
    sec
    sbc     #MAP_WIDTH*3+1
    sta     mapCursor
    jsr     setCoordinate
    jsr     drawQuarter
    inc     mapCursor
    inc     tileX
    inc     tileX
    jsr     drawQuarter

    lda     mapCursor
    clc
    adc     #MAP_WIDTH-1
    sta     mapCursor
    jsr     setCoordinate
    jsr     drawQuarter
    inc     mapCursor
    inc     tileX
    inc     tileX
    jsr     drawQuarter

    lda     mapCursorCopy
    sta     mapCursor
    rts

temp:           .byte   0
mapCursorCopy:  .byte   0
.endproc

;------------------------------------------------
; Draw Quarter
;------------------------------------------------
.proc drawQuarter
    ldx     mapCursor
    lda     isoMapLevel,x
    and     #$7f            ; ignore bit 7 (movement flag)
    clc
    ldy     tileY
    adc     playerLine,y
    tay
    lda     jumpTable,y
    sta     *+4
    jmp     draw012         ; link return
.endproc

;-----------------------------------------------------------------------------
; isoDrawMap
;
;   Draw ISO map
;-----------------------------------------------------------------------------

.proc isoDrawMap

    sta     isoIdxY
    lda     tileX
    sta     tileXCopy

loopY:
    tay
    lda     playerLine,y
    sta     playerOffset

    lda     tileXCopy
    sta     tileX

    lda     isoIdxY
    tax

loopX1:
    lda     isoMapLevel,x
    and     #$7f            ; ignore bit 7 (movement flag)
    clc
    adc     playerOffset
    tay
    lda     jumpTable,y

    sta     *+4
    jsr     draw012

    inx

    clc
    lda     tileX
    adc     #2
    sta     tileX
    cmp     tileX2
    bmi     loopX1

    clc
    lda     isoIdxY
    adc     #MAP_WIDTH
    sta     isoIdxY

    inc     tileY
    lda     tileY
    cmp     tileY2
    bmi     loopY

    rts

tileXCopy:      .byte   0
isoIdxY:        .byte   0
playerOffset:   .byte   0

.endproc

.align 256

; should be draw012
.proc draw012
    lda     isoMap0,x
    jsr     DHGR_DRAW_BG_28X8
    lda     isoMap1,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMap2,x
    jsr     DHGR_DRAW_MASK_28X8
    rts
.endproc

.proc draw012P
    lda     isoMap0,x
    jsr     DHGR_DRAW_BG_28X8
    lda     isoMap1,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMap2,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMapP,x
    jsr     DHGR_DRAW_MASK_28X8
    rts
.endproc

.proc draw01P2
    lda     isoMap0,x
    jsr     DHGR_DRAW_BG_28X8
    lda     isoMap1,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMapP,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMap2,x
    jsr     DHGR_DRAW_MASK_28X8
    rts
.endproc

.proc draw0P12
    lda     isoMap0,x
    jsr     DHGR_DRAW_BG_28X8
    lda     isoMapP,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMap1,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMap2,x
    jsr     DHGR_DRAW_MASK_28X8
    rts
.endproc

.proc drawP012
    lda     isoMapP,x
    jsr     DHGR_DRAW_BG_28X8
    lda     isoMap0,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMap1,x
    jsr     DHGR_DRAW_MASK_28X8
    lda     isoMap2,x
    jsr     DHGR_DRAW_MASK_28X8
    rts
.endproc

.proc drawBad
    brk     ; Should not get here!
.endproc

;-----------------------------------------------------------------------------
; Tables
;-----------------------------------------------------------------------------

jumpTable:

    ; including "bad" to catch map/map conflicts (more than 3 levels)
    ; should probably replace with 012P for release to avoid crashes with just a glitch

    ; no player - $00
    .byte   <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012
    .byte   <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012
    .byte   <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012
    .byte   <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012, <draw012

    ; player7   - $20
    .byte   <draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P
    .byte   <draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<drawBad
    .byte   <draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<drawBad
    .byte   <draw012P,<draw012P,<draw012P,<drawBad, <draw012P,<drawBad, <drawBad, <drawBad

    ; player6   - $40
    .byte   <draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P
    .byte   <draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<drawBad
    .byte   <draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<drawBad
    .byte   <draw01P2,<draw01P2,<draw01P2,<drawBad, <draw01P2,<drawBad, <drawBad, <drawBad

    ; player5   - $60
    .byte   <draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P,<draw012P
    .byte   <draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<drawBad
    .byte   <draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<drawBad
    .byte   <draw0P12,<draw0P12,<draw0P12,<drawBad, <draw0P12,<drawBad, <drawBad, <drawBad

    ; player3   - $80
    .byte   <draw012P,<draw012P,<draw01P2,<draw01P2,<draw01P2,<draw01P2,<draw0P12,<draw0P12
    .byte   <draw01P2,<draw01P2,<draw0P12,<draw0P12,<draw0P12,<draw0P12,<drawP012,<drawBad
    .byte   <draw01P2,<draw01P2,<draw0P12,<draw0P12,<draw0P12,<draw0P12,<drawP012,<drawBad
    .byte   <draw0P12,<draw0P12,<drawP012,<drawBad, <drawP012,<drawBad, <drawBad, <drawBad

;-----------------------------------------------------------------------------
; setCoordinate
;
;   Index passed in A
;-----------------------------------------------------------------------------
.proc setCoordinate
    ; cursor = yyyyxxxx

    ldy     #MAP_OFFSET_Y
    sty     tileY

divideLoop:
    sec
    sbc     #MAP_WIDTH
    bcc     :+
    inc     tileY
    bne     divideLoop
:
    clc
    adc     #MAP_WIDTH
    asl     ;remainder*2
    adc     #MAP_OFFSET_X
    sta     tileX

    rts

.endproc

;-----------------------------------------------------------------------------
; set background
;   Pattern index passed in X
;-----------------------------------------------------------------------------

.proc setBackground
    lda     pattern,x
    sta     bgPattern00
    lda     pattern+1,x
    sta     bgPattern01
    lda     pattern+2,x
    sta     bgPattern10
    lda     pattern+3,x
    sta     bgPattern11
    rts

pattern:
    .byte   $00,$00,$00,$00     ; 00 - black
    .byte   $ff,$ff,$ff,$ff     ; 04 - white
    .byte   $55,$2a,$2a,$55     ; 08 - gray1
    .byte   $2a,$55,$55,$2a     ; 0C - gray2
    .byte   $7f,$7f,$00,$00     ; 10 - horizontal
    .byte   $63,$63,$63,$63     ; 14 - vertical
    .byte   $54,$15,$6a,$2b     ; 18 - pattern
    .byte   $2a,$57,$57,$2a     ; 1C - border
    .byte   $7f,$00,$00,$7f     ; 20 - checker

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
; Global
;-----------------------------------------------------------------------------
mapCursor:  .byte   0   ; Offset into map table

boxLeft:        .byte   0
boxRight:       .byte   0
boxTop:         .byte   0
boxBottom:      .byte   0

playerTiles:
    .byte   $58,$59
    .byte   $5a,$5b
    .byte   $5c,$5d
    .byte   $5e,$5f

; Lookup table for animation sequence
; Water:    $14 <-> $64
;           $15 <-> $65
;           $16 <-> $66
;           $17 <-> $67
;           $18 <-> $68
;           $19 <-> $69
;           $1a <-> $6a
;           $1b <-> $6b

animateMap:
    ;        x0  x1  x2  x3  x4  x5  x6  x7  x8  x9  xA  xB  xC  xD  xE  xF
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 0x
    .byte   $00,$00,$00,$00,$64,$65,$66,$67,$68,$69,$6a,$6b,$00,$00,$00,$00     ; 1x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 2x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 3x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 4x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 5x
    .byte   $00,$00,$00,$00,$14,$15,$16,$17,$18,$19,$1a,$1b,$00,$00,$00,$00     ; 6x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 7x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 8x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; 9x
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; Ax
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; Bx
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; Cx
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; Dx
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; Ex
    .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     ; Fx

; player
isoMapP:        .res    256
playerLine:     .res    24      ; optmizied to use a global Y [0..23]

scriptActive:       .byte   0
scriptCondition:    .byte   0
scriptEvents:       .byte   0       ; 8 bit vector
scriptArg0:         .byte   0
scriptArg1:         .byte   0
scriptDialogActive: .byte   0
dialogX:            .byte   DIALOG_LEFT
dialogY:            .byte   DIALOG_TOP
dialogCount:        .byte   0       ; Used to display twice (lo/hi page)

.align 256
; Game State
gameState:
mapNumber:      .byte   $0      ; Current map
playerIdx:      .byte   $7C     ; Player position
newPlayerIdx:   .byte   $0      ; Failed move position
oldPlayerIdx:   .byte   $0      ; Previous player position
gameTime0:      .byte   $0      ; incremented every 256 frames
gameTime1:      .byte   $0      ; incremented every 256*256 frames
bgColor:        .byte   $0C     ; Background pattern
playerLocked:   .byte   $0      ; Prevent player input
scriptTimer:    .byte   $0      ; count down timer

gameStateEnd:
                .res    256-(gameStateEnd-gameState)


.align 256
.include "map00.asm"