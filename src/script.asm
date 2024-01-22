;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; Script
;-----------------------------------------------------------------------------

;---------------------------------
; Instruction Bytes
;---------------------------------
.define     INST_DONE           $00
.define     INST_GOTO           $01
.define     INST_BRANCH         $02

.define     INST_TRUE           $10
.define     INST_FALSE          $11
.define     INST_AND            $12
.define     INST_OR             $13
.define     INST_ADJACENT       $14
.define     INST_COMPARE_VALUE  $15

.define     INST_READ           $20
.define     INST_CLEAR          $21
.define     INST_SET            $22
.define     INST_SET_VALUE      $23
.define     INST_INC            $24

.define     INST_IMAGE          $30
.define     INST_DIALOG         $31
.define     INST_SET_TIMER      $32

;---------------------------------
; Helper macros
;---------------------------------
.macro  GS_OFFSET label
    ; FIXME: forward only
    .byte   label-*-1
.endmacro

;---------------------------------
; Script Instructions
;---------------------------------

; Return control
.macro GS_DONE
    .byte   INST_DONE
.endmacro

; Jump to script address
.macro  GS_GOTO label
    .byte INST_GOTO
    GS_OFFSET label
.endmacro

; Jump to script address if condition flag is true
.macro  GS_BRANCH label
    .byte INST_BRANCH
    GS_OFFSET label
.endmacro


; Set condition flag to true
.macro GS_TRUE
    .byte   INST_TRUE
.endmacro

; Set condition flag to false
.macro GS_FALSE
    .byte   INST_FALSE
.endmacro

; Do a logical AND of the condition flag and the next command result
.macro GS_AND
    .byte   INST_AND
.endmacro

; Do a logical OR of the condition flag and the next command result
.macro GS_OR
    .byte   INST_OR
.endmacro

; Set condition flag if player is adject to map coordinate
.macro GS_ADJACENT coordinate
    .byte   INST_ADJACENT, coordinate
.endmacro

; Set condition flag if state matches argument
.macro GS_COMPARE_VALUE state, value
    .byte   INST_COMPARE_VALUE, state, value
.endmacro

; Read condition flag from game state
.macro GS_READ state
    .byte   INST_READ, state
.endmacro

; Clear game state
.macro GS_CLEAR state
    .byte   INST_CLEAR, state
.endmacro

; Set game state to 1
.macro GS_SET state
    .byte   INST_SET, state
.endmacro

; Set game state to value
.macro GS_SET_VALUE state, value
    .byte   INST_SET_value, state, value
.endmacro

; Increment game state
.macro GS_INC state
    .byte   INST_INC, state
.endmacro


; Display passed image
.macro  GS_IMAGE address
    .byte   INST_IMAGE, <address, >address
.endmacro

; Display dialog
.macro  GS_DIALOG address
    .byte   INST_DIALOG, <address, >address
.endmacro

; Set countdown timer
.macro  GS_SET_TIMER value
    .byte   INST_SET_TIMER, value
.endmacro


