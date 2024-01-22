;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------
; Level data
;-----------------------------------------------------------------------------

.org    $6000

.include "script.asm"
.include "macros.asm"

;-----------------------------------------------------------------------------
; Constants
;-----------------------------------------------------------------------------

wizardLocation  =   $7A
wizardState     =   $FF         ; reserve state from back to front

;-----------------------------------------------------------------------------
; Event jump table -- fixed locations
;-----------------------------------------------------------------------------

enterMap:       GS_GOTO         init
timer:          GS_GOTO         alarm
playerAction:   GS_GOTO         action      ; Not used
playerMove:     GS_DONE                     ; Not used
                GS_DONE                     ; padding
playerBlocked:  GS_DONE                     ; Not used
                GS_DONE                     ; padding
scriptEvent0:   GS_DONE                     ; Not used
                GS_DONE                     ; padding
reserved0C:     GS_DONE                     ; Not used
                GS_DONE                     ; padding
reserved0E:     GS_DONE                     ; Not used
                GS_DONE                     ; padding

;-----------------------------------------------------------------------------
; Script
;-----------------------------------------------------------------------------

init:           GS_IMAGE        wizard1Even     ; display wizard
                GS_DIALOG       wizardText0     ; give initial dialog
                GS_CLEAR        wizardState     ; reset state
                GS_SET_TIMER    10              ; Set initial timer
                GS_DONE

;--------------------

action:         GS_ADJACENT     wizardLocation  ; check if next to wizard
                GS_BRANCH       :+
                GS_DONE
:               GS_READ         wizardState     ; Did we already talk to the wizard
                GS_BRANCH       :+
                GS_DIALOG       wizardText1     ; give next dialog
                GS_SET          wizardState
                GS_DONE
:               GS_DIALOG       wizardText2     ; give final dialog
                GS_DONE

;--------------------

alarm:          GS_READ         wizardState     ; Did we already talk to the wizard
                GS_BRANCH       :+
                GS_DIALOG       alarmText
                GS_SET_TIMER    10              ; repeat
 :              GS_DONE

;-----------------------------------------------------------------------------
; Dialog
;-----------------------------------------------------------------------------

    ;             <-- 20 columns ---->
wizardText0:
    .byte        13
    StringBold   "H"
    StringCont    "ello friend!"
    StringCont   "Come over here and"
    StringCont   "let us talk."
    .byte        "Use "
    StringBold        "WESD"
    StringCont             " to move,"
    .byte        "and "
    StringBold        "SPACEBAR"
    StringCont                 " to"
    StringCont   "talk or perform"
    StringCR     "other actions."

wizardText1:
    .byte        13
    ;             <-- 20 columns ---->
    StringBold   "Y"
    StringCont    "ou can call me"
    StringBold   "MERLIN"
    StringCont          "-8 as I come"
    StringCont   "from a long line of:"
    StringCR     "wizards."

wizardText2:
    .byte        13
    ;             <-- 20 columns ---->
    StringBold   "N"
    StringCont    "ow go explore the"
    StringCR     "forest!"

alarmText:
    .byte        13
    StringCR     "Over here!"

;-----------------------------------------------------------------------------
; Images
;-----------------------------------------------------------------------------
.align  256
.include "images0.asm"


