;-----------------------------------------------------------------------------
; Paul Wasson - 2024
;-----------------------------------------------------------------------------
; Level data
;-----------------------------------------------------------------------------

.org    $6000

.include "script.asm"
.include "macros.asm"

wizardLocation  =   $2C
wizardState     =   $FF         ; reserve state backwards

; Event jump table

enterMap:       GS_GOTO     init
timer:          GS_DONE                     ; Not used
playerAction:   GS_DONE                     ; Not used
playerMove:     GS_DONE                     ; Not used
playerBlocked:  GS_DONE                     ; Not used
scriptEvent0:   GS_DONE                     ; Not used
reserved0C:     GS_DONE                     ; Not used
reserved0E:     GS_DONE                     ; Not used

init:           GS_IMAGE    wizard1Even
                GS_DIALOG   wizardText0
                GS_DONE

action:         GS_ADJACENT wizardLocation
                GS_BRANCH   :+
                GS_DONE
:               GS_READ     wizardState     ; Did we already talk to the wizard
                GS_BRANCH   :+
                GS_DIALOG   wizardText1
                GS_SET      wizardState
                GS_DONE
:               GS_DIALOG   wizardText2
                GS_DONE

    ;             <-- 20 columns ---->
wizardText0:
    StringBold   "H"
    StringCont    "ello friend!"
    StringCont   "Come over here and"
    StringCont   "let us talk."
    .byte        "Use "
    StringBold        "WESD"
    StringCont             " to move,"
    StringBold   "SPACEBAR"
    StringCont            " to talk"
    StringCont   "or perform other"
    String       "actions."

wizardText1:
    ;             <-- 20 columns ---->
    StringBold   "Y"
    StringCont    "ou can call me"
    StringBold   "MERLIN"
    StringCont          "-8 as I come"
    StringCont   "from a long line of"
    String       "wizards."

wizardText2:
    ;             <-- 20 columns ---->
    StringBold   "N"
    StringCont    "ow go explore the"
    String       "forest!"

.include "images0.asm"


