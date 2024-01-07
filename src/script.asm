;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; Script
;-----------------------------------------------------------------------------

; Loading a room:
;   - Loading any assest needed
; 		- Map of the room
;   	- Script for the room
;   	- Dialog for the room
; 		- Tiles
; 		- Pictures
;
; Bespoke byte code script for game
;
; Each map has its own script.
; Could also have a global script, but may not be needed.
;
; A script is invoked by events such as:
;   - entering a room
; 	- player interacting with object / NPC
;   - stepping on a special spot
; All of these can be generalized to just be a location with a qualifier
; of active or passive.
;
; The script is a sequence of actions.
;
; Actions:
;   - Enter a room (room # and location)
;   - Display a picture (picture #)
;   - Display a dialog (dialog #)