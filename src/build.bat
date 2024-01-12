::---------------------------------------------------------------------------
:: Compile code
::   Assemble twice: 1 to generate listing, 2 to generate object
::---------------------------------------------------------------------------
cd ..\build

:: Engine
ca65 -I ..\src -t apple2 ..\src\engine.asm -l engine.dis || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\engine.asm apple2.lib  -o engine.apple2 -C ..\src\startC00.cfg || exit

:: Game
ca65 -I ..\src -t apple2 ..\src\game.asm -l game.dis || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\game.asm apple2.lib  -o game.apple2 -C ..\src\start6000.cfg || exit

::---------------------------------------------------------------------------
:: Compile tools
::---------------------------------------------------------------------------

:: Font Editor
ca65 -I ..\src -t apple2 ..\src\fontedit.asm -l fontedit.dis || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\fontedit.asm apple2.lib  -o fontedit.apple2 -C ..\src\start6000.cfg || exit

:: Map Editor
ca65 -I ..\src -t apple2 ..\src\mapedit.asm -l mapedit.dis || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\mapedit.asm apple2.lib  -o mapedit.apple2 -C ..\src\start6000.cfg || exit

:: Tile Editor
ca65 -I ..\src -t apple2 ..\src\tileedit.asm -l tileedit.dis || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileedit.asm apple2.lib  -o tileedit.apple2 -C ..\src\start6000.cfg || exit

:: Display Image
ca65 -I ..\src -t apple2 ..\src\displayImage.asm -l displayImage.dis || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\displayImage.asm apple2.lib  -o display.apple2 -C ..\src\start6000.cfg || exit

::---------------------------------------------------------------------------
:: Compile assets
::---------------------------------------------------------------------------

cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tilesheet_0.asm apple2.lib  -o tilesheet_0.apple2 -C ..\src\start6000.cfg || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\font7x8_0.asm apple2.lib  -o font7x8_0.apple2 -C ..\src\start6000.cfg || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\font7x8_1.asm apple2.lib  -o font7x8_1.apple2 -C ..\src\start6000.cfg || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\imagesheet_0.asm apple2.lib  -o imagesheet_0.apple2 -C ..\src\start6000.cfg || exit

::---------------------------------------------------------------------------
:: Build disk
::---------------------------------------------------------------------------

:: Start with a blank prodos disk
copy ..\disk\template_prodos.dsk mono_prodos.dsk  || exit

:: Engine
java -jar C:\jar\AppleCommander.jar -p  mono_prodos.dsk engine.system sys < C:\cc65\target\apple2\util\loader.system || exit
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk engine bin < engine.apple2  || exit

:: Game
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/game bin < game.apple2  || exit

:: Font Editor (tool 0)
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/tool.0 bin < fontedit.apple2  || exit

:: Tile Editor (tool 1)
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/tool.1 bin < tileedit.apple2  || exit

:: Map Editor (tool 2)
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/tool.2 bin < mapedit.apple2  || exit

:: Display Image (tool 3)
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/tool.3 bin < display.apple2  || exit

:: Throw on basic
java -jar C:\jar\AppleCommander.jar -p mono_prodos.dsk basic.system sys < ..\disk\BASIC.SYSTEM  || exit

:: Assets
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/tilesheet.0 bin < tilesheet_0.apple2  || exit
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/font7x8.0 bin < font7x8_0.apple2  || exit
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/font7x8.1 bin < font7x8_1.apple2  || exit
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/imagesheet.0 bin < imagesheet_0.apple2  || exit
java -jar C:\jar\AppleCommander.jar -p mono_prodos.dsk data/title.0 bin < title.0  || exit
java -jar C:\jar\AppleCommander.jar -p mono_prodos.dsk data/title.1 bin < title.1  || exit

:: Copy results out of the build directory
copy mono_prodos.dsk ..\disk || exit

::---------------------------------------------------------------------------
:: Test on emulator
::---------------------------------------------------------------------------

C:\AppleWin\Applewin.exe -no-printscreen-dlg -d1 mono_prodos.dsk

