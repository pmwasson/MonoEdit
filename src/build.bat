::---------------------------------------------------------------------------
:: Compile code
::   Assemble twice: 1 to generate listing, 2 to generate object
::---------------------------------------------------------------------------
cd ..\build

:: Font Editor
ca65 -I ..\src -t apple2 ..\src\fontedit.asm -l fontedit.dis || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\fontedit.asm apple2.lib  -o fontedit.apple2 -C ..\src\start4000.cfg || exit

:: Map Editor
ca65 -I ..\src -t apple2 ..\src\mapedit.asm -l mapedit.dis || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\mapedit.asm apple2.lib  -o mapedit.apple2 -C ..\src\start4000.cfg || exit

:: Tile Editor
ca65 -I ..\src -t apple2 ..\src\tileedit.asm -l tileedit.dis || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileedit.asm apple2.lib  -o tileedit.apple2 -C ..\src\start6000.cfg || exit

:: Engine
ca65 -I ..\src -t apple2 ..\src\engine.asm -l engine.dis || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\engine.asm apple2.lib  -o engine.apple2 -C ..\src\startC00.cfg || exit

:: Loader
ca65 -I ..\src -t apple2 ..\src\loader.asm -l loader.dis || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\loader.asm apple2.lib  -o loader.apple2 -C ..\src\start2000.cfg || exit

:: Image
ca65 -I ..\src -t apple2 --cpu 65C02 ..\src\displayImage.asm -l image.dis || exit
cl65 -I ..\src -t apple2 --cpu 65C02 -u __EXEHDR__ ..\src\displayImage.asm apple2.lib  -o image.apple2 -C ..\src\start4000.cfg || exit

::---------------------------------------------------------------------------
:: Compile assets
::---------------------------------------------------------------------------

cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tilesheet_0.asm apple2.lib  -o tilesheet_0.apple2 -C ..\src\start6000.cfg || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\font7x8_0.asm apple2.lib  -o font7x8_0.apple2 -C ..\src\start6000.cfg || exit
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\font7x8_1.asm apple2.lib  -o font7x8_1.apple2 -C ..\src\start6000.cfg || exit

::---------------------------------------------------------------------------
:: Build disk 
::---------------------------------------------------------------------------

:: Start with a blank prodos disk
copy ..\disk\template_prodos.dsk mono_prodos.dsk  || exit

:: Loader
java -jar C:\jar\AppleCommander.jar -p  mono_prodos.dsk loader.system sys < C:\cc65\target\apple2\util\loader.system || exit
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk loader bin < loader.apple2  || exit
::java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk loader.system sys < loader.apple2  || exit

:: Image
java -jar C:\jar\AppleCommander.jar -p  mono_prodos.dsk image.system sys < C:\cc65\target\apple2\util\loader.system || exit
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk image bin < image.apple2  || exit

:: Font Editor
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/fontedit bin < fontedit.apple2  || exit

:: Map Editor
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/mapedit bin < mapedit.apple2  || exit

:: Tile Editor
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/tileedit bin < tileedit.apple2  || exit

:: Throw on basic
java -jar C:\jar\AppleCommander.jar -p mono_prodos.dsk basic.system sys < ..\disk\BASIC.SYSTEM  || exit

:: Assets
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/engine bin < engine.apple2  || exit
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/tilesheet.0 bin < tilesheet_0.apple2  || exit
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/font7x8.0 bin < font7x8_0.apple2  || exit
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/font7x8.1 bin < font7x8_1.apple2  || exit

java -jar C:\jar\AppleCommander.jar -p mono_prodos.dsk data/title.0 bin < title.0  || exit
java -jar C:\jar\AppleCommander.jar -p mono_prodos.dsk data/title.1 bin < title.1  || exit

:: Copy results out of the build directory
copy mono_prodos.dsk ..\disk || exit

::---------------------------------------------------------------------------
:: Test on emulator
::---------------------------------------------------------------------------

C:\AppleWin\Applewin.exe -no-printscreen-dlg -d1 mono_prodos.dsk

