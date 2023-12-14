::---------------------------------------------------------------------------
:: Compile code
::   Assemble twice: 1 to generate listing, 2 to generate object
::---------------------------------------------------------------------------
cd ..\build

ca65 -I ..\src -t apple2 ..\src\editor.asm -l editor.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\editor.asm apple2.lib  -o editor.apple2 -C ..\src\start4000.cfg

:: Engine
ca65 -I ..\src -t apple2 ..\src\engine.asm -l engine.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\engine.asm apple2.lib  -o engine.apple2 -C ..\src\startC00.cfg

:: Loader
ca65 -I ..\src -t apple2 ..\src\loader.asm -l loader.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\loader.asm apple2.lib  -o loader.apple2 -C ..\src\start2000.cfg

:: Image
ca65 -I ..\src -t apple2 ..\src\displayImage.asm -l image.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\displayImage.asm apple2.lib  -o image.apple2 -C ..\src\start6000.cfg

::---------------------------------------------------------------------------
:: Compile assets
::---------------------------------------------------------------------------

cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileset56x16_0.asm apple2.lib  -o tileset56x16_0.apple2 -C ..\src\start6000.cfg
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\tileset7x8_0.asm apple2.lib  -o tileset7x8_0.apple2 -C ..\src\start6000.cfg

::---------------------------------------------------------------------------
:: Build disk 
::---------------------------------------------------------------------------

:: Start with a blank prodos disk
copy ..\disk\template_prodos.dsk mono_prodos.dsk

:: Put boot program first

:: Loader
java -jar C:\jar\AppleCommander.jar -p  mono_prodos.dsk loader.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk loader bin < loader.apple2 

:: Editor
java -jar C:\jar\AppleCommander.jar -p  mono_prodos.dsk editor.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/editor bin < editor.apple2 

:: Image
java -jar C:\jar\AppleCommander.jar -p  mono_prodos.dsk image.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk image bin < image.apple2 

:: Throw on basic
java -jar C:\jar\AppleCommander.jar -p mono_prodos.dsk basic.system sys < ..\disk\BASIC.SYSTEM 

:: Assets
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/engine bin < engine.apple2 
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/tileset56x16.0 bin < tileset56x16_0.apple2 
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk data/tileset7x8.0 bin < tileset7x8_0.apple2 

:: Copy results out of the build directory
copy mono_prodos.dsk ..\disk

::---------------------------------------------------------------------------
:: Test on emulator
::---------------------------------------------------------------------------

C:\AppleWin\Applewin.exe -no-printscreen-dlg -d1 mono_prodos.dsk

