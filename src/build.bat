::---------------------------------------------------------------------------
:: Compile code
::   Assemble twice: 1 to generate listing, 2 to generate object
::---------------------------------------------------------------------------
cd ..\build

ca65 -I ..\src -t apple2 ..\src\mono.asm -l mono.dis
cl65 -I ..\src -t apple2 -u __EXEHDR__ ..\src\mono.asm apple2.lib  -o mono.apple2 -C ..\src\start4000.cfg

::---------------------------------------------------------------------------
:: Build disk 
::---------------------------------------------------------------------------

:: Start with a blank prodos disk
copy ..\disk\template_prodos.dsk mono_prodos.dsk

:: Put boot program first

java -jar C:\jar\AppleCommander.jar -p  mono_prodos.dsk mono.system sys < C:\cc65\target\apple2\util\loader.system
java -jar C:\jar\AppleCommander.jar -as mono_prodos.dsk mono bin < mono.apple2 

:: Throw on basic
java -jar C:\jar\AppleCommander.jar -p mono_prodos.dsk basic.system sys < ..\disk\BASIC.SYSTEM 

:: Copy results out of the build directory
copy mono_prodos.dsk ..\disk

::---------------------------------------------------------------------------
:: Test on emulator
::---------------------------------------------------------------------------

C:\AppleWin\Applewin.exe -no-printscreen-dlg -d1 mono_prodos.dsk

