@echo off

python3 imageConvert.py wizard1.jpg     ..\build\wizard1.png    140 64 >  ..\src\images0.asm  || exit
python3 imageConvert.py wizard2.jpg     ..\build\wizard2.png    140 64 >> ..\src\images0.asm  || exit
python3 imageConvert.py robot2.jpg      ..\build\robot.png      140 64 >> ..\src\images0.asm  || exit

python3 imageConvert.py gypsy1.jpg      ..\build\gypsy1.png     140 64 >  ..\src\images1.asm  || exit
python3 imageConvert.py computer.jpg    ..\build\computer.png   140 64 >> ..\src\images1.asm  || exit
python3 imageConvert.py ff.jpg          ..\build\ff.png         140 64 >> ..\src\images1.asm  || exit

python3 imageConvert.py elf1.jpeg       ..\build\elf1.png       140 64 >  ..\src\images2.asm  || exit
python3 imageConvert.py girl.jpeg       ..\build\girl.png       140 64 >> ..\src\images2.asm  || exit
python3 imageConvert.py girl3.jpeg      ..\build\girl3.png      140 64 >> ..\src\images2.asm  || exit

python3 imageConvert.py hero.jpg        ..\build\hero.png       140 64 >  ..\src\images3.asm  || exit
python3 imageConvert.py ogre.jpeg       ..\build\ogre.png       140 64 >> ..\src\images3.asm  || exit
python3 imageConvert.py warrior.jpg     ..\build\warrior.png    140 64 >> ..\src\images3.asm  || exit

python3 imageConvert.py goblin.jpg      ..\build\goblin.png     140 64 >  ..\src\images4.asm  || exit
python3 imageConvert.py warrior2.jpg    ..\build\warrior2.png   140 64 >> ..\src\images4.asm  || exit
python3 imageConvert.py warrior3.jpeg   ..\build\warrior3.png   140 64 >> ..\src\images4.asm  || exit

python3 imageConvert.py title2.jpg      ..\build\title.png       560 192 > ..\src\title.asm   || exit
