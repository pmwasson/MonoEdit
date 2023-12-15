import sys
import os.path
from PIL import Image

def main():
    print(";","--------------------------------------------------------------------")
    #outputBytes = bytearray();
    im = Image.open(sys.argv[1])
    print(";",sys.argv[1],im.format, im.size, im.mode)
    im = im.resize((140,64)).convert("1")
    im.save(sys.argv[2])
    print(";",sys.argv[2],im.format, im.size, im.mode)

    evenData = []
    oddData = []
    for y in range(im.size[1]):
        value = 0
        line = []
        for x in range(im.size[0]):
            if (im.getpixel((x,y)) != 0):
                value = value | (1 << x%7)
            if (x%7 == 6):
                line.append(value)
                #outputBytes.append(value)
                value = 0

        evenData.append(line[0::2])
        oddData.append(line[1::2])

    #with open(sys.argv[2], "wb") as binary_file:
    #    byteCount = binary_file.write(outputBytes)

    name = os.path.basename(os.path.splitext(sys.argv[2])[0])
    print("{}Even:".format(name))
    for line in evenData:
        print(".byte ",end="")
        print(*line,sep=", ")

    print("{}Odd:".format(name))
    for line in oddData:
        print(".byte ",end="")
        print(*line,sep=", ")
main()