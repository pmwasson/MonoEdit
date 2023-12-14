import sys
from PIL import Image

def main():
    outputBytes = bytearray();
    im = Image.open(sys.argv[1]).resize((140,64)).convert("1")

    evenData = []
    oddData = []
    for y in range(im.size[1]):
        #print(".byte ",end='')
        value = 0
        line = []
        for x in range(im.size[0]):
            if (im.getpixel((x,y)) != 0):
                value = value | (1 << x%7)
            if (x%7 == 6):
                line.append(value)
                #if (x != 6):
                #    print(",",end='')
                #print(value,end='')
                outputBytes.append(value)
                value = 0

        evenData.append(line[0::2])
        oddData.append(line[1::2])

    with open(sys.argv[2], "wb") as binary_file:
        byteCount = binary_file.write(outputBytes)

    print("; --- Even bytes ---")
    for line in evenData:
        print(".byte ",end="")
        print(*line,sep=", ")

    print("; --- Odd bytes ----")
    for line in oddData:
        print(".byte ",end="")
        print(*line,sep=", ")
main()