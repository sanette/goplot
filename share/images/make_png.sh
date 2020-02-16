#!/bin/bash
# "./make_png.sh param" convertit param.svg en param_icon64.png et 128.png, etc

file=$1

for i in 16 32 48 64 96 128 192
do
    out="/tmp/${file}_icon${i}.png"
    echo $out
    inkscape -z -e $out -w $i -h $i ${file}.svg

    # malheureusement rsvg ne marche pas bien, la taille du svg n'est
    # pas bien détectée... :(
    #rsvg-convert -w $i -h $i ${file}.svg -o "/tmp/${file}_icon${i}.png"
done
