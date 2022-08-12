set datafile separator "\t"
set terminal svg
set output '<OUTPUT_FILE>'
set datafile missing ""

set title "<TITLE>"
set ylabel "ms"

set yrange [10:*]

set xtics rotate by 45 offset 0,-1 font ",9"

set boxwidth 0.3
set style fill solid 1.000000 border -1
set bmargin 3
set pointsize 2
set tics scale 0.0
set grid y
set style data lines
plot '<INPUT_FILE>' using ($0-0.2):3 index 0 with boxes title "before", \
     '' using ($0+0.2):4 index 0 with boxes title "after", \
     '' using ($0):xtic(1) lw 0 notitle  

