set datafile separator "\t"
set terminal svg size 8000,4000
set output '<OUTPUT_FILE>' 
set datafile missing ""

set xlabel "ms"

set yrange [-1:*]

set xrange [10:*]

set boxwidth 0.3
set style fill solid  # solid color boxes
# unset key             # turn off all titles

myBoxWidth = 0.4
set offsets 0,0,0.5-myBoxWidth/2.,0.5

plot '<INPUT_FILE>' using (2):0:0:2:($0-myBoxWidth/2.):($0+myBoxWidth/2.):($0+1) index 0 with boxxyerror title "before", \
     '' using ($3+0.2):0:0:3:($0+0.5-myBoxWidth/2.):($0+0.5+myBoxWidth/2.):($0+1) index 0 with boxxyerror title "after", \
     '' using 2:0:ytic(1) lw 0 ps 0 notitle

