grep 'nb pieces' out_P1 | cut -d":" -f3 > out_P1.data
grep 'nb pieces' out_P2 | cut -d":" -f3 > out_P2.data
echo 'plot "out_P1.data" using 1 with lines, "out_P2.data" using 1 with lines'
gnuplot

