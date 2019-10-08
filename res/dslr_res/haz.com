#!/bin/bash
#\rm -f dslr_res

outfile=dslr_res

if [ -f $outfile ]; then
  \rm -f $outfile
fi

for f in resp*.csv; do
  printf "%9d  %s\n" `wc $f | awk '{print $1}'` $f >> $outfile
  cat $f >> $outfile
done
