#!/bin/bash

set -e -u -o pipefail

proto=6
maxtime=1
fdate=$1
ftime=$2
fyear=${fdate:0:4}

if [ ! -d "ses_src/" ]; then
    mkdir "ses_src/"
fi

outdir="ses_src/${fdate}"
if [ ! -d "$outdir" ]; then
    mkdir "$outdir"
fi

./ipsrc_ses_extract.py /home/sci/data/ivanovo/logs/${fdate}/log-${fdate}-${ftime}.txt.gz ${outdir}/ses_${fdate}_${ftime}.txt -p ${proto} -t ${maxtime}
#gzip ${outdir}/ses_${fdate}${ftime}.txt



