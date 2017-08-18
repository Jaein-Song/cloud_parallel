#!/bin/bash
export Fig_dir=/data2/ncio/CLD_258/hmbprodat/CFRADIAL/Figs	
export web_dir=$Fig_dir/CLD1
figlist1=(`ls $Fig_dir/*/*noQC_FTD*201707*.png`)
figlistnum=${#figlist1[*]}
#mkdir -p $web_dir
i=0
while [ $i -lt $figlistnum ]; do
    linkname=${figlist1[$i]##/*_}
    linkname=$web_dir'/'$linkname
    cp  ${figlist1[$i]} $linkname
    let i++
done

figlist2=(`ls $Fig_dir/*/*noQC_NFT*201707*.png`)
web_dir=${web_dir/CLD1/CLD2}
#mkdir -p $web_dir
figlistnum=${#figlist2[*]}
i=0
while [ $i -lt $figlistnum ]; do
    linkname=${figlist2[$i]##/*_}
    linkname=$web_dir'/'$linkname
    cp  ${figlist2[$i]} $linkname
    let i++
done

figlist3=(`ls $Fig_dir/*/*QC14_FTD*201707*.png`)
web_dir=${web_dir/CLD2/CLD3}
#mkdir -p $web_dir
figlistnum=${#figlist3[*]}
i=0
while [ $i -lt $figlistnum ]; do
    linkname=${figlist3[$i]##/*_}
    linkname=$web_dir'/'$linkname
    cp  ${figlist3[$i]} $linkname
    let i++
done

figlist4=(`ls $Fig_dir/*/*QC15_FTD*201707*.png`)
web_dir=${web_dir/CLD3/CLD4}
#mkdir -p $web_dir
figlistnum=${#figlist4[*]}
i=0
while [ $i -lt $figlistnum ]; do
    linkname=${figlist4[$i]##/*_}
    linkname=$web_dir'/'$linkname
    cp  ${figlist4[$i]} $linkname
    let i++
done

figlist5=(`ls $Fig_dir/*/*QC17_NFT*201707*.png`)
web_dir=${web_dir/CLD4/CLD5}
#mkdir -p $web_dir
figlistnum=${#figlist5[*]}
i=0
while [ $i -lt $figlistnum ]; do
    linkname=${figlist5[$i]##/*_}
    linkname=$web_dir'/'$linkname
    cp  ${figlist5[$i]} $linkname
    let i++
done

figlist6=(`ls $Fig_dir/*/*QC17_CIL*.png`)
web_dir=${web_dir/CLD5/CLD6}
#mkdir -p $web_dir
figlistnum=${#figlist6[*]}
i=0
while [ $i -lt $figlistnum ]; do
    linkname=${figlist6[$i]##/*_}
    linkname=$web_dir'/'$linkname
    cp  ${figlist6[$i]} $linkname
    let i++
done
