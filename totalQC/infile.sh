#!/bin/bash
rm $current_dir/totalQC/ifl$ppn
echo $flag >$current_dir/totalQC/ifl$ppn
echo $ofn >>$current_dir/totalQC/ifl$ppn
tlen=`expr 86400 / $time_intv`
echo $tlen >>$current_dir/totalQC/ifl$ppn
i=0
hr[$i]=0
mr[$i]=0
sr[$i]=0
hrs[$i]=`printf %02g ${hr[$i]}`
mrs[$i]=`printf %02g ${mr[$i]}`
srs[$i]=`printf %02g ${sr[$i]}`
hmsr[$i]=${hrs[$i]}${mrs[$i]}${srs[$i]}
fl=(`ls ${dir[$i]}/*THI_900*.cfradial`)
fln=${#fl[*]}
let i++
j=0
k=0
l=0
while [ $k -lt $fln ]; do
        hmsf[$k]=${fl[$k]##/*$ymd}
        hmsf[$k]=${hmsf[$k]:0:6}
        let k++
done
k=0
while [ $i -le $tlen ]; do
        hr[$i]=${hr[$j]}
        mr[$i]=${mr[$j]}
        sr[$i]=`expr ${sr[$j]} + $time_intv`
        while [ ${sr[$i]} -gt 59 ]; do
                sr[$i]=`expr ${sr[$i]} - 60`
                mr[$i]=`expr ${mr[$i]} + 1`
        done
        while [ ${mr[$i]} -gt 59 ]; do
                mr[$i]=`expr ${mr[$i]} - 60`
                hr[$i]=`expr ${hr[$i]} + 1`
        done
        hrs[$i]=`printf %02g ${hr[$i]}`
        mrs[$i]=`printf %02g ${mr[$i]}`
        srs[$i]=`printf %02g ${sr[$i]}`
        hmsr[$i]=${hrs[$i]}${mrs[$i]}${srs[$i]}
        if [ $k -lt $fln ]; then
                if [ ${hmsf[$k]} -lt ${hmsr[$i]} ]; then
                        while [ ${hmsf[$k]} -lt ${hmsr[$j]} ]; do
                                let k++
                                echo $ymd $i $k
                        done
                        n=0
                        m=$k
                        while [ ${hmsf[$k]} -lt ${hmsr[$i]} ]; do
                                let n++
                                let k++
                        done

                        if [ $n -gt 0 ]; then
                                echo $i $n ${hmsr[$j]} ${hmsr[$i]}>>$current_dir/totalQC/ifl$ppn
                                while [ $m -lt $k ]; do
                                        echo ${fl[$m]}>>$current_dir/totalQC/ifl$ppn
                                        let m++

                                done
                        fi
                fi
        else
                i=$tlen
        fi
        let i++
        let j++
done

