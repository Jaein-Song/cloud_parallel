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
timer[$i]=0
fl=(`ls ${dir[$i]}/*THI_900*.cfradial`)
fln=${#fl[*]}
let i++
j=0
k=0
l=0
while [ $k -lt $fln ]; do
        hmsf[$k]=${fl[$k]##/*$ymd}
        hmsf[$k]=${hmsf[$k]:0:6}
	hf=${hmsf[$k]:0:2}
	mf=${hmsf[$k]:2:2}
	sf=${hmsf[$k]:4:2}
	hf=`expr ${hf#0} * 3600`
	mf=`expr ${mf#0} * 60`
	sf= ${sf#0}
	timef[$k]=`expr $hf + $mf + $sf`
	timee[$k]=`expr ${timef[$k]} + $CLD_flen`
        let k++
done
k=0
while [ $i -le $tlen ]; do
        hr[$i]=${hr[$j]}
        mr[$i]=${mr[$j]}
        sr[$i]=`expr ${sr[$j]} + $time_intv`
	timer[$i]=`expr ${timer[$j]} + $time_intv`
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
                if [ ${timef[$k]} -lt ${timer[$i]} ]; then
                        while [ ${timee[$k]} -lt ${timer[$j]} ]; do
                                let k++
                                echo $ymd $i $k
                        done
                        n=0
                        m=$k
                        while [ ${timef[$k]} -lt ${timer[$i]} ]; do
                                let n++
                                let k++
                        done

                        if [ $n -gt 0 ]; then
                                echo $i $n ${hmsr[$j]} ${hmsr[$i]}>>$current_dir/totalQC/ifl$ppn
                                while [ $m -lt $k ]; do
					ldt=`expr ${timer[$j]} - ${timef[$m]}`
					ldt=`expr $ldt * 1000`
					udt=`expr ${timer[$i]} - ${timef[$m]}`
					udt=`expr $udt * 1000`
					intv=`expr $CLD_flen * 1000`
					if [ $ldt -gt 0 ]; then
						l=0
						time_inc=`expr $l * $CLD_tres`
						while [ $time_inc -lt $ldt ]; do
							let l++
							time_inc=`expr $l * $CLD_tres`
						done
						sbin=`expr $l + 1`
					else
						sbin=1
					fi
					if [ $udt -lt $flen ]; then
						l=$sbin
						time_inc=`expr $l * $CLD_tres`
						while [ $time_inc -lt $udt ]; do
							let l++
							time_inc=`expr $l * $CLD_tres`
						done
						ebin=$l
						k=`expr $k - 1`
					else
						ebin=$CLD_fcount
					fi
					echo $sbin $ebin
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

