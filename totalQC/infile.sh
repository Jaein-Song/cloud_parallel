#!/bin/bash
rm $current_dir/totalQC/ifl$ppn
echo $flag >ifl$ppn
echo $ofn >>ifl$ppn
tlen=`expr 86400 / $time_intv`
echo $tlen >>ifl$ppn
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
	${hmsf[$k]}=${fl[$k]##/*$ymd}
	${hmsf[$k]}=${hmsf:0:6}
	let k++
done
k=0
while [ $i -le $tlen ]; do
	hr[$i]=0
	mr[$i]=0
	sr[$i]=`expr $sr + $time_intv`
	while [ $sr -gt 59 ]; do
		sr[$i]=`expr $sr - 60`
		mr[$i]=`expr $mr + 1`
	done
	while [ $mr -gt 59 ]; do
		mr[$i]=`expr $mr - 60`
		hr[$i]=`expr $hr + 1`
	done
	hrs[$i]=`printf %02g ${hr[$i]}`
	mrs[$i]=`printf %02g ${mr[$i]}`
	srs[$i]=`printf %02g ${sr[$i]}`
	hmsr[$i]=${hrs[$i]}${mrs[$i]}${srs[$i]}	
	if [ $k -lt $fln ]; then
		if [ ${hmsf[$k]} -lt ${hmsr[$i]} ]; then
			while [ ${hmsf[$k]} -lt ${hmsr[$j]}]; do
				let k++
			done
			n=0
			m=$k
			while [ ${hmsf[$k]} -lt ${hmsr[$i]}]; do
				let n++
				let k++
			done
			if [ $n -gt 0 ]; then
				echo $i $n >>ifl$ppn
				while [ $m -lt $k ]; do
					echo ${fl[$m]}>>ifl$ppn
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

