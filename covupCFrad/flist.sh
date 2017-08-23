#!/bin/bash
ibd=$CLD_dir
dl=(`cat $current_dir/dl`)
dln=${#dl[*]}
i=${ppn#0}
while [ $i -lt $dln ]; do
	echo processing file at ${dl[$i]}
	fl=(`ls ${dl[$i]}/*THI_900*.OBS`)
	fln=${#fl[*]}
	flt=${dl[$i]##/*BASEDAT}
	flt=${flt:0:1}
#echo $flt
#	odt=${dl[$i]/BASEDAT/CFRADIAL\/BASEDAT}
	mkdir -p ${dl[$i]/$CLD_dir/$CF_dir}
	if [ $flt = 'C' ]; then
		ofc=${dl[$i]/$CLD_dir/$CF_dir}
		ofnno=`ls $ofc/*.cfradial|wc -l`
		odt=${dl[$i]/BASEDATC/QC14}
		od14=${odt/$CLD_dir/$CF_dir}
		ofn14=`ls $od14/*.cfradial|wc -l`
		mkdir -p ${odt/$CLD_dir/$CF_dir}
		odt=${dl[$i]/BASEDATC/QC15}
		od15=${odt/$CLD_dir/$CF_dir}
		ofn15=`ls $od15/*.cfradial|wc -l`
		mkdir -p ${odt/$CLD_dir/$CF_dir}
	else
		ofd=${dl[$i]/$CLD_dir/$CF_dir}
		ofnno=`ls $ofc/*.cfradial|wc -l`
		ofn14=$fln
		ofn15=$fln
	fi
	if [ $ofnno -eq $fln ]; then
		flagno=1
	fi
	if [ $ofn14 -eq $fln ]; then
		flag14=1
	fi
	if [ $ofnno -eq $fln ]; then
		flag15=1
	fi
	flagt=`expr $flagno + $flag14 + $flag15 `
	if [ $flagt -lt 3 ]; then
	if [ $fln -gt 1 ]; then
	j=0
		while [ $j -lt $fln ]; do
		p=${fl[$j]}
		pl=`du $p`
		fsize=${pl%%$CLD_dir*.OBS}
		stype=${p##/*__}
		stype=${stype:0:3}
		if [ $fsize -gt 6000 ] ; then
			q=${p/OBS/cfradial}
			q=${q/$CLD_dir/$CF_dir}
			if [ $flt = 'C' ]; then
				if [ "$stype" = "THI" ]; then
		        	q14=${q/0_C/0_C_QC14}
					q14=${q14/BASEDATC/QC14}
					q15=${q/0_C/0_C_QC15}
					q15=${q15/BASEDATC/QC15}
					lgc=`ls $q |wc -l`
					lgc14=`ls $q14 |wc -l`
					lgc15=`ls $q15 |wc -l`
				else # PPI and RHI scan are not subject to QC14 and QC15
					lgc=1
					lgc14=1 
					lgc15=1
				fi
			else #QC14 and QC15 uses only filtered data (C)
				if [ "$stype" = "THI" ]; then
					lgc=`ls $q |wc -l`
					lgc14=1  
					lgc15=1
				else
					lgc=1
					lgc14=1  
					lgc15=1
				fi
					
			fi

			lgct=`expr $lgc + $lgc14 + $lgc15`
			echo $q
			echo $lgc $lgc14 $lgc15
			if [ $lgct -lt 3 ]; then
				rm ifl ofl14 ofl ofl15
				echo $lgc $lgc14 $lgc15 >$current_dir/covupCFrad/ifl$ppn 
			 	echo $p >>$current_dir/covupCFrad/ifl$ppn
				if [ $lgc -lt 1 ]; then
					echo $q > $current_dir/covupCFrad/ofl$ppn
				fi
				if [ $lgc14 -lt 1 ]; then
					echo $q14 > $current_dir/covupCFrad/ofl14$ppn
				fi
				if [ $lgc15 -lt 1 ]; then
					echo $q15 > $current_dir/covupCFrad/ofl15$ppn
				fi
echo "EOF" >> $current_dir/covupCFrad/ifl$ppn
echo "EOF" >> $current_dir/covupCFrad/ofl$ppn
echo "EOF" >> $current_dir/covupCFrad/ofl14$ppn
echo "EOF" >> $current_dir/covupCFrad/ofl15$ppn
			export task_name=frun$ppn
			$current_dir/cpu_proc.sh >> $current_dir/logs/cpulog_$task_name &	
			$current_dir/covupCFrad/frun$ppn
			fi
		fi
		let j++
		done
	fi
	fi
#	let i++
	i=`expr $i + $num_cpu`
done
ppn=`printf %02g $ppn`
echo '1' >$current_dir/doneflags/cpustat_$ppn
