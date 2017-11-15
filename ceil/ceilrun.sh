#/data2/ncio/CL51_258/2017/01/A7010100.DAT
#dl=(`ls -d $ceil_dir/2*$pmn/*`)
ceil_dl=(`cat $current_dir/ceil_dl`)
dln=${#ceil_dl[*]}
i=${ppn#0}
time_min=`expr $time_intv / 60`
echo $i $dln
while [ $i -lt $dln ]; do
    j=1
    p=${ceil_dir##/*/}
    y=${ceil_dl[$i]##/*$p/}
    echo $y >>dl
    y=${y:0:4}
    y1=${y:3:1}
    mm=${ceil_dl[$i]:${#ceil_dl[$i]}-2:2}
    while [ $j -le 5 ]; do
    dd=`printf %02g $j`
    fn=`ls ${ceil_dl[$i]}/A$y1$mm$dd'00.DAT' |wc -l`
        if [ $fn -eq 1 ]; then
            ifname1=${ceil_dl[$i]}/A$y1$mm$dd'00.DAT'
            ifname2=${ceil_dl[$i]}'/CEILOMETER_1_LEVEL_3_DEFAULT_'$dd'.his'
            `mkdir -p $ceil_dir/netCDF/201$y1/`
            doy=`date -d $y$mm$dd +%j`
            ofname1=$ceil_cdf_dir/$y/'NCIO_CIL_noQC_RAW__daily_'$doy'_'$y$mm$dd.'cdf'
            ofname2=$ceil_cdf_dir/$y/'NCIO_CIL_avg'$time_min'_LV3__daily_'$doy'_'$y$mm$dd.'cdf'
            ifn1=`ls $ifname1 |wc -l`
            ifn2=`ls $ifname2 |wc -l`
            ofn=`ls $ofname1 |wc -l`
	    echo ===============ifn,ofn,ifname1,ifnae2,dd================
	    echo $ifn1
	    echo $ofn
	    echo $ifname1
	    echo $ifname2
	    echo $dd $dupl
	    echo =====================testprint end======================
            if [ $ofn -lt $dupl ]; then
            if [ $ifn1 -gt 0 ]; then
            if [ $ifn2 -gt 0 ]; then
                rm ifl$ppn 
                #sleep 0.2
                echo '1'> $current_dir/ceil/ifl$ppn
		echo $time_min >>$current_dir/ceil/ifl$ppn
                echo $ifname1 >> $current_dir/ceil/ifl$ppn
                echo $ofname1 >> $current_dir/ceil/ifl$ppn
                echo $ifname2 >> $current_dir/ceil/ifl$ppn
                echo $ofname2 >> $current_dir/ceil/ifl$ppn
		export task_name=ceilrun$ppn
		#$current_dir/cpu_proc.sh >>$current_dir/logs/cpulog_$taskname &
                $current_dir/ceil/ceilrun$ppn
            else
                rm ifl$ppn 
                #sleep 0.2
                echo '0'> $current_dir/ceil/ifl$ppn
		echo $time_min >>$current-dir/ceil/ifl$ppn
                echo $ifname1 >> $current_dir/ceil/ifl$ppn
                echo $ofname1 >> $current_dir/ceil/ifl$ppn
                echo $ofname2 >> $current_dir/ceil/ifl$ppn
		export task_name=ceilrun$ppn
		#$current_dir/cpu_proc.sh >>$current_dir/logs/cpulog_$taskname &
                $current_dir/ceil/ceilrun$ppn
            fi
            fi
            fi
        fi
	let j++
    done
    #let i++
    i=`expr $i + $num_cpu`
done
#ppn=`printf %02g $ppn`
mkdir -p $current_dir/doneflags 
echo '1' > $current_dir/doneflags/cpustat_$ppn
#rm $current_dir/ceil/ifl$ppn
