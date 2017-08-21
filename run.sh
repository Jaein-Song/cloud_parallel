#!/bin/bash
#Cloud radar data process, Parallel computing for 12 cpus
## CONFIGURATION AND INITIATION FILE
## MADE BY J.I.Song, NIMS, 2017
## THIS FILE MUST BE MODIFIED BEFORE USE THE ENTIRE PROGRAM IN ANY OTHER ENVIRONMENT.
## compile first
#Set file path or switch in this file
rm -rf *.mod
rm -rf *.o
source ./list                   # Call the configuration file.
cd $current_dir
chmod +x *sh */*.sh
# compile pararell bins
ppn=0                       #Process Parallel computing Number
pmn=1                       #Process Month Number

while [ $ppn -lt $num_cpu ]; do
    export ppn=$ppn
    pmn=`printf %02g $pmn`
    ppn=`printf %02g $ppn`
    rm -rf $current_dir/covupCFrad/frun$ppn
    rm -rf $current_dir/ceil/ceilrun$pid
    rm -rf $current_dir/totalQC/crun$pid
    export pmn=$pmn
    $current_dir/flmodules.sh
    #ceil
    if [ $flag_ceil -gt 0 ]; then
	export ceilfl=(`ls -d $ceil_dir/2*/*`)
	insource="$current_dir/ceil/varmod.f90 $current_dir/ceil/ceil.f90 $current_dir/ceil/Sorting.f90 $current_dir/ceil/ncwrite.f90 $current_dir/ceil/subs.f90"
	outbinary='-o '$current_dir'/ceil/ceilrun'$ppn
	$FC $options1 $insource $FC_NC_lib $outbinary
    fi
    #covupCF
    if [ $flag_b2n -gt 0 ]; then
	insource="$current_dir/covupCFrad/mod_para.f $current_dir/covupCFrad/varmod.f90 $current_dir/covupCFrad/covup_data.f90 $current_dir/covupCFrad/importpdf.f90 $current_dir/covupCFrad/sub_head.f $current_dir/covupCFrad/sub_block.f $current_dir/covupCFrad/write_cfradial.f $current_dir/covupCFrad/qc14.f90 $current_dir/covupCFrad/qc15.f90"
	outbinary='-o '$current_dir'/covupCFrad/frun'$ppn
	$FC $options $insource $FC_NC_lib $outbinary
    fi
    #totalQC
    if [ $flag_day -gt 0 ]; then
	insource="$current_dir/totalQC/varmod.f90 $current_dir/totalQC/covup_dailyCFradial.f90 $current_dir/totalQC/noqcavg.f90 $current_dir/totalQC/knuavg.f90 $current_dir/totalQC/qc17avg.f90 $current_dir/totalQC/qc17_ceil.f90 $current_dir/totalQC/ncwrite.f90 $current_dir/totalQC/ncread.f90"
	outbinary='-o '$current_dir'/totalQC/'crun$ppn
	$FC $options $insource $FC_NC_lib $outbinary
    fi
    chmod +x $current_dir/*/*run$ppn
    pmn=${pmn#0}
    ppn=${ppn#0}
    let pmn++
    let ppn++
done
ppn=0                       #Process Parallel computing Number
pmn=1                       #Process Month Number
while [ $ppn -lt $num_cpu ]; do
    export ppn=$ppn
    ppn=`printf %02g $ppn`
    pmn=`printf %02g $pmn`
    export pmn=$pmn
    $current_dir/execute.sh >$current_dir/logs/'cpu'$ppn'_log' &
    pmn=${pmn#0}
    ppn=${ppn#0}
    let pmn++
    let ppn++
done
#$current_dir/plot.sh
echo END
