#!/bin/bash
#Cloud radar data process, Parallel computing for multiple cpus
## CONFIGURATION AND EXECUTION FILE
## MADE BY J.I.Song, NIMS, 2017
## TO USE IN OTHER ENVIRONMENT, list file should be modifed first
## compile first
#Set file path or switch in this file
rm -rf *.mod
rm -rf *.o
source ./list                   # Call the configuration file.
cd $current_dir
chmod +x *sh */*.sh
# compile pararell bins
ppn=0                       #Process Parallel computing Number

while [ $ppn -lt $num_cpu ]; do
    export ppn=$ppn
    ppn=`printf %02g $ppn`
    rm -rf $current_dir/covupCFrad/frun$ppn
    rm -rf $current_dir/ceil/ceilrun$pid
    rm -rf $current_dir/totalQC/crun$pid
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
    ppn=${ppn#0}
    let ppn++
done

#File directory refine, would be done in sequential process
if [ $flag_refn -gt 0 ]; then
	$current_dir/refinefiledir.sh
fi

##START CEIL PROCESS
if [ $flag_ceil -gt 0 ]; then
	ppn=0                       #Process Parallel computing Number
	while [ $ppn -lt $num_cpu ]; do
    		export ppn=`printf %02g $ppn`
		$current_dir/ceil/ceilrun.sh >$current_dir/logs/ceil_cpu_$ppn&
		ppn=${ppn#0}
		let ppn++
	done
prev_job_flag=0
while [ $prev_job_flag -lt 1 ]; do
	doneflagsnum=`ls $current_dir/doneflags/done_*|wc -l`
	if [ $doneflagsnum -eq $num_cpu ]; then
		prev_job_flag=1
		rm -rf $current_dir'/doneflags/done*'
	fi
done
fi

##START BINARY TO NETCDF PROCESS
if [ $flag_b2n -gt 0 ]; then
	ppn=0                       #Process Parallel computing Number
	while [ $ppn -lt $num_cpu ]; do
    		export ppn=`printf %02g $ppn`
		$current_dir/covupCFrad/covrun.sh >$current_dir/logs/b2n_cpu_$ppn&
		ppn=${ppn#0}
		let ppn++
	done
prev_job_flag=0
while [ $prev_job_flag -lt 1 ]; do
	doneflagsnum=`ls $current_dir/doneflags/done_*|wc -l`
	if [ $doneflagsnum -eq $num_cpu ]; then
		prev_job_flag=1
		rm -rf $current_dir'/doneflags/done*'
	fi
done
fi

##START MERGE FILES DAILY
if [ $flag_day -gt 0 ]; then
	ppn=0                       #Process Parallel computing Number
	while [ $ppn -lt $num_cpu ]; do
    		export ppn=`printf %02g $ppn`
		$current_dir/totalQC/covupDCR.sh >$current_dir/logs/day_cpu_$ppn &
		ppn=${ppn#0}
		let ppn++
	done
prev_job_flag=0
while [ $prev_job_flag -lt 1 ]; do
	doneflagsnum=`ls $current_dir/doneflags/done_*|wc -l`
	if [ $doneflagsnum -eq $num_cpu ]; then
		prev_job_flag=1
		rm -rf $current_dir'/doneflags/done*'
	fi
done
fi

##START BINARY TO NETCDF PROCESS
if [ $flag_b2n -gt 0 ]; then
	ppn=0                       #Process Parallel computing Number
	while [ $ppn -lt $num_cpu ]; do
    		export ppn=`printf %02g $ppn`
		$current_dir/covupCFrad/covrun.sh >$current_dir/logs/b2n_cpu_$ppn&
		ppn=${ppn#0}
		let ppn++
	done
prev_job_flag=0
while [ $prev_job_flag -lt 1 ]; do
	doneflagsnum=`ls $current_dir/doneflags/done_*|wc -l`
	if [ $doneflagsnum -eq $num_cpu ]; then
		prev_job_flag=1
		rm -rf $current_dir'/doneflags/done*'
	fi
done
fi
$current_dir/webpagedisplay.sh
echo END
