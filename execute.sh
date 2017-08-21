#!/bin/sh
if [ $flag_refn -gt 0 ]; then 
	echo start refine file directory process at cpu$ppn
	$current_dir/refinedir.sh           # Refine the path of each files
else
	echo skip refine file directory process
fi

if [ $flag_ceil -gt 0 ]; then 
	echo start ceilometer process at cpu$ppn
	$current_dir/ceil/ceilrun.sh            # make CFradial files of ceilometer data
else
	echo skip ceilometer process
fi

if [ $flag_b2n -gt 0 ]; then 
	echo start covupCFrad process at cpu$ppn
	$current_dir/covupCFrad/covrun.sh      # make CFradial files of each cloud radar observation files
else
	echo skip process
fi

if [ $flag_day -gt 0 ]; then 
	echo start totalQC process at cpu$ppn
	$current_dir/totalQC/covupDCR.sh         # Integrate files daily
else
	echo skip totalQC process
fi

if [ $flag_plot -gt 0 ]; then 
	echo start web process at cpu$ppn
	$current_dir/plot.sh			# plot 
else
	echo skip web process
fi

if [ $flag_web -gt 0 ]; then 
	echo start web process at cpu$ppn
	$current_dir/webpagedisplay.sh		#readdress figures for webpage display
else
	echo skip web process
fi
