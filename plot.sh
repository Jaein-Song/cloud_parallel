#!/bin/bash
time=`date`
echo start plotting at $time
$filelist=(`cat $current_dir/filelist`)
filenumber=${#filelist[*]}
i=${ppn#0}
while [ $i -lt $filenumber ]; do
	Fig_File=${filelist[$i]}
	Fig_File_out=${Fig_File/$CF_dir\/DAILYMEAN/$Fig_dir}
	Fulldir=${Fig_File_out%/*cfradial}
	Fig_File_out=${Fig_File_out%.*radial}
	figtitle=${Fig_File_out##/*/}
	outfile=$Fig_File_out'.'$extend
	ofn=`ls $outfile |wc -l`
	if [ $ofn -lt 2 ]; then
		mkdir -p $Fulldir
		export 	Fig_File=$Fig_File
		export 	Fig_File_out=$Fig_File_out
		export	figtitle=$figtitle
		ncl $current_dir/CRcontourplot_2b2.ncl
	fi
	i=`expr $i + $num_cpu`
done
ppn=`printf %02g $ppn`
echo '1'>$current_dir/doneflags/cpustat_$ppn
