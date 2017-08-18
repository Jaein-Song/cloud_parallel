#!/bin/bash
       	export NCARG_ROOT=/home/frldata/util/ncl
       	export PATH=$PATH:$NCARG_ROOT
	export ncl=$NCARG_ROOT/bin/ncl
#echo $current_dir
#echo $NCARG_ROOT
#echo $PATH
#ncl -V
time=`date`
echo start plotting at $time
filelist=(`ls $CF_dir/DAILYMEAN/2*/*.cfradial`)
filenumber=${#filelist[*]}
i=0
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
	let i++
done
