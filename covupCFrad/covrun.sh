#!/bin/bash
#This is made by Jae In Song, NIMS, 2017
#This shell script is for execute the total program, following the stage below:
#0. Initialize: remove the running file
#1. set NETCDF path
#2. list up the file need to coverz up, output path is set at this stage
#3. compile CFradial file using fortran code
#4. run the compiled program
rm -rf $current_dir/covupCFrad/ifl$ppn
rm -rf $current_dir/covupCFrad/ofl$ppn
rm -rf $current_dir/covupCFrad/ofl14$ppn
rm -rf $current_dir/covupCFrad/ofl15$ppn
echo "start compile"
compileresult=`ls $current_dir/covupCFrad/frun$ppn |wc -l`
if [ $compileresult -gt 0 ]; then
chmod +x $current_dir/covupCFrad/frun$ppn
echo "start running"
sh $current_dir/covupCFrad/flist.sh
else
echo 'compile error'
fi
#$current_dir/covupCFrad/frun
echo "END"
#rm $current_dir/covupCFrad/*fl  
#rm $current_dir/covupCFrad/ofl1*
