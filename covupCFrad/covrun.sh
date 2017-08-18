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
#echo ${FC:0:1}
#if [ ${FC:0:1} == 'g' ]; then
#	$FC -ffree-line-length-512 $current_dir/covupCFrad/mod_para.f $current_dir/covupCFrad/varmod.f90 $current_dir/covupCFrad/covup_data.f90 $current_dir/covupCFrad/importpdf.f90 $current_dir/covupCFrad/sub_head.f $current_dir/covupCFrad/sub_block.f $current_dir/covupCFrad/write_cfradial.f $current_dir/covupCFrad/qc14.f90 $current_dir/covupCFrad/qc15.f90 -I$NETCDF/include -L$NETCDF/lib -lnetcdf -lnetcdff -o $current_dir/covupCFrad/frun
#else
#fi
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
