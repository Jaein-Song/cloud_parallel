#!/bin/bash
#This is made by Jae In Song, Mar 17, 2017
#This shell script is for execute the total program, following the stage below:
#1. Initialize: remove the running file, set base directory, and input directory list, NETCDF setting, output file name and directory
#2. Get input files
#3. Make input file list
#4. Set Variable, Parmaeter, output file name
#5. compile
#6. Run
#7. clean up

#########SWITCH#####################
#1 for produce, 0 for not produce
flag_noqco=1
flag_noqcf=1
flag_qc14o=1
flag_qc15o=1
flag_qc17o=1
flag_qc172o=1

#1. Initialize
rm $current_dir/totalQC/ifl

bdl=$CF_dir'/DAILYMEAN/' #output file bse directory
#determine ymd
dlDn=${#dlD[*]}
dlCn=${#dlD[*]}
dl14n=${#dlD[*]}
dl15n=${#dlD[*]}
i=0
i=${ppn#0}

while [ $i -lt $dlDn ]; do
#2. Get input files
    iflDn=(`ls ${dlD[$i]}/*00__THI*.cfradial |wc -l`) #list of input file of the certain day
    iflCn=(`ls ${dlC[$i]}/*00__THI*.cfradial |wc -l`) #list of input file of the certain day
    ifl14n=(`ls ${dl14[$i]}/*00__THI*QC14.cfradial |wc -l`) #list of input file of the certain day
    ifl15n=(`ls ${dl15[$i]}/*00__THI*QC15.cfradial |wc -l`) #list of input file of the certain day
    if [ $flag_noqco -eq 1 ]; then
    if [ $iflDn -gt 10 ]; then
        fl=(`ls ${dlD[$i]}/*00__THI*.cfradial`) #list of input file of the certain day
        ymd=${fl[1]##/*HMBR_BS_} #Year, Month, Date
        ymd=${ymd:0:8}
        doy=`date -d $ymd +%j`
        y=${ymd:0:4}
	echo 'fl:' ${fl[$i]}
	echo $doy $ymd
        ofd=$bdl$y/  #output file directory
	#ofd=./
	ofdn=`ls -d $ofd |wc -l`
        if [ $ofdn -lt 1 ]; then
                mkdir -p $ofd
        fi
        ofn=$ofd'NCIO_CLD_noQC_NFT__daily_'$doy'_'$ymd.cfradial 
        ofnn=`ls $ofn |wc -l`
        if [ $ofnn -lt 1 ]; then
        j=0
        rm $current_dir/totalQC/ifl$ppn
        echo 0 > $current_dir/totalQC/ifl$ppn  #QC TYPE: 00-noQC,noFIlter
        echo $ofn >> $current_dir/totalQC/ifl$ppn
        echo $iflDn >> $current_dir/totalQC/ifl$ppn 
                while [ $j -lt $iflDn ]; do
                        echo ${fl[j]} >> $current_dir/totalQC/ifl$ppn 
                        let j++
                done
        echo 'EOF' >>$current_dir/totalQC/ifl$ppn
	export task_name=crun$ppn
	$current_dir/cpu_proc.sh >> $current_dir/logs/cpulog_$taskname &
        $current_dir/totalQC/crun$ppn
        fi
    fi
    fi

    if [ $flag_noqcf -eq 1 ]; then
    if [ $iflCn -gt 10 ]; then
        fl=(`ls ${dlC[$i]}/*00__THI*.cfradial`) #list of input file of the certain day
        ymd=${fl[1]##/*HMBR_BS_} #Year, Month, Date
        ymd=${ymd:0:8}
        doy=`date -d $ymd +%j`
        y=${ymd:0:4}
        ofd=$bdl$y/  #output file directory
	#ofd=./
	ofdn=`ls -d $ofd |wc -l`
        if [ $ofdn -lt 1 ]; then
                mkdir -p $ofd
        fi
        ofn=$ofd'NCIO_CLD_noQC_FTD__daily_'$doy'_'$ymd.cfradial 
        ofnn=`ls $ofn |wc -l`
        if [ $ofnn -lt 1 ]; then
        j=0
        rm $current_dir/totalQC/ifl$ppn
        echo 1 > $current_dir/totalQC/ifl$ppn #QC TYPE: 1-noQC,FIltered
        echo $ofn >> $current_dir/totalQC/ifl$ppn 
        echo $iflCn >> $current_dir/totalQC/ifl$ppn 
                while [ $j -lt $iflCn ]; do
                        echo ${fl[j]} >> $current_dir/totalQC/ifl$ppn 
                        let j++
                done
        echo 'EOF' >>$current_dir/totalQC/ifl$ppn
	export task_name=crun$ppn
	$current_dir/cpu_proc.sh >> $current_dir/logs/cpulog_$taskname &
        $current_dir/totalQC/crun$ppn
        fi
    fi
    fi

    if [ $flag_qc14o -eq 1 ]; then
    if [ $ifl14n -gt 10 ]; then
        fl=(`ls ${dl14[$i]}/*00__THI*QC14.cfradial`) #list of input file of the certain day
        ymd=${fl[1]##/*HMBR_BS_} #Year, Month, Date
        ymd=${ymd:0:8}
        doy=`date -d $ymd +%j`
        y=${ymd:0:4}
        ofd=$bdl$y/  #outaput file directory
	#ofd=./
	ofdn=`ls -d $ofd |wc -l`
        if [ $ofdn -lt 1 ]; then
                mkdir -p $ofd
        fi
        ofn=$ofd'NCIO_CLD_QC14_FTD__daily_'$doy'_'$ymd.cfradial 
        ofnn=`ls $ofn |wc -l`
        if [ $ofnn -lt 1 ]; then
        j=0
        rm $current_dir/totalQC/ifl$ppn
        echo 14 > $current_dir/totalQC/ifl$ppn  #QC TYPE: 14-QC14,FIltered
        echo $ofn >> $current_dir/totalQC/ifl$ppn 
        echo $iflDn >> $current_dir/totalQC/ifl$ppn 
                while [ $j -lt $ifl14n ]; do
                        echo ${fl[j]} >> $current_dir/totalQC/ifl$ppn 
                        let j++
                done
	#sleep  0.2s
        echo 'EOF' >>$current_dir/totalQC/ifl$ppn
	export task_name=crun$ppn
	$current_dir/cpu_proc.sh >> $current_dir/logs/cpulog_$taskname &
        $current_dir/totalQC/crun$ppn
        fi
    fi
    fi

    if [ $flag_qc15o -eq 1 ]; then
    if [ $ifl15n -gt 10 ]; then
        fl=(`ls ${dl15[$i]}/*00__THI*QC15.cfradial`) #list of input file of the certain day
        ymd=${fl[1]##/*HMBR_BS_} #Year, Month, Date
        ymd=${ymd:0:8}
        doy=`date -d $ymd +%j`
        y=${ymd:0:4}
        ofd=$bdl$y/  #outaput file directory
	#ofd=./
	ofdn=`ls -d $ofd |wc -l`
        if [ $ofdn -lt 1 ]; then
                mkdir -p $ofd
        fi
        ofn=$ofd'NCIO_CLD_QC15_FTD__daily_'$doy'_'$ymd.cfradial 
        ofnn=`ls $ofn |wc -l`
        if [ $ofnn -lt 1 ]; then
        j=0
        rm $current_dir/totalQC/ifl$ppn
        echo 15 > $current_dir/totalQC/ifl$ppn  #QC TYPE: 15-QC15,FIltered
        echo $ofn >> $current_dir/totalQC/ifl$ppn
        echo $iflDn >> $current_dir/totalQC/ifl$ppn   
                while [ $j -lt $ifl15n ]; do
                        echo ${fl[j]} >> $current_dir/totalQC/ifl$ppn
                        let j++
                done
        echo 'EOF' >>$current_dir/totalQC/ifl$ppn
	export task_name=crun$ppn
	$current_dir/cpu_proc.sh >> $current_dir/logs/cpulog_$taskname &
        $current_dir/totalQC/crun$ppn
        fi
    fi
    fi

    if [ $flag_qc17o -eq 1 ]; then
    if [ $iflDn -gt 10 ]; then
        fl=(`ls ${dlD[$i]}/*00__THI*.cfradial`) #list of input file of the certain day
        ymd=${fl[1]##/*HMBR_BS_} #Year, Month, Date
        ymd=${ymd:0:8}
        y=${ymd:0:4}
        doy=`date -d $ymd +%j`
        ofd=$bdl$y/  #output file directory
	#ofd=./
	ofdn=`ls -d $ofd |wc -l`
        if [ $ofdn -lt 1 ]; then
                mkdir -p $ofd
        fi
        ofn=$ofd'NCIO_CLD_QC17_NFT__daily_'$doy'_'$ymd.cfradial 
        ofnn=`ls $ofn |wc -l`
        if [ $ofnn -lt 1 ]; then
        j=0
        rm $current_dir/totalQC/ifl$ppn
        echo 17 > $current_dir/totalQC/ifl$ppn  #QC TYPE: 17-QC17,FIltered
        echo $ofn >> $current_dir/totalQC/ifl$ppn 
        echo $iflDn >> $current_dir/totalQC/ifl$ppn 
                while [ $j -lt $iflDn ]; do
                        echo ${fl[j]} >> $current_dir/totalQC/ifl$ppn 
                        let j++
                done
        echo 'EOF' >>$current_dir/totalQC/ifl$ppn
	export task_name=crun$ppn
	$current_dir/cpu_proc.sh >> $current_dir/logs/cpulog_$taskname &
        $current_dir/totalQC/crun$ppn
        fi
    fi
    fi

    if [ $flag_qc172o -eq 1 ]; then
    if [ $iflDn -gt 10 ]; then
        ceiln=`ls $ceil_cdf_dir/$y/*LV3*$ymd.cdf |wc -l`
        if [ $ceiln -gt 0 ]; then
                echo `ls $ceil_cdf_dir/$y/*LV3*$ymd.cdf` >$current_dir/totalQC/cfl$ppn
                fl=(`ls ${dlD[$i]}/*00__THI*.cfradial`) #list of input file of the certain day
        ymd=${fl[1]##/*HMBR_BS_} #Year, Month, Date
        ymd=${ymd:0:8}
	echo $ymd $fl[1]
        doy=`date -d $ymd +%j`
        y=${ymd:0:4}
                ofd=$bdl$y/  #output file directory
                #ofd=./
                ofdn=`ls -d $ofd |wc -l`
                if [ $ofdn -lt 1 ]; then
                        mkdir -p $ofd
                fi
                ofn=$ofd'NCIO_CLD_QC17_CIL__daily_'$doy'_'$ymd.cfradial 
                ofnn=`ls $ofn |wc -l`
                if [ $ofnn -lt 1 ]; then
                j=0
                rm $current_dir/totalQC/ifl$ppn
                echo 172 > $current_dir/totalQC/ifl$ppn  #QC TYPE: 17-QC17,FIltered
                #sleep  0.2s
                echo $ofn >> $current_dir/totalQC/ifl$ppn 
                #sleep  0.2s
                echo $iflDn >> $current_dir/totalQC/ifl$ppn 
                #sleep  0.2s
                        while [ $j -lt $iflDn ]; do
                                echo ${fl[j]} >> $current_dir/totalQC/ifl$ppn 
                                let j++
                        done
                #sleep  0.2s
                echo 'EOF' >>$current_dir/totalQC/ifl$ppn
	export task_name=crun$ppn
	$current_dir/cpu_proc.sh >> $current_dir/logs/cpulog_$taskname &
                $current_dir/totalQC/crun$ppn
                fi
        fi
    fi
    fi
    i=`expr $i + $cpu_num`
done
echo "END"
ppn=`printf %02g $ppn`
mkdir -p $current_dir/doneflags
cat <<end >$current_dir/doneflags/done_$ppn
done job at cpu no.$ppn process covupCFrad
end
