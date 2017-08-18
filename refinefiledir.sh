#!/bin/bash
#Refine the file directories
#Set Cloud radar data paths

bdbc=$CLD_dir/BASEDATC          # Filtered file path
bdbd=$CLD_dir/BASEDATD          # Unfiltered file path
bdnc=$CF_dir/BASEDATC  # CFradial filtered out file path
bdnd=$CF_dir/BASEDATD  # CFradial unfiltered output file path

yc=`date +%y`
mc=`date +%m`
ddc=`date +%d`
dc=`expr $ddc - 1`
yc=`expr 20$yc`
if [ $dc -lt 10 ];then
dc=`expr 0$dc`
fi
#Starting date
y=$sy
mr=$sm
dr=01

while [ $y -le $yc ]; do
echo $y $yc
    if [ $y -eq $yc ]; then
        mx=$mc
    else
        mx=12
    fi
    echo $mx
    while [ $mr -le $mx ]; do
        m=`prntf "%02d $mr"`
        if [ $y$m -eq $yc$mc ]; then
            dx=$dc
        else
            dx=31
        fi

        while [ $dr -le $dx ]; do
            d=`printf "%02d" $dr`
            bdbcn=`ls $bdbc/$y$m/$y$m$d/*.OBS |wc -l`
    	    echo $bdbc/$y$m/$y$m$d/ $bdbcn
            bdbdn=`ls $bdbd/$y$m/$y$m$d/*.OBS |wc -l`
            bdncn=`ls $bdnc/$y$m/$y$m$d/*.cfradial |wc -l`
            bdndn=`ls $bdnd/$y$m/$y$m$d/*.cfradial |wc -l`
            if [ $bdbcn -gt 0 ]; then
                bdo=$bdbc
                bn=$bdbcn
                bd=$bdo/$y$m/$y$m$d
                bdl=(`ls $bd/*OBS`)
                fli=0
                while [ $fli -lt $bn ]; do
                    fn=${bdl[$fli]}
                    fnp=${fn##/*HMBR_BS_}
                    ymdf=${fnp:0:8}
                    if [ $ymdf != $y$m$d ]; then
                        mkdir -p $fn $bdo/${ymdf:0:6}/$ymdf/
                        mv $fn $bdo/${ymdf:0:6}/$ymdf/
                        echo "$ymdf"
                    fi
                    let fli++
                done
            fi
            if [ $bdbdn -gt 0 ]; then
                bdo=$bdbd
                bn=$bdbdn
                bd=$bdo/$y$m/$y$m$d
                bdl=(`ls $bd/*OBS`)
                fli=0
                while [ $fli -lt $bn ] ; do
                    fn=${bdl[$fli]}
                    fnp=${fn##/*HMBR_BS_}
                    ymdf=${fnp:0:8}
		            echo "ymdferr $ymdf ymd: $y$m$d"
                    if [ $ymdf != $y$m$d ]; then
                        mkdir -p $fn $bdo/${ymdf:0:6}/$ymdf/
                        mv $fn $bdo/${ymdf:0:6}/$ymdf/
                        echo "$ymdf"
                    fi
                    let fli++
                done
            fi
            if [ $bdncn -gt 0 ]; then
                bdo=$bdnc
                bn=$bdncn
                bd=$bdo/$y$m/$y$m$d
                bdl=(`ls $bd/*cfradial`)
                fli=0
                while [ $fli -lt $bn ]; do
                    fn=${bdl[$fli]}
                    fnp=${fn##/*HMBR_BS_}
                    ymdf=${fnp:0:8}
                    if [ $ymdf != $y$m$d ]; then
                        mkdir -p $fn $bdo/${ymdf:0:6}/$ymdf/
                        mv $fn $bdo/${ymdf:0:6}/$ymdf/
                        echo "$ymdf"
                    fi
                    let fli++
                done
            fi
            if [ $bdndn -gt 0 ]; then
                bdo=$bdnd
                bn=$bdndn
                bd=$bdo/$y$m/$y$m$d
                bdl=(`ls $bd/*cfradial`)
                fli=0
                while [ $fli -lt $bn ]; do
                    fn=${bdl[$fli]}
                    fnp=${fn##/*HMBR_BS_}
                    ymdf=${fnp:0:8}
                    if [ $ymdf != $y$m$d ]; then
                        mkdir -p $fn $bdo/${ymdf:0:6}/$ymdf/
                        mv $fn $bdo/${ymdf:0:6}/$ymdf/
                       echo "$ymdf transf"
                    fi
                    let fli++
                done
            fi

            let dr++
        done
        let mr++
    done
    let y++
done
