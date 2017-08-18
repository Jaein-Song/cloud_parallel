#!/bin/sh
#sh ./refinedir.sh           # Refine the path of each files
#sh ./ceil/ceilrun.sh            # make CFradial files of ceilometer data
echo start covupCFrad of month $pmn at cpu $ppn
$current_dir/covupCFrad/covrun.sh      # make CFradial files of each cloud radar observation files
echo start totalQC of month $pmn at cpu $ppn
$current_dir/totalQC/covupDCR.sh         # Integrate files daily
#sh ./webpagedisplay.sh
