#!/bin/bash
sleep 0.5
pfid=`pgrep $task_name`
echo $task_name $ppn
pfid=0
flag=0
taskset -pc $ppn $pfid

#while [ $flag -lt 1 ]; do
#	pfid=`pgrep $task_name`
#	if [ $pfid -gt 0 ]; then
#		echo 'before'
#		echo `taskset -pc $pfid`
#		taskset -pc $ppn $pfid
#		flag=1
#	else
#		flag=0
#		echo fail $pfid $flag
#	fi	
#done
#
