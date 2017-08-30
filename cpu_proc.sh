#!/bin/bash
sleep 0.5
pfidn=`pgrep $task_name`
pfid=0
flag=0
sec=0
while [ $flag -lt 1 ]; do
	pfidn=`pgrep $task_name`
	pfid=`expr $pfid + $pfidn`
	if [ $pfid -gt 0 ]; then
		echo 'before'
		echo $task_name $ppn $pfid
		echo `taskset -pc $pfid`
		taskset -pc $ppn $pfid
		flag=1
	else
		sleep 1
		let sec++
		flag=0
	fi
	if [ $sec -gt 9 ]; then
		echo $task_name $ppn 
		flag=1
		echo fail $pfid $flag
	fi
done

