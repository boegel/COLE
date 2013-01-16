#!/bin/bash

# Example evaluation script for COLE.
# (see http://github.com/kehoste/COLE)
#
# $COLE_OPTFLAGS is set by COLE with optimization flags to use
#
# (only) evaluation result is printed to stdout, prefixed by 'Just'
# 'Nothing' is printed in case compilation or execution failed
#
# progress of evaluation script is printed to stderr (ignored by COLE)

if [ -z "$COLE_OPTFLAGS" ]
then
    echo "Environment variable COLE_OPTFLAGS not found!"
    exit 1
fi

benchmark="n-body"
#param=5000000
param=500000

# fetch evaluation result for O0
base_result_file=COLE_base_result_${benchmark}-${param}.txt
if [ -f $base_result_file ] || [ "$COLE_OPTFLAGS" == "-O0" ]
then
     if [ "$COLE_OPTFLAGS" != "-O0" ]
     then
         # fetch base result
         base_comp_time=`cat $base_result_file | sed 's/Just (\([0-9.]*\),[0-9.]*)/\1/g'`
         base_exec_time=`cat $base_result_file | sed 's/Just ([0-9.]*,\([0-9.]*\))/\1/g'`
     fi
else
    # this will make COLE crash due to a parsing error -- good!
    echo "Base result is missing! (see $base_result_file)"
    exit 1
fi

# compile benchmark using given flags
echo -n "... compiling " 1>&2
/usr/bin/time -f "%e" ghc -fforce-recomp --make -XBangPatterns $COLE_OPTFLAGS $benchmark &> /tmp/comp.out
if [ $? -eq 0 ]
then
    comp_time=`cat /tmp/comp.out | egrep -v "] Compiling|Linking"`
	# execute compiled benchmark
    echo -n "(comp. time: $comp_time) ... executing $benchmark $param " 1>&2
	exec_time=`/usr/bin/time -f "%e" ./$benchmark $param 2>&1 > /dev/null`

	if [ $? -eq 0 ]
	then
		# cleanup
		rm -f $benchmark ${benchmark}.hi ${benchmark}.o

        echo "(exec. time: $exec_time) ... DONE!" 1>&2

		# return results
        if [ "$COLE_OPTFLAGS" == "-O0" ]
        then
		     echo "Just ($comp_time,$exec_time)"
        else
             comp_cost=`echo "$base_comp_time $comp_time" | awk '{print $2/$1;}'`
             speedup=`echo "$base_exec_time $exec_time" | awk '{print $1/$2;}'`
             echo "Just ($comp_cost,$speedup)"
        fi
	else
		# execution failed => no result
		echo "Execution failed: $exec_time" 1>&2
		echo "Nothing"
	fi
else	
	# compilation failed => no result
        echo "Compilation failed: " 1>&2
	cat /tmp/comp.out 1>&2
	echo "Nothing"
fi
rm -f /tmp/comp.out
