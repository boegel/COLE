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

benchmark="n-body"
param=5000000
#param=500000

# compile benchmark using given flags
echo -n "... compiling $benchmark " 1>&2
/usr/bin/time -f "%e" ghc -fforce-recomp --make -XBangPatterns $COLE_OPTFLAGS $benchmark &> /tmp/comp.out
if [ $? -eq 0 ]
then
    comp_time=`cat /tmp/comp.out | egrep -v "] Compiling|Linking"`
	# execute compiled benchmark
    echo -n "... executing $benchmark $param " 1>&2
	exec_time=`/usr/bin/time -f "%e" ./$benchmark $param 2>&1 > /dev/null`

	if [ $? -eq 0 ]
	then
		# cleanup
		rm -f $benchmark ${benchmark}.hi ${benchmark}.o

        echo "... DONE!" 1>&2

		# return results
		echo "Just $exec_time"
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
