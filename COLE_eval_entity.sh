#!/bin/bash

# Example evaluation script for COLE.
# (see http://github.com/kehoste/COLE)
#
# $COLE_OPTFLAGS is set by COLE with optimization flags to use
# $COLE_EXPERIMENT_COUNT indicates how many times an experiments should be timed
#
# (only) evaluation result is printed to stdout, prefixed by 'Just'
# 'Nothing' is printed in case compilation or execution failed
#
# expected format: Just ([0.1,0.2,0.3],[4.0,5.0,6.0])
#
# progress of evaluation script is printed to stderr (ignored by COLE)

TMP_COMP=/tmp/comp.out
TMP_EXEC=/tmp/exec.out

benchmark="n-body"
#param=5000000
param=500000

# compile benchmark using given flags
echo -n "... compiling ($COLE_EXPERIMENT_COUNT times) " 1>&2
rm -f $TMP_COMP
for((i=0; i < $COLE_EXPERIMENT_COUNT; i++)) {
    /usr/bin/time -f "%e" -o $TMP_COMP -a -- ghc -fforce-recomp --make -XBangPatterns $COLE_OPTFLAGS $benchmark 2> /tmp/tmp.out > /dev/null
    comp_ok=$?
    # show compilation errors (if any)
    cat /tmp/tmp.out | egrep -v "] Compiling|Linking"
    rm -f /tmp/tmp.out
    if [ $comp_ok -ne 0 ]
    then
        break
    fi
}
if [ $comp_ok -eq 0 ]
then
    comp_times=`cat $TMP_COMP | egrep -v "] Compiling|Linking" | tr '\n' ',' | sed 's/^\(.*\),$/[[\1]]/'g`
    
	# execute compiled benchmark
    echo -n "... executing $benchmark $param ($COLE_EXPERIMENT_COUNT times) " 1>&2
    rm -f $TMP_EXEC
    for((i=0; i < $COLE_EXPERIMENT_COUNT; i++)) {
	    /usr/bin/time -f "%e" -o $TMP_EXEC -a ./$benchmark $param > /dev/null
        exec_ok=$?
        if [ $exec_ok -ne 0 ]
        then
            break
        fi
    }
    exec_times=`cat $TMP_EXEC | tr '\n' ',' | sed 's/^\(.*\),$/[[\1]]/'g`

	if [ $? -eq 0 ]
	then
		# cleanup
		rm -f $benchmark ${benchmark}.hi ${benchmark}.o

        echo "... DONE!" 1>&2

		# return results
		echo "Just ($comp_times,$exec_times)"
	else
		# execution failed => no result
		echo "Execution failed:" 1>&2
        cat $TMP_EXEC 2>&1
		echo "Nothing"
	fi
else	
	# compilation failed => no result
    echo "Compilation failed: " 1>&2
	cat /tmp/comp.out 1>&2
	echo "Nothing"
fi
rm -f $TMP_COMP $TMP_EXEC
