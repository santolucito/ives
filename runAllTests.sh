# Clean up old ramdisk
umount ./tmp
rm -rf ./tmp

# Create new ramdisk - this frees Ives from most of its IO
# bottleneck. All the temp files are written to RAM and
# never committed to disk. The only remaining overhead is
# the system calls, but to get around this would require
# substantial effort with much less improvement, I'd guess.
# We get 33% faster just by doing this. I doubt much more
# could be done without a near-rewrite.
mkdir -p ./tmp
mount -t tmpfs -o size=256m tmpfs ./tmp/

# Reset results dir
rm -rf ./test-results
mkdir -p ./test-results

# Gather testing data 
TESTS=(tests/benchmarks/*.hs)
COUNT=${#TESTS[@]}

longest=0
for word in "${TESTS[@]}"
do
    len=${#word}
    if (( len > longest ))
    then
        longest=$len
    fi
done

# Run the tests!
I=1
for TEST in "${TESTS[@]}"
do
    NDOTS=$(($longest - ${#TEST} + 3))
    NAME=`basename $TEST .hs`
    printf "[%02d/%02d] Testing %s " $I $COUNT $NAME
    printf '%*s' "$NDOTS" '' | tr ' ' .
    ./dist/build/Ives/Ives $TEST >./test-results/$NAME.stdout 2>./test-results/$NAME.stderr
    RESULT=`grep "synth in" test-results/$NAME.stdout | cut -d' ' -f 3`
    if [[ -z "${RESULT// }" ]]; then
        printf ' FAILED :(\n'
    else
        printf ' Done in %s\n' $RESULT
    fi
    I=$((I+1))
done
