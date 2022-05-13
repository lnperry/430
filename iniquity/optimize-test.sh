make clean > /dev/null 2>&1
make > /dev/null
make "$1.s" > /dev/null
racket -t compile-file-optimize.rkt -m "$1.rkt" > "$1Optimized.s"
make "$1.run" > /dev/null
make "$1Optimized.run" > /dev/null
echo "Generating reports for 1000 executions of example.run and exampleOptimized.run ..."
sudo perf stat -r1000 ./"$1.run" > /dev/null
sudo perf stat -r1000 ./"$1Optimized.run" > /dev/null
make clean
