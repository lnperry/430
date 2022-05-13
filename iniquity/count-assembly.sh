make clean > /dev/null 2>&1
make > /dev/null
make "$1.s" > /dev/null
racket -t compile-file-optimize.rkt -m "$1.rkt" > "$1Optimized.s"
make "$1.run" > /dev/null
make "$1Optimized.run" > /dev/null
echo "Assembly instructions before optimizations:\t$(wc -l < $1.s)"
echo "Assembly instructions after optimizations:\t$(wc -l < $1Optimized.s)"

