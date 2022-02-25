nasm -f elf64 -o $1.o $1.s
gcc -m64 -c -o main.o main.c
gcc main.o $1.o -o $1.run
echo "resulting assembly is:\n"
./$1.run
