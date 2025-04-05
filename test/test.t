$Prog tests
  $ echo "fun f x y z = x + y + z" > expr1.txt
  $ echo "fun main = f 2 3 4" >> expr1.txt
  $ ../bin/main.exe expr1.txt
  $ riscv64-unknown-elf-as -march=rv64gc output.s -o expr1.o
  $ riscv64-unknown-elf-as -march=rv64gc rv64_runtime.s -o rv64_runtime.o
  $ riscv64-unknown-elf-ld expr1.o rv64_runtime.o -o expr1.exe
  $ qemu-riscv64 expr1.exe
                                 9
