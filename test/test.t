$Prog tests

  $ riscv64-unknown-elf-as -march=rv64gc rv64_runtime.s -o rv64_runtime.o
  $ echo "fun f x y z = x + y + z" > expr.txt
  $ echo "fun main = f 5 5 4" >> expr.txt
  $ ../bin/main.exe expr.txt
  $ riscv64-unknown-elf-as -march=rv64gc output.s -o expr.o
  $ riscv64-unknown-elf-ld expr.o rv64_runtime.o -o expr.exe
  $ qemu-riscv64 expr.exe
                                14
  $ echo "fun f x y z = x - y / z" > expr.txt
  $ echo "fun main = f 8 4 2" >> expr.txt
  $ ../bin/main.exe expr.txt
  $ riscv64-unknown-elf-as -march=rv64gc output.s -o expr.o
  $ riscv64-unknown-elf-ld expr.o rv64_runtime.o -o expr.exe
  $ qemu-riscv64 expr.exe
                                 6
  $ echo "fun f x y = x - y" > expr.txt
  $ echo "fun main = f 12 4" >> expr.txt
  $ ../bin/main.exe expr.txt
  $ riscv64-unknown-elf-as -march=rv64gc output.s -o expr.o
  $ riscv64-unknown-elf-ld expr.o rv64_runtime.o -o expr.exe
  $ qemu-riscv64 expr.exe
                                 8
  $ echo "fun f x y z v = (x - y) * v + z" > expr.txt
  $ echo "fun main = f 10 5 10 2" >> expr.txt
  $ ../bin/main.exe expr.txt
  $ riscv64-unknown-elf-as -march=rv64gc output.s -o expr.o
  $ riscv64-unknown-elf-ld expr.o rv64_runtime.o -o expr.exe
  $ qemu-riscv64 expr.exe
                                20

  $ echo "fun f x y z v u g h n = (x - 2 * (z + y)) / (h - u) + (n - g) * v" > expr.txt
  $ echo "fun main = f 44 3 5 2 4 1 8 1" >> expr.txt
  $ ../bin/main.exe expr.txt
  $ riscv64-unknown-elf-as -march=rv64gc output.s -o expr.o
  $ riscv64-unknown-elf-ld expr.o rv64_runtime.o -o expr.exe
  $ qemu-riscv64 expr.exe
                                 7
$assoc test
  $ echo "fun f x y z = x + y - z" > expr1.txt
  $ echo "fun main = f 3 5 6" >> expr1.txt
  $ ../bin/main.exe expr1.txt "right"
  $ riscv64-unknown-elf-as -march=rv64gc output.s -o expr1.o
  $ riscv64-unknown-elf-ld expr1.o rv64_runtime.o -o expr1.exe
  $ qemu-riscv64 expr1.exe
                                 4
  $ echo "fun f x y z = x + y - z" > expr1.txt
  $ echo "fun main = f 3 5 6" >> expr1.txt
  $ ../bin/main.exe expr1.txt
  $ riscv64-unknown-elf-as -march=rv64gc output.s -o expr1.o
  $ riscv64-unknown-elf-ld expr1.o rv64_runtime.o -o expr1.exe
  $ qemu-riscv64 expr1.exe
                                 2
$sli tests
  $ echo "fun f x y z = 4 * x + y + z" > expr1.txt
  $ echo "fun main = f 3 5 6" >> expr1.txt
  $ ../bin/main.exe expr1.txt
  $ riscv64-unknown-elf-as -march=rv64gc output.s -o expr1.o
  $ riscv64-unknown-elf-ld expr1.o rv64_runtime.o -o expr1.exe
  $ qemu-riscv64 expr1.exe
                                23
  $ echo "fun f x y z = 16 * x + y + z" > expr1.txt
  $ echo "fun main = f 3 5 6" >> expr1.txt
  $ ../bin/main.exe expr1.txt
  $ riscv64-unknown-elf-as -march=rv64gc output.s -o expr1.o
  $ riscv64-unknown-elf-ld expr1.o rv64_runtime.o -o expr1.exe
  $ qemu-riscv64 expr1.exe
                                59

lambda tests
  $ echo "(Lx.x) y" > lambda1.txt
  $ ../bin/main.exe lambda lambda1.txt
  (\x.x) y

  $ echo "Lx.Ly.x y" > lambda1.txt
  $ ../bin/main.exe lambda lambda1.txt
  (\x.(\y.x y))

