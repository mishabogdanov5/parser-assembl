AS = riscv64-unknown-elf-as
LD = riscv64-unknown-elf-ld
QEMU = qemu-riscv64
FLAG = -march=rv64gc -o
TARGET = file

build:
	dune build
test:
	dune runtest
clean: 
	dune clean
test_ass:
	$(AS) $(FLAG) $(TARGET).o $(TARGET).s
	$(LD) -o $(TARGET) $(TARGET).o
	$(QEMU) $(TARGET)

clean_ass:
	rm $(TARGET) 
	rm $(TARGET).o

.PHONY: test clean build