BUILD_DIR := $(shell if [ ! -d "build" ]; \
				   then `mkdir -p "build"`; fi; \
				   echo "build")

MNT_DIR := $(shell if [ ! -d "/mnt/floppy" ]; \
				   then `sudo mkdir -p "/mnt/floppy"`; fi; \
				   echo "/mnt/floppy")

PROJ	:= MBCore_OS
EMPTY	:=
SPACE	:= $(EMPTY) $(EMPTY)
SLASH	:= /

V       := @

CC 	:= gcc
CFLAGS  := -m32 -nostdlib -nostdinc -fno-builtin -fno-stack-protector -ffreestanding
CFLAGS	+= -nostartfiles -nodefaultlibs -fno-PIC -Wall -ggdb -gstabs -Wall -Wextra -Werror -c

AS	:= nasm
ASFLAGS := -f elf

LD      := ld
LDFLAGS	:= -m elf_i386 -nostdlib

OBJCOPY := objcopy
OBJDUMP := objdump

COPY	:= cp -f
MKDIR   := mkdir -p
RMDIR   := rmdir
MV	:= mv
RM	:= rm -f
AWK	:= awk
SED	:= sed
SH	:= sh
TR	:= tr
TOUCH	:= touch -c

BOOTSECT  := $(BUILD_DIR)/boot/bootsect
LOADER	  := $(BUILD_DIR)/boot/loader.bin

KERNEL	  := $(BUILD_DIR)/kernel/kernel.sys

BOOTSIGN  := $(BUILD_DIR)/tools/bootsign

DISK_IMG  := $(BUILD_DIR)/disk.img

all: Image

Image: $(BOOTSECT) $(LOADER) $(KERNEL)

$(BOOTSECT): $(BOOTSIGN)
	make -C boot
	sudo $(BOOTSIGN) $(BOOTSECT) $(BOOTSECT)

$(BOOTSIGN):
	make -C tools

$(LOADER):
	make -C boot

$(KERNEL):
	make -C kernel

disk: Image
	dd if=/dev/zero of=$(DISK_IMG) bs=512 count=2880
	dd if=$(BOOTSECT) of=$(DISK_IMG) bs=512 seek=0 count=1 conv=notrunc
	sudo mount -o loop $(DISK_IMG) $(MNT_DIR)
	sudo cp -fv $(LOADER) $(MNT_DIR)
	sudo cp -fv $(KERNEL) $(MNT_DIR)
	sudo umount $(MNT_DIR)
	@echo "Success!"

bochs:
	bochs -f bochsrc

clean:
	make -C boot clean
	make -C tools clean
	make -C kernel clean
	$(RM) $(DISK_IMG)
	$(RMDIR) $(BUILD_DIR)
	sudo $(RMDIR) $(MNT_DIR)
