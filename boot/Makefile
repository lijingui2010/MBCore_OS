# Makefile for boot
BUILD_DIR := $(shell if [ ! -d "../build/boot" ]; \
				   then `mkdir -p "../build/boot"`; fi; \
				   echo "../build/boot")
# Programs, flags, etc.
AS	:= nasm
ASFLAGS	:= -I include/
ASFLAGS += -f elf

ASFBLAGS := -I include/
ASFBLAGS += -f bin

LD      := ld
LDFLAGS	:= -nostdlib

OBJCOPY := objcopy

RM 	:= rm -f
RMDIR   := rmdir

# This Program
TARGET	:= $(BUILD_DIR)/bootsect $(BUILD_DIR)/loader.bin

# All Phony Targets
.PHONY : all clean

# Default starting position
all : $(TARGET)

# $(BUILD_DIR)/bootsect
$(BUILD_DIR)/bootsect: $(BUILD_DIR)/bootsect.o
	$(OBJCOPY) -S -O binary $< $@

$(BUILD_DIR)/bootsect.o: $(BUILD_DIR)/bootsect.elf
	$(LD) $(LDFLAGS) -T bootsect.lds -o $@ $^

$(BUILD_DIR)/bootsect.elf: bootsect.asm
	$(AS) $(ASFLAGS) -o $@ $<

# $(BUILD_DIR)/loader.bin
$(BUILD_DIR)/loader.bin: loader.asm include/Floppy16.inc include/FAT12.inc include/loader.inc
	$(AS) $(ASFBLAGS) -o $@ $<

clean :
	$(RM) $(BUILD_DIR)/*
	$(RMDIR) $(BUILD_DIR)
