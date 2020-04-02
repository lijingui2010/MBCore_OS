# Makefile for boot
BUILD_DIR := $(shell if [ ! -d "../build/kernel" ]; \
				   then `mkdir -p "../build/kernel"`; fi; \
				   echo "../build/kernel")
# Programs, flags, etc.
AS	:= nasm
ASFLAGS	:= -I include/
ASFLAGS += -f elf

ASFBLAGS := -I ../boot/include/
ASFBLAGS += -f bin

LD      := ld
LDFLAGS	:= -nostdlib

OBJCOPY := objcopy

RM 	:= rm -f
RMDIR   := rmdir

# This Program
TARGET	:= $(BUILD_DIR)/kernel.sys

# All Phony Targets
.PHONY : all clean

# Default starting position
all : $(TARGET)

# $(BUILD_DIR)/loader.bin
$(BUILD_DIR)/kernel.sys: kernel.asm
	$(AS) $(ASFBLAGS) -o $@ $<

clean :
	$(RM) $(BUILD_DIR)/*
	$(RMDIR) $(BUILD_DIR)
