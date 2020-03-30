# Makefile for boot
BUILD_DIR := $(shell if [ ! -d "../build/tools" ]; \
				   then `mkdir -p "../build/tools"`; fi; \
				   echo "../build/tools")

# Programs, flags, etc.
CC 	:= gcc
CFLAGS  := -O2
CFLAGS	+= -Wall -ggdb -gstabs -Wall -Wextra -Werror

AS	:= nasm
ASFLAGS	:= -I include/
ASFLAGS += -f elf

LD      := ld
LDFLAGS	:= -nostdlib

OBJCOPY := objcopy

RM 	:= rm -f
RMDIR   := rmdir

# This Program
TARGET	:= $(BUILD_DIR)/bootsign

# Default starting position
all : $(TARGET)

# $(BUILD_DIR)/bootsign
$(BUILD_DIR)/bootsign: bootsign.c
	$(CC) $(CFLAGS) $< -o $@

clean :
	$(RM) $(BUILD_DIR)/*
	$(RMDIR) $(BUILD_DIR)
