#!/bin/bash

set -e

MINCC="./mincc"
TEST_DIR="tests"
ASM_DIR="build"
BIN_DIR="build"

mkdir -p "$ASM_DIR" "$BIN_DIR"

for src in "$TEST_DIR"/*.minLang; do
    base=$(basename "$src" .minLang)
    asm="$ASM_DIR/$base.asm"
    obj="$ASM_DIR/$base.o"
    bin="$BIN_DIR/$base.out"

    echo "🛠️ Compiling $src..."
    $MINCC -o "$asm" -c "$src" -a nasm

    echo "🛠️ Assembling $asm..."
    nasm -f elf64 "$asm" -o "$obj"

    echo "🛠️ Linking $obj..."
    gcc "$obj" -o "$bin"

    echo "🚀 Running $bin..."
    "$bin"
    if [ $? -eq 0 ]; then
        echo "✅ Test $base: PASS"
    else
        echo "❌ Test $base: FAIL"
    fi
done