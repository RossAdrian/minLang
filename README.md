# 🧵 MinLang — A Minimalist Programming Language

MinLang is a compact, clean, and expressive programming language designed for educational, experimental, and low-overhead compiler development. With just a handful of core features, MinLang makes it easy to explore compiler design, code generation, and systems programming in OCaml.

This repository includes **`mincc.ml`**, a single-file compiler that transforms MinLang source code into assembly.

## 🔧 Language Features

MinLang draws inspiration from C, while stripping the syntax down to the essentials. Here's what the language supports:

- **Primitive types**: `int`, `char`, `ptr<T>` (pointers)
- **Variable declarations** with type inference (i.e. `let x = 0;`, optionally `let x: int = 0`)
- **Functions** with typed parameters and return values
- **Function calls**: Nested function calls result in undefined behavior
- **Basic expressions** and operators:
  - Binary: `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `=`
  - Unary: `!`, `*` (dereference), `&` (address-of)
- **Return statements**
- No runtime, no standard library—just minimal code and direct control


## 💻 Example

```minlang
fn square(x: int): int {
    return x * x;
}

fn main(): int {
    let a: int = 5;
    let b: int = square(a);
    return b;
}
```

## 🚀 Using the Compiler

The compiler is implemented in OCaml as a single file: `mincc.ml`. To build and run:

### 1️⃣ Build the Compiler

```bash
ocamlc -o mincc mincc.ml
```

### 2️⃣ Compile a MinLang Source File

To compile a *minLang* source file to an executable, run the
following script (x64 linux):

```bash
# Compile the source file to assembler code
./mincc -c input.min -o output.asm -a nasm

# Assemble assembly code to object file
nasm -felf64 output.asm -o output.o

# Link object file with C standard library
gcc -fno-pie -no-pie output.o -o Output
```

### Using a standard library

By default, *minLang* is linked with the *C standard library*
by linking with *gcc*. You can freely access library functions
by declaring function prototypes:

```minLang
fn printf(ptr<char>): int;
fn exit(int): int;

fn main(): int {
  printf("Hello World!\n");
  return exit(1);
}
```

### Parameters

- `-c <input>`: MinLang source file to compile
- `-o <output>`: Output assembly file
- `-a <asm>`: Assembler dialect
  - `riscv`: Generate *32bit RISC-V* code
  - `ir`: Dump *IR (Intermediate Representation)* code
  - `nasm`: Generate *64bit x64 NASM assembler* code

## 📦 Output

The compiler emits x86 assembly code with basic structure, suitable for further compilation via `nasm`, `gcc`, or similar tools.

## 🧪 Goals

MinLang is designed for:

- Learning compiler internals
- Playing with parsing, ASTs, and code generation in OCaml
- Keeping things as simple as possible—and nothing more

## 📚 License

This project is open source under the MIT License. See the `LICENSE` file for details.
