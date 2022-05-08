# ezc

A minimalist C compiler.

This program uses [asmorg](https://github.com/driverfury/asmorg) assembler.

## How it works?

1. It takes a single C source file as input.

2. It converts the source code into a stream of tokens.

3. The stream of tokens is then parsed (ANSI C90 syntax rules are applied). An AST (Abstract Syntax Tree) is generated.

4. It does a semantic check (type checking, type inference, symbols checking, ...) on the AST.

5. The AST is translated into an intermediate language (IR-C [[1](https://ls12-www.cs.tu-dortmund.de/daes/media/documents/publications/downloads/2003-samosIII.pdf)] in our case). Since our intermediate language is a subset of the ANSI C90, we can use a subset of the main AST components.

6. The IR-C AST is translated into x86 assembly code.

7. The x86 assembly code is compiled into a final statically linked executable ELF file by the [asmorg](https://github.com/driverfury/asmorg) assembler.

## Sources
[1] [https://ls12-www.cs.tu-dortmund.de/daes/media/documents/publications/downloads/2003-samosIII.pdf](https://ls12-www.cs.tu-dortmund.de/daes/media/documents/publications/downloads/2003-samosIII.pdf)
