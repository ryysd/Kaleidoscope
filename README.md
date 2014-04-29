Kaleidoscope
============

Implementing a language with LLVM
## Compile
clang++ -g ./src/runtime.cpp \`llvm-config --cppflags --ldflags --libs core jit native\` -O3 -o kaleidoscope

## Usage
`./kaleidoscope` Run as interpretor
### Options
`-i=<file_name>` Read program from \<file_name\>

`-o=<file_name>` Write result to \<file_name\>

`--dump-enabled` Dump LLVM IR

`--opt-enabled` Enable optimization

`--dump-only` Dump LLVM IR without execution