Kaleidoscope
============

Implementing a language with LLVM
## Compile
clang++ -g ./src/runtime.cpp `llvm-config --cppflags --ldflags --libs core jit native` -O3 -o kaleidoscope
