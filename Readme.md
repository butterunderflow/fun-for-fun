# Fun for Fun
![GitHub CI](https://github.com/butterunderflow/fun-for-fun/actions/workflows/ci.yml/badge.svg)

Welcome to **fun for fun**, a functional programming language compiler made for fun! 

## Build the project


To build the project, you need to install the dependencies using `opam`:
```sh
opam install . --deps-only --with-test --with-doc
```

Then, build the project using `dune`:

```sh
dune build
```

Finally, install the compiler and runtime. 
This will install the compiler `ff` and a compiler wrapper `ffw` in `~/.opam/<ocaml-version>/bin/`, 
and the runtime library in `~/.opam/<ocaml-version>/share/fun4fun/`.

```sh
dune install
```

## Hello World

Create a hello world program `hello.fun` to test if compiler installed successfully.

```OCaml
external println_str : string -> unit = "ff_builtin_println_str"

let _ = println_str "Hello World!"
```

### Compile and Run

1. Compile to C++ Source:

Use the previously installed compiler to generate a C++ source `hello.cpp` from `hello.fun`.
```sh
ff hello.fun -o hello.cpp
```


2. Compile C++ Source:

Use `g++` to compile generated c++ source and link it to runtime library.
```sh
g++ hello.cpp -I $(opam var share)/fun4fun $(opam var share)/fun4fun/libfun_rt.a -o hello
```

3. Run the Executable:

Execute the compiled program.
```
./hello
```

and check the output.

```
Hello World!
```

### Use Compiler Wrapper
Alternatively, you can use the compiler wrapper `ffw` to automatically call `g++`:

```sh
ffw hello.fun
```

This will generate an executable program `hello.fun.out` in current directly. 


Run it:

```
./hello.fun.out
```

and check the output:

```
Hello World!
```
