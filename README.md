# µcomp-lang

This project was developed as the final assignment for the LCI course at the University of Pisa.
The task was to build a simple compiler for a C-style toy language, called µcomp. More specifics may be found [here.](https://github.com/lillo/compiler-course-unipi/tree/main/mcomp-lang)

## Requirement to build the code
The code requires:
* OCaml >= 4.12.0
* Menhir >= 20210419
* ppx_deriving >= 5.2
* LLVM >= 10

You can install the required dependencies via `opam`
```sh
$ opam install menhir ppx_deriving
```
[Here](https://github.com/ocaml-ppx/ppx_deriving), you can read the documentation of `ppx_deriving`.

## Building the code and running the tests
Typing `make` will generate the compiler executable `bin/mcompc.exe` and testing programs for all single passage such as `test/codegen_test.exe` etc:
```
$ make
```

To clean-up the folder, run:
```
$ make clean
```

To test the compiler you can use the files in `test/samples` directory, for example
```
$ dune exec bin/mcompc.exe -- samples/test-hello.mc
```
or 
```
$ dune exec test/codegen_test.exe -- samples/test-hello.mc
```
You can also use the bash script `testall.sh` to test the result of your compilation. 
Note that this script should be adapted to your local machine.
