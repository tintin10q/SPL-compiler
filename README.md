# SPL

This repository contains a compiler for SPL (Simple Programming Language) for the 2023-2024 course Compiler Construction (NWI-IMC004).
Read the [full report here](./report/Compiler-Construction-Final-Report.pdf).


## Running the compiler

To run the compiler you need to instal [cabal](https://www.haskell.org/cabal/)
Once you have done that you can run the compiler with:


```shell
cabal run SPL examples/helloworld.spl
```

Get help with:

```shell
cabal run SPL -- --help
```

This will print:

```
SPL Compiler By Quinten Cabo
This compiler compiles the SPL programming language into ssm.
The first argument should be the filename of the .spl file to compile.
Possible further arguments:
--output (or -o) <filename>      default: output.ssm
--hide-info              Optional argument to hide info messages
--hide-warnings          Optional argument to hide warning messages
--skip-optimizer         Optional argument to skip the optimizer step
--help (or -h)           Print this help message
```

## Running the generated code

The code that is generated are ssm instructions for the [simple stack machine](https://webspace.science.uu.nl/~hage0101/SSM/index.html).
You can run the code using the ssm.jar in the ssm directory. 

```shell
ssm/ssm.sh --cli --file output.ssm  # linux
ssm/ssm.bat --cli --file output.ssm  # windows
```

This does require java to actually run the jar.

# SPL

SPL is a C-style language, but with the following main differences:

- Support for polymorphic data types. 
- Type inference
- No pointers
- Type safe operators, you get a type error with + on a chars for example
- Overloaded print and equality
- Arrays are quite different with the : (cons) operator and .tl and .hd access fields
- Variables should be defined at the top of a function.

Here are some examples of SPL:

## Hello world

There is no literal list syntax in SPL but there is the : (cons) operator. 

```c
main() : Void {
    print('H':'e':'l':'o':' ':'W':'o':'r':'d':[]); 
}
```

However, I added a literal string syntax that is parsed into cons syntax.

```c
main() : Void {
    print("Hello World"); 
}
```

## Conditions and loops

Just like c, SPL has if and else statements and while loops.

```c
// This is one line SPL comment
/* Multi  
line /* Nested */ comment */

countdown(x: Int) {
    Int zero = 0;
    while (x > zero) { // x is infered as Int 
        if (x == 5) {
            print("count is greater than 5\n");
        } else {
            print("Count is not greater than 5");
        }
        x = x - 1; 
    }
}

main() : Void {
    countdown(10); 
}
```

## Type inference

Because of the type inference you can actually leave out the types. 
This means that the following code:

```c
var a = 3;

foo(x) {
    var zero = 0;
    var list = [];
    if (x) {print(x);}
    return a:zero:list;
}

main() {
    foo(a > 10);
    return;
}
```

Will be inferred as:

```c
Int a = 3;

foo(x : Bool) : [Int] {
    Int zero = 0;
    [t] list = [];
    if (x) {print(x);}
    return a:zero:list;
}

main() : Void {
    foo(a > 10);
    return;
}
```

Here is an example of a program with multiple type errors:

```c
foo() {
    var list = [];
    3:list;
    return 'a':list; // Can not add a char anymore because list is inferred as [Int]
}

bar() {
    if (1) { // Not a boolean!
        return 3 == ([1] + 2); // Non matching types!
    }
}

fee(x) {
    if (x) {return 3;}
    return '3'; // Invalid return type!
}
```

This next program shows off tuple and list support as well as overloaded equals and printing.

```c
var q = 137:[];

main() {
    var aa = ("Equal!", q.hd + 1);
    var zz = ("-Equal!", q.hd);
    
    var bb = (aa, aa);
    var yy = (zz, zz);
    
    var cc = (bb, bb);
    var xx = (yy, yy);

    print("cc: "); print(cc); print(); // Print without arguments print a new line
    print("xx: "); print(xx); print("\ncc and xx are ");
    if (cc == xx) { print(bb.snd.fst); } else { print("Not Equal!"); }

    aa.snd = bb.snd.snd - 1; // Make them the same
    zz.fst = zz.fst.tl; 

    print("\n\ncc: "); print(cc); print(); 
    print("zz: ");   print(zz); print("\ncc and zz are ");
    if (cc == xx) { print(bb.snd.fst); } else { print("Not Equal!"); }
}
```

The type checked version of this program is:

```c
[Int] q = 137:[];

main() : Void {
    ([Char], Int) aa = ('E':'q':'u':'a':'l':'!':[], q.hd + 1);
    ([Char], Int) zz = ('-':'E':'q':'u':'a':'l':'!':[], q.hd);
    (([Char], Int), ([Char], Int)) bb = (aa, aa);
    (([Char], Int), ([Char], Int)) yy = (zz, zz);
    ((([Char], Int), ([Char], Int)), (([Char], Int), ([Char], Int))) cc = (bb, bb);
    ((([Char], Int), ([Char], Int)), (([Char], Int), ([Char], Int))) xx = (yy, yy);
    
    print('c':'c':':':' ':[]);
    print(cc);
    print();
    print('x':'x':':':' ':[]);
    print(xx);
    print('\n':'c':'c':' ':'a':'n':'d':' ':'x':'x':' ':'a':'r':'e':' ':[]);
    if (cc == xx) {
        print(bb.snd.fst);
    } else {
        print('N':'o':'t':' ':'E':'q':'u':'a':'l':'!':[]);
    }
    aa.snd = bb.snd.snd - 1;
    zz.fst = zz.fst.tl;
    print('\n':'\n':'c':'c':':':' ':[]);
    print(cc);
    print();
    print('z':'z':':':' ':[]);
    print(zz);
    print('\n':'c':'c':' ':'a':'n':'d':' ':'z':'z':' ':'a':'r':'e':' ':[]);
    if (cc == xx) {
        print(bb.snd.fst);
    } else {
        print('N':'o':'t':' ':'E':'q':'u':'a':'l':'!':[]);
    }
    return;
}
```

The output of this program is: 

```
cc: ((("Equal!", 138), ("Equal!", 138)), (("Equal!", 138), ("Equal!", 138)))
xx: ((("-Equal!", 137), ("-Equal!", 137)), (("-Equal!", 137), ("-Equal!", 137)))
cc and xx are Not Equal!

cc: ((("Equal!", 137), ("Equal!", 137)), (("Equal!", 137), ("Equal!", 137)))
xx: ((("Equal!", 137), ("Equal!", 137)), (("Equal!", 137), ("Equal!", 137)))
cc and xx are Equal!
```

Here is an example of polymorphic types:

```c 
id(value: a): a {
    return value;
}

tuple(v1: a, v2: b) : (a,b) {
    return (v1,v2);
}

main() {
    var foo = id(137);
    var bar = id(True);

    var foobar = tuple(foo,bar);

    print(foobar);
} 

```

Here `main` is typechecked as:

```c
main() : Void {
    Int foo = id(137);
    Bool bar = id(True);
    (Int, Bool) foobar = tuple(foo, bar);
    
    print(foobar);
    return;
}
```

It knows that the type of a is based on type of the input value. 


# Setting up a development environment

If you are using Visual Studio Code, setting up a development environment is very easy.

1. Install [Dev Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers);
2. Open this project using Visual Studio Code;
3. Press `F1` to bring up the command palette;
4. Select "Dev Containers: Reopen in Container...".

This sets up an *entire* development environment, including a debugger, a linter and documentation. It may take a while to set up (~30 minutes) the first time.
