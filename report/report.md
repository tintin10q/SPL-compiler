---
title: "Compiler Construction"
subtitle: "Semantic Analyses (Interim Report)"
date: \today
author:
- "Marijn van Wezel (s1040392)"
- "Quinten Cabo (s1076992)"
lang: en
documentclass: article
toc: false
numbersections: true
fontsize: 11pt
font-family: fourier
fontfamily: fourier
bibliography: references.bib
geometry: margin=2.5cm
listings: true
biblio-style: apa
colorlinks: true
titlepage: true
listings-disable-line-numbers: true
footnotes-pretty: true
titlepage-rule-height: 0
header-includes: |
    \usepackage{amsmath}
    \usepackage[T1]{fontenc}
    \usepackage{textcomp}
    \usepackage{fourier}
    \usepackage{pmboxdraw}
    \usepackage{microtype}
    \usepackage{amsthm}
    \usepackage{listings}
    \usepackage{footmisc}

    \newcommand{\coq}{\textsc{Coq}} 

    \DeclareUnicodeCharacter{0393}{$\Gamma$}
    \DeclareUnicodeCharacter{03BB}{$\lambda$}
    \DeclareUnicodeCharacter{03C3}{$\sigma$}
    \DeclareUnicodeCharacter{03C4}{$\tau$}

    \lstdefinelanguage{SPL}{%
        alsoletter={0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_+-*/\%=<>!\&|},
        morekeywords={while,else,if,print,return,isEmpty,hd,tl,Int,Char,Bool,Void,True,False,var},%
        sensitive=true,%
        morecomment=[l]{//},%
        morecomment=[n]{/*}{*/},%
        literate=%
            {->}{{$\rightarrow$}}2
            {>=}{{$\geq\:$}}1
            {<=}{{$\leq\:$}}1
            {==}{{$\equiv$}}1
    }
    \lstset{%
        upquote=true,
        breakatwhitespace=false,
        breaklines=true,
        postbreak=\mbox{\textcolor{gray}{$\hookrightarrow$}\space},
        keepspaces=true,
        basicstyle=\tt\footnotesize,
        commentstyle=\sl,
        keywordstyle=\bf,
        stringstyle=\tt,
        showspaces=false,
        showstringspaces=false,
        showtabs=false,
        tabsize=4,
        basewidth=0.43em,
        columns=[c]fixed,
        texcl=true,
        captionpos=b
    }
---

# Introduction

This report outlines our progress with the SPL compiler.  

The language we will be implementing is called SPL (Simple Programming Language). It is similar to C, but with support for polymorphic datatypes.

Some examples of SPL include:

```spl
fib(n : Int) : Int {
    if (n == 0) {
        return 0;
    }

    if (n == 1) {
        return 1;
    }

    return fib(n - 1) + fib(n - 2);
}
```

```spl
fac(n : Int) : Int {
    if (n == 0) {
        return 1;
    }

    return n * fac(n - 1);
}
```

We chose [Haskell](https://www.haskell.org/) to implement our compiler, since it is especially well-suited for compiler construction. Mainly Haskell's support for algebraic data types and its well-suitedness for writing parsers makes it ideal for crafting compilers. It also provides a good learning opportunity for both of us, since we want to get better at Haskell.

# Lexical analyses

This section describes the lexical analyses phase of the compiler (parsing).

## Designing the abstract syntax tree

We designed the abstract syntax tree immediately the first week. It was the first thing that we did in the project and we spent quite some time on it. We used a bottom-up approach to construct the AST. We started with the simple constructs (e.g. literals), and worked up to larger constructs such as function declarations. The program is just a list of function declarations.

Since we did not have a grammar yet, we looked at the available examples in the GitLab repository to figure out the syntax of SPL. We used our AST as a grammer for the most part but we did write an extended BNF grammer in the appendix.

Our abstract syntax tree is split up into four major inductive types, `Decl`, `Stmt`, `Expr` and `Literal`, with some helper inductives (e.g. for the types and binary operators) as well. These inductive types have the following meaning:

- `Decl`: for declarations (functions, global variables);
- `Stmt`: for statements (e.g. return, if, while);
- `Expr`: for expressions (e.g. binary expression, assignment, function call);
- `Literal`: for literals (e.g. numbers, chars, tuples).


## The parser

We use parser combinators for parsing. We started with building our own parser combinators. While this was a good learning experience,  we soon realised this would require a significant amount of work and would make it much more difficult to get good error messages. Therefore, we decided that using more mature tools (e.g. an existing parser combinator library) would give us more time to work on cool extensions. 

Therefore, we switched to the parser combinator library [megaparsec](https://hackage.haskell.org/package/megaparsec), which is the (informal) successor of [parsec](https://hackage.haskell.org/package/parsec). To become more familiar with the libary, and for occasional tips, we used [this excellent tutorial by Mark Karpov](https://markkarpov.com/tutorial/megaparsec.html), the maintainer of megaparsec.

Parser combinators work through *function composition*, where we take very simple parsers (such as "parse a single character") and compose these together to create more complicated parsers (such as "parse the word 'parser'"). This composition usually happens through helper functions, defined by megaparsec, that take zero or more parsers, and construct a new (bigger) parser. For example, the `<|>` combinator takes two parsers, and constructs a new parser that first tries the parser on the left, and if it fails tries the parser on the right. A large part of the internal mechanics of parser combinators, such as carrying error messages, are hidden away in the `Parser` type.

Since parsers in megaparsec are also part of a number of useful typeclasses, such as Monad, we can use a number of useful combinators Haskell itself provides to compose parsers (e.g. `do` notation, `many`, `some` and `<$>`).

## Handling associativity

Since we use the megaparsec library, we get quite a number of things for free, such as error handling and associativity. Megaparsec also provides us with the helper function `makeExprParser` that, given an operator table and a parser for terms (e.g. any expression that does not have an operator, such as literals), constructs a parser that has the specified operators in the specified order or precedence and with the specified associativity. We are using this `makeExprParser` function.

Our operator table is as follows:

```haskell
operatorTable =
    [ [ Postfix (unary (FieldAccess HeadField) (try (L.tDot <* L.tHead)))
      , Postfix (unary (FieldAccess TailField) (try (L.tDot <* L.tTail)))
      ]
    , [ Prefix (unary Negate L.tExcl)
      ]
    , [ InfixL (binary Mul L.tStar)
      , InfixL (binary Div L.tSlash)
      , InfixL (binary Mod L.tPercent)
      ]
    , [ InfixL (binary Add L.tPlus)
      , InfixL (binary Sub L.tMin)
      ]
    , [ InfixR (binary Cons L.tColon)]
    , [ InfixN (binary Gt L.tGt)
      , InfixN (binary Gte L.tGte)
      , InfixN (binary Lt L.tLt)
      , InfixN (binary Lte L.tLte)
      , InfixN (binary Eq L.tDoubleEq)
      , InfixN (binary Neq L.tExclEq)
      ]
    , [ InfixR $ binary And L.tDoubleAmpersand  ]
    , [ InfixR $ binary Or L.tDoublePipe ]
    ]
```

The operator table is ordered in decreasing precedence (i.e. the higher in the list, the greater the binding strength of the set of operators). Any operators in the same sublist have the same precedence. Associativity can be modified using the constructors from the `Operator` datatype, which supports:

- `InfixN`: for non-associative infix operators;
- `InfixL`: for left-associative infix operators;
- `InfixR`: for right-associative infix operators;
- `Prefix`: for prefix operators;
- `Postfix`: for postfix operators.

We chose the same precedence and associativity as Haskell ([source](https://rosettacode.org/wiki/Operator_precedence#Haskell)). We also use the `makeExprParser` for the field access operators (`.hd` and `.tl`), as this was the easiest solution.

## Error handling

Error handling is handled by megaparsec, which, out of the box gives quite good error messages. For example:

```
test.spl:1:7:
  |
1 | (a + b
  |       ^
unexpected end of input
expecting "!=", "&&", "<=", "==", ">=", "||", '!', '%', ''', ')', '*', '+', ',', '-', '/', ':', '<', '>', '_', or alphanumeric character
```

Megaparsec also supports [custom error messages](https://markkarpov.com/tutorial/megaparsec.html#parse-errors) and [error recovery](https://markkarpov.com/tutorial/megaparsec.html#reporting-multiple-parse-errors), but we have not implemented those yet.

## Lexer

We do not have a seperate lexing step. Instead, we use a (what we call) just-in-time lexer (scannerless). Our lexer is just a collection of regular parsers that only parse simple tokens (as a regular lexer would), such as keywords, identifiers or symbols, while discarding whitespace and comments. In megaparsec, whitespace is only discarded at the end of tokens in the lexer. This is why our main program parser has an additional "whitespace" parser at the start to discard comments and whitespace at the beginning of files as well.

"Lexing" is mostly done using the `symbol` parser combinator, which takes any string and creates a parser that parses exactly that string, while throwing away comments and whitespace at the end. For example, `symbol "a"` should parse `a`, but also `a/* comment */`.

We use these lexer parsers all throughout the rest of the parser, in places where you would normally consume a token with a regular parser. We chose this approach, since it is well supported by megaparsec and it just easily deals with any comments and whitespace as soon as you use the symbol parser. Eventhough megaparsec does work on arbitrary input streams (including user-defined tokens), you cannot use a large part of the predefined parsers created by the megaparsec community. We do not see a benefit of converting the whole input into tokens first when using parser combinators.
`
## Pretty printing

We have implemented a pretty printer. However, since comments are not included in the AST, our pretty printer strips comments, and can therefore not realistically be used for formatting. 

## Testing

For testing, we use the testing library [hspec](https://hackage.haskell.org/package/hspec), which has great support for megaparsec through the [hspec-megaparsec](https://hackage.haskell.org/package/hspec-megaparsec) library.

These tests really helped us during development, as we were able to find bugs quickly.

## Problems

Since we started with writing the AST in Haskell based on the examples in the repo, we did not have any major problems during this phase. Having a well-defined AST early on really helped us make the parsers, as we were able to start from the most simple parsers (such as literals), and easily work our way upwards.

We initially did struggle with the left-recusivity of property access (e.g. `a.hd`), but fixed this by making `.hd` and `.tl` postfix operators in the `makeExprParser`. This fixes the left-recursion, as megaparsec will only parse things with lower precedence on the left, causing it to no longer be fully recursive.

We also wrote many tests for our parsers from the start, which helped reduce bugs.

# Semantic analyses

This section describes the semantic analyses phase of the compiler (typechecking, etc). We are currently behind on schedule with our compiler, and have therefore not fully implemented this phase yet. We will try to catch up in the coming weeks. In this section, we explain what we have done so far, and the problems that we are currently tackling.

## Trees that grow

We wanted a flexible system to annotate trees with additional metadata that extends to the multiple phases of the compiler. To achieve this, we have implemented a variation of Trees that Grow [@DBLP:journals/corr/NajdJ16].

The approach to annotate our AST is based on type families. This works by adding a parameter to each (relevant) `data` type that determines the phase of the compiler. Using type families you can create seperate instances of `data` based on the phase of the compiler while keeping the general structure of the AST the same. Without type families you would need to build a seperate similar but distint trees for every phase of the compiler.

By using the `DataKinds` language extension, the phase is also a `data` type:

```hs
data Phase
  = EmptyP        -- Empty phase, used for testing
  | ParsedP       -- Phase after parsing with location information
  | TypecheckedP  -- Phase after full typechecking
```

Next, we add to each constructor of the `data` type a new field that has a type family with the phase as its typed. Since type families are functions from type to type, we can control the actual type of that field through the phase.

To explain this concept fully, we will look at how to implement this approach for a simplified version of expressions in SPL, where only binary expressions and literals are allowed. We start with the naive approach of using separate `data` types for each phase, then we introduce type families.

```hs
data ExprEmpty
  = BinOpExprEmpty BinOp ExprEmpty ExprEmpty
  | LiteralExprEmpty Literal

data ExprParsed
  = BinOpExprParsed SourcePos BinOp ExprParsed ExprParsed
  | LiteralExprParsed SourcePos Literal

data ExprTypechecked
  = BinOpExprTypechecked SourcePos Type BinOp ExprTypechecked ExprTypechecked
  | LiteralExprTypechecked SourcePos Type Literal 
```

The above approach is clumpsy, and requires us to make modifications in many places to make changes to the AST, since it requires us to keep all these different `data` types in sync. Additionally, it forces us to use long names for the different data constructors and makes it impossible to make functions that work on generic ASTs (e.g. functions that are agnostic to the phase of the AST).

Trees that Grow [@DBLP:journals/corr/NajdJ16] solves this by introducing a type family, which is a function from type to type, for each field of the `data` type. The only argument to these type families will be the phase of the AST. The type families for this example will therefore be:

```hs
type family BinOpExpr (p :: Phase)
type family LiteralExpr (p :: Phase)
```

Any fields specific to a particular phase of the compiler will then be replaced by a field of the associated type family, like so:

```hs
data Expr (p :: Phase)
  = BinOpExpr (BinOpExpr p) BinOp (Expr p) (Expr p)
  | LiteralExpr (LiteralExpr p) Literal
```

Finally, for each phase we can create an instance of the type family that determines the actual type for that field:

```hs
type instance BinOpExpr EmptyP = ()
type instance BinOpExpr ParsedP = SourcePos
type instance BinOpExpr TypecheckedP = (SourcePos, Type)

type instance LiteralExpr EmptyP = ()
type instance LiteralExpr ParsedP = SourcePos
type instance LiteralExpr TypecheckedP = (SourcePos, Type)
```

This gives us a lot of control over the actual contents of the AST for each type, since it allows us to assign a different type for each constructor in the AST for each phase of the compiler, while keeping the overall structure of the AST identical. Furthermore, it allows us to write functions that work on any phase by simply ignoring the field with the type family.

## Error messages

In the typecheck phase we have to unify the types that variables have with the types of expressions and the uses of the variables. This unification operation may fail, and as such our `unify` function returns an `Either`.

Because we have the location for each node in the AST, we can use this location to generate decent error messages that point to a specific location in the source code. However, since we have not yet finished the typechecking phase completely, we have not yet implemented generation of these error messages. Currently, the error is a simple string that explains *how* the unification failed (e.g. "Cannot unify `Int` with `Bool`.").glorious .

## Polymorphism, inference 

Our inference and type checking has now been implemented. But we still need to implement narrowing of functions for the code generation phase.

### Type variable instantiation

We have already made the choice to have type variables be local to their (function) declaration, meaning that the "same" type variable used in different declarations will actually be different during typechecking. For example, in the following code, the `a` in `f1`'s signature is different from the `a` in `f2`'s signature:

```c
id1(v: a): a {
  return v + v;
}

id2(v: a): a {
  return v;
}
```

This means that when we determine the `a` in `id1` to be an `Int`, we do **not** update the `a` in `id2`. This happens because we shall generate specific versions of functions with functions with type variables based on how the functions are called. 

### Polymorphism

We have not yet implemented polymorphism. This requires rewriting the ast to use specific functions for specific types. We will implement this based on how functions are called and we shall generate specific versions of them.

### Inference

We have implemented type inference for the expressions. 

### Overloading

We will not implement overloading except for the print function.

## Problems

In this section, we explain the problems that we are currently tackling.

- Fields are not really implemented in the parser or in the type checker but they are in the abstract syntax tree.
- Mutual recursion is not implemented, we now generate code by first generating code for all the var decls, then the funcdecls in order from top down.
- Specilizing functions after (or during?) type inference. We did not realize we needed to do this.
  - Should we do this during type checking or as a seperate phase? 
  - Is it a good idea to apply the generated substitution during type checking to do this before you look at how the functions are called?
- Having a working enviroment for code generation.
- Check return statements
  - Does every branch return? 
  - Does every return return the same type as the function decleration.

# Code generation 

We are not very far yet on the code generation. We implemented a data with every SSM instruction and WASM instruction. 
We implemented Monoid for these data so that the instructions can be chained together with the `<>` operator.

Then we made a type class called `GenSSM` and `GenWASM` with a single generate function that takes an enviroment and an AST node that implements the type generate class.

We will implement the lists as linked list in the memory. This allows us to implement the cons operator with a single instruction `STMH 2`. 
We will then just only put the adress of the list into the stack. This way every variable on the stack will be the same size.

We would like some feedback about how we are planning to represent the enviroment during code generation.
We came up with two types:

```hs
data VarData = VarData {
                        updateCode :: Code, -- Code to update value we find on stack in the storage,
                        loadCode :: Code, -- Code to push variable onto the stack
                        typeof :: Type } -- Type of the variable
type VarEnv = Map.Map String VarData
```

or

```hs
type VarEnv = Map.Map String Int
```

So either a string to an address or a record that has the code to load the variale onto the stack and update the variable wherever it is. 
This is a choice between making the code that generates the enviroment more complicated or making the code generation more complicated. What do you think?

# Appendix

## Grammar

| Symbol  | Meaning                                |
|---------|----------------------------------------|
| `|`     | or                                     |
| `'if'`  | literal string                         |
| `'\''`  | literal single quote                   |
| `[ a ]` | should appear 0 or 1 time              |
| `a*`    | should appear 0 or more times          |
| `a+`    | should appear 1 or more times          |

```
Program = Decl+

Type =
    | 'Int'             
    | 'Char'
    | 'Bool'
    | 'Void'
    | '(' Identifier ',' Identifier ')'
    | '[' Identifier ']'
    | Identifier  -- a

Decl =
    Identifier '(' [Identifier [':' Type] [',' Identifier [':' Type] ]* ] ')' [':' Type] '{' Stmt* '}'

Stmt =
    | 'return' [Expr] ';'
    | 'if' '(' Expr ')' '{' Stmt* '}' [else '{' Stmt* '}']
    | 'while' '(' Expr ')' '{' Stmt* '}'               
    | Variable '=' Expr ';'
    | Expr  ';'
    | 'var' [Type] Identifier Expr  ';'

Expr =
    | Expr BinOp Expr
    | UnaryPrefix Expr
    | UnaryPostfix Expr
    | Identifier '(' ')'
    | Identifier '(' Expr [',' Expr]* ')'
    | Variable              
    | Literal             

UnaryPrefix = 
    | '!'

UnaryPostfix =
    | '.' Field

BinOp =
    | '*'
    | '/'
    | '%'
    | '+'
    | '-'
    | ':'
    | '>'
    | '>='
    | '<'
    | '<='
    | '=='
    | '!='
    | '&&'
    | '||'

Variable = Identifier ['.' Field]

Field =
    | 'hd'
    | 'tl'

Literal =
    | 'true'
    | 'false'
    | Int            
    | Char          
    | '('Expr ',' Expr')' 
    | '[]' 

Char = ''' UnicodeChar '''
Float = [ '-' | '+' ] Int '.' Int
Int = [ '-' | '+' ] digit+
Identifier = (alphaNumLower | '_') (alphaNum | '_') '\''*
```

# Bibliography
