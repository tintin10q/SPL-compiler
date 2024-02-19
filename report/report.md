---
title: "Compiler Construction"
subtitle: "Lexical Analyses (Interim Report)"
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

We chose Haskell to implement our compiler, since it is especially well-suited for compiler construction. Mainly Haskell's support for algebraic data types and its well-suitedness for writing parsers makes it ideal for crafting compilers. It also provides a good learning opportunity for both of us, since we want to get better at Haskell.

# Lexical analyses

This section describes the lexical analyses phase of the compiler (parsing).

## Designing the abstract syntax tree

We designed the abstract syntax tree immediately the first week. It was the first thing that we did in the project and we spent quite some time on it.
We used a bottom-up approach to construct the AST. We started with the simple constructs (e.g. literals), and worked up to larger constructs such as function declarations. The program is just a list of function declarations.

Since we did not have a grammar yet, we looked at the available examples in the GitLab repository to figure out the syntax of SPL. We used our ast as a grammer for the most part but we did write an extended BNF grammer in the appendix.

## The parser

We use a parser combinator for parsing. We started with building our own parser combinators. This was a good learning experiance. 
But we soon realised this effort would require an significantly amount of work and make it much more difficult to get good error messages.
We decided that using more mature tools instead of trying to reinvent the weel would give us more time to work on cool extensions. 

Therefore, we switched to the parser combinator library [megaparsec](https://hackage.haskell.org/package/megaparsec), which is the (informal) successor of [parsec](https://hackage.haskell.org/package/parsec).

To become more familiar with the libary, and for occasional tips, we used [this excellent tutorial by Mark Karpov](https://markkarpov.com/tutorial/megaparsec.html).

Parser combinators work through *function composition*, where we take very simple parsers (such as "parse a single character") and compose these together to create more complicated parsers (such as "parse the word 'parser'"). This composition usually happens through helper functions, defined by megaparsec, that take zero or more parsers, and construct a new (bigger) parser. For example, the `<|>` combinator takes two parsers, and constructs a new parser that first tries the parser on the left, and if it fails tries the parser on the right.  

Since parsers in megaparsec are also part of a number of useful typeclasses, such as Monad, we can use a number of useful functions Haskell itself provides to compose parsers (e.g. `do` notation, `many`, `some`, `<$>`).

## Handling associativity

Since we use the megaparsec library, we get quite a number of things for free, such as error handling and associativity. Megaparsec provides us with the helper function `makeExprParser` that given an operator table and a parser for terms (e.g. any expression that does not have an operator, such as literals) constructs a parser that has the specified operators in the specified order or precedence and with the specified associativity.

Our operator table is as follows:

```haskell
operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ Prefix (UnaryOp Negate <$ L.tExcl)
      ]
    , [ InfixL (BinOp Mul <$ L.tStar)
      , InfixL (BinOp Div <$ L.tSlash)
      , InfixL (BinOp Mod <$ L.tPercent)
      ]
    , [ InfixL (BinOp Add <$ L.tPlus)
      , InfixL (BinOp Sub <$ L.tMin)
      ]
    , [ InfixR (BinOp Cons <$ L.tColon)]
    , [ InfixN (BinOp Gt <$ L.tGt)
      , InfixN (BinOp Gte <$ L.tGte)
      , InfixN (BinOp Lt <$ L.tLt)
      , InfixN (BinOp Lte <$ L.tLte)
      , InfixN (BinOp Eq <$ L.tDoubleEq)
      , InfixN (BinOp Neq <$ L.tExclEq)
      ]
    , [ InfixR (BinOp And <$ L.tDoubleAmpersand) ]
    , [ InfixR (BinOp Or <$ L.tDoublePipe) ]
    ]
```

The operator table is ordered in decreasing precedence (i.e. the higher in the list, the greater the binding strength of the set of operators). Any operators in the same sublist have the same precedence. Associativity can be modified using the constructors from the `Operator` datatype, which supports:

- `InfixN`: for non-associative infix operators;
- `InfixL`: for left-associative infix operators;
- `InfixR`: for right-associative infix operators;
- `Prefix`: for prefix operators;
- `Postfix`: for postfix operators.

We chose the same precedence and associativity as Haskell ([source](https://rosettacode.org/wiki/Operator_precedence#Haskell)).

## Error handling

Error handling is handled by megaparsec, which, out of the box gives very good error messages. For example:

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

We do not have a seperate lexing step. Instead, we use a (what we call) just-in-time lexer. Our lexer is just a collection of regular parsers that only parse simple tokens (as a regular lexer would), such as keywords, identifiers or symbols, while keeping whitespace and comments into account.

This is (mostly) done using the `symbol` parser combinator, which takes any string and creates a parser that parses exactly that string, while throwing away comments and whitespace at the end. For example, `symbol "a"` should parse `a`, but also `a/* comment */`.

We use these lexer parsers all throughout the rest of the parser, in places where you would normally consume a token with a regular parser.

We chose this approach, since it is well supported by megaparsec. Eventhough megaparsec does work on arbitrary input streams (including user-defined tokens), you cannot use a large part of the predefined parsers created by the megaparsec community. We do not see a benefit of converting the whole input into tokens first when using parser combinators.

## Pretty printing

We have not yet implemented the pretty printer. But we do know that with our current lexer design all the comments will be lost. To resolve this we could store the comments somewhere outside of the AST or actually have a lexer step, seperate from the parsing. 

## Testing

For testing, we use the testing library [hspec](https://hackage.haskell.org/package/hspec), which has great support for megaparsec through the [hspec-megaparsec](https://hackage.haskell.org/package/hspec-megaparsec) library.

These tests really helped us during development, as we were able to find bugs quickly.

## Problems

Since we started with writing the AST in Haskell based on the examples in the repo, we did not have any major problems during this phase. Having a well-defined AST really helped us make the parsers, as we were able to start from the most simple parsers (such as literals), and easily work our way upwards.

We initially did struggle with the left-recusivity of property access (e.g. `a.hd`), but fixed this eventually.

We also wrote many tests for our parsers from the start, which helped reduce bugs.

\pagebreak

# appendix

## Grammar

| Symbol | Meaning                                |
|--------|----------------------------------------|
| \|     | or                                     |
| 'if'   | Literal if                             |
| '''    | Literal '                              |
| [ a ]  | a should appear 0 or 1 time            |
| a*     | a should appear 0 or more times (many) |
| a+     | a should appear 1 or more times (some) |

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
  
Typed = ':' Type

Decl =
    -- a(b : c, d : e) : Bool {
    --     f();
    -- }
    Identifier '(' [Identifier [Typed] [',' Identifier [Typed] ]* ] ')' [Typed] '{' Stmt* '}'

Stmt =
    | 'return' [Expr] ';'
    | 'if' '(' Expr ')' '{' Stmt* '}' [else '{' Stmt* '}']    -- if (a) {b} else {c}
    | 'while' '(' Expr ')' '{' Stmt* '}'                -- while (a) {b}
    | Expr  ';'                         -- a;
    | 'var' [Type] Identifier Expr  ';' -- var hello = 'w':'o':'r':'l':'d':[]

  
Expr =
    | Expr BinOp Expr         -- a ∘ b
    | UnaryOp Expr            -- ∘ a
    | Variable '=' Expr       -- a = b
    | Identifier '(' [Expr] [',' Expr]* ')' -- f()
    | Variable '.' Variable   -- a
    | Variable                -- a
    | Literal                 -- 10

UnaryOp = '!' -- !a

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

Variable = 
    | Identifier -- a
    | Expr '.' Identifier -- a.b

Literal =
    | 'true'
    | 'false'
    | Int            
    | Float        
    | Char          
    | '('Expr ',' Expr')' 
    | '[' ']' 

Char = ''' UnicodeChar '''
Float = Int '.' Int -- Is this ok?
Int = [ '-' ] digit+
Identifier = alphaNumLower [''']
```