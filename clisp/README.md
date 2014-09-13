# Setup

I'm using clisp and lisp-unit to test the macros.  Here's what I did to set it up 
on OS X.

## Prerequisites

- Common Lisp
- lisp-unit test library


## Installing clisp

```bash
brew install clisp
```

## Installing test library

```bash
curl -O http://beta.quicklisp.org/quicklisp.lisp
clisp -repl quicklisp.lisp
[1]> (quicklisp-quickstart:install)
[1]> (quit)
```

## Running the tests

I'm using (https://github.com/github/hub)[hub] to frontend my git clone because then there's less to type

```bash
hub clone markmontymark/macros
cd macros/clisp
make
```

