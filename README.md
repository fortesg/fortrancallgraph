# FortranCallGraph

`FortranCallGraph` (FCG) is a static source code analysis tool for Fortran. 
It's main purpose is to track recursively the usage of variables within a subroutine and its called subroutines and functions.
Therefore it creates a call graph by parsing [GCC assembler files](https://gcc.gnu.org/onlinedocs/gcc/Overall-Options.html#index-S) and then traverses this call graph while analyzing the original source code of the routines.

The analysis result will be a list of used global variables and a list of used members of derived type arguments. 
Arguments of basic types will not be tracked but also listed in the results.

`FortranCallGraph` is written in Python and is able to analyze Fortran90+ source files. It was mainly built to support the `FortranTestGenerator`: https://github.com/fortesg/fortrantestgenerator, but also contains a command-line interface.

So far, the documentation is very poor. If your interested in using `FortranCallGraph`, please feel free to contact me:   
Christian Hovy <<hovy@informatik.uni-hamburg.de>>

**Contents:** [Quick Start Guide](#quick-start-guide) | [Notes for ICON developers](#notes-for-icon-developers) | [License](#license)

## Quick Start Guide

### 1. Clone this repo

```
$> git clone https://github.com/chovy1/fortrancallgraph.git
$> cd fortrancallgraph
```

### 2. Fill out the configuration file [config_fortrancallgraph.py](config_fortrancallgraph.py):

The meaning of the variables is documented in the [sample configuration file](config_fortrancallgraph.py).

### 3. Create assembler files

Compile your Fortran application with [gfortran](https://gcc.gnu.org/fortran) and the options `-S -g -O0` or `-save-temps -g -O0` to generate assembler files.

### 4. Run `./FortranCallGraph.py`

```
usage: FortranCallGraph.py [-h]
                           (-p {list-modules,list-subroutines,tree,dot} | -a {all,globals,arguments,result} | -d {statements,lines} | -l {use,last,doc,contains,all,specs,first} | -u {files,modules})
                           [-v VARIABLE] [-ml MAXLEVEL] [-po] [-ln] [-cc] [-q]
                           [-i IGNORE] [-cf CONFIGFILE]
                           module [subroutine]

Print or analyse a subroutine's call graph.

positional arguments:
  module
  subroutine

optional arguments:
  -h, --help            show this help message and exit
  -p {list-modules,list-subroutines,tree,dot}, --printer {list-modules,list-subroutines,tree,dot}
                        Print the callgraph (list-modules: only list modules
                        containing subroutines from the call graph, list-
                        subroutines: only list subroutines, tree: in a tree-
                        like form, dot: in DOT format for Graphviz).
  -a {all,globals,arguments,result}, --analysis {all,globals,arguments,result}
                        Analyze variable usage (all: both arguments and
                        globals, globals: only module variables, arguments:
                        only subroutine arguments, result: only function
                        result).
  -d {statements,lines}, --dump {statements,lines}
                        Dump subroutine or module source code (statements:
                        normalized source lines, lines: original source
                        lines). When no subroutine is given, the whole module
                        is dumped.
  -l {use,last,doc,contains,all,specs,first}, --line {use,last,doc,contains,all,specs,first}
                        Show some interesting source lines of the subroutine
                        (use: the last USE statement, last: the last line,
                        containing the END keyword, doc: the first line of the
                        leading comment - the same as "first" when no comment
                        exists, contains: the CONTAINS statement - -1 when
                        there is no such statement, all: all of the others,
                        specs: the last variable specification, first: the
                        first line, containing the SUBROUTINE/FUNCTION
                        keyword).
  -u {files,modules}, --use {files,modules}
                        Prints use dependencies of a subroutine (files: file
                        pathes, modules: module names).
  -v VARIABLE, --variable VARIABLE
                        Restrict the analysis to the given variable which has
                        to be a subroutine argument and of a derived type.
                        Applicable with -a arguments.
  -ml MAXLEVEL, --maxLevel MAXLEVEL
                        Limits depth of callgraph output. Applicable with -p.
  -po, --pointersOnly   Limit result output to pointer variables. Applicable
                        with -a.
  -ln, --lineNumbers    Add line numbers to the output. Applicable with -d.
  -cc, --clearCache     Create a new call graph instead of using a cached one.
                        Applicable with -p or -a.
  -q, --quiet           Reduce the output. Applicable with -a and -l.
  -i IGNORE, --ignore IGNORE
                        Leave out subroutines matching a given regular
                        expression. Applicable with -p and -a.
  -cf CONFIGFILE, --configFile CONFIGFILE
                        Import configuration from this file.
```
#### Examples:

* Print the call graph of the subroutine `my_subroutine` from module `my_module`:  
```
$> ./FortranCallGraph.py -p tree my_module my_subroutine
```
or  
```
$> ./FortranCallGraph.py -p tree __my_module_MOD_my_subroutine
```
* Create a visual graph using [Graphviz](http://www.graphviz.org):
```
$> ./FortranCallGraph.py -p dot my_module my_subroutine | dot -Tpng > my_subroutine.png
```
* List all used global variables used in subroutine `my_subroutine` from module `my_module` or in directly and indirectly called subroutines and functions:  
```
$> ./FortranCallGraph.py -a globals my_module my_subroutine
```  

* List all basic type arguments and used members of derived type arguments:  
```
$> ./FortranCallGraph.py -a arguments my_module my_subroutine
```  

* List all used members of derived type argument `arg1`:  
```
$> ./FortranCallGraph.py -a arguments -v arg1 my_module my_subroutine
```  

* List both, used globals and arguments:  
```
$> ./FortranCallGraph.py -a all my_module my_subroutine
```

Everything else you have to find out on your own, so far.

## Notes for ICON developers

#### 1. Create assembler files

I have done it like this:

* In my `mh-linux` file I have added `$FFLAGS` itself to `FFLAGS` under the `gcc` section:
  ```
  FFLAGS      = $FFLAGS $FCPP $FLANG $FWARN $INCLUDES
  ```
* Then, when I want to create the assembler files, I just run:
  ```
  $> make clean
  $> export FFLAGS='-save-temps -g -O0' && ./configure && make
  $> find build/x86_64-unknown-linux-gnu -name *.f90 -delete 
  $> export FFLAGS='' && ./configure && make
  ```
#### 2. Configuration

[My configuration file for ICON](https://github.com/fortesg/config-examples/blob/master/icon/config_fortrancallgraph.py)

## License

[GNU General Public License v3.0](LICENSE)
