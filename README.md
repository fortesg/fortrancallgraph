# fortrancallgraph

`FortranCallgraph` (FCG) is a static source code analysis tool for Fortran. 
It's main purpose is to track recursively the usage of variables within a subroutine and its called subroutines and functions.
Therefore it creates a call graph by parsing [GCC assembler files](https://gcc.gnu.org/onlinedocs/gcc/Overall-Options.html#index-S) and then traverses this call graph while analyzing the original source code of the routines.

The analysis result will be a list of used global variables and a list of used members of derived type arguments. 
Arguments of basic types will not be tracked but also listed in the results.

`FortranCallgraph` is written in Python and is able to analyze Fortran90+ source files.

So far, the documentation is very poor. If your interested in using `FortranCallgraph`, please feel free to contact me:   
Christian Hovy <<hovy@informatik.uni-hamburg.de>>

## Quick Start Guide

#### 1. Clone this repo

```
$> git clone https://github.com/chovy1/fortrancallgraph.git
$> cd fortrancallgraph
```

#### 2. Fill out the configuration file [config_fortrancallgraph.py](config_fortrancallgraph.py):

`FCG_DIR` : The location of FortranCallgraph (usually `os.path.dirname(os.path.realpath(__file__))`)

`CACHE_DIR` : FCG is able to cache call graphs. This is folder for the cache files (usually `FCG_DIR + '/cache'`).

`ASSEMBLER_DIR` : The location of the GCC assembler files you have created from your application's sources.

`SOURCE_DIR` : The location of the source files of your application. Can be organized in subfolders.

`SPECIAL_MODULE_FILES` : FCG assumes that your code is organized in modules and that every module is in a file with the name `<modulename>.f90`. In this constant you can declare a dict with exceptions from this rule.
Example:  
```python
SPECIAL_MODULE_FILES = { 'mod_foo': 'bar.f90' }
```
`EXCLUDE_MODULES` : A list of modules that shall not be analyzed

`IGNORE_GLOBALS_FROM_MODULES` : A list of modules which module variables shall not appear in the result

`IGNORE_DERIVED_TYPES` : A list of derived type which members shall not appear in the result

#### 3. Create assembler files

Compile your Fortran application with [gfortran](https://gcc.gnu.org/fortran) and the options `-S -g -O0` to generate assembler files.

#### 4. Run `./FortranCallGraph.py`

```
usage: FortranCallgraph.py [-h]
                           (-p {list-modules,list-subroutines,tree,dot} | -a {all,globals,arguments} | -d {statements,lines} | -l {use,last,doc,contains,all,specs,first})
                           [-v VARIABLE] [-ln] [-cc] [-q] [-i IGNORE]
                           module [subroutine]

Build and print call graph.

positional arguments:
  module
  subroutine

optional arguments:
  -h, --help            show this help message and exit
  -p {list-modules,list-subroutines,tree,dot}, --printer {list-modules,list-subroutines,tree,dot}
  -a {all,globals,arguments}, --analysis {all,globals,arguments}
  -d {statements,lines}, --dump {statements,lines}
  -l {use,last,doc,contains,all,specs,first}, --line {use,last,doc,contains,all,specs,first}
  -v VARIABLE, --variable VARIABLE
  -ln, --lineNumbers
  -cc, --clearCache
  -q, --quiet
  -i IGNORE, --ignore IGNORE
```
##### Examples:

* Print the call graph of the subroutine `my_subroutine` from module `my_module`:  
```
$> ./FortranCallgraph.py -p tree my_module my_subroutine
```
or  
```
$> ./FortranCallgraph.py -p tree __my_module_MOD_my_subroutine
```

* List all used global variables used in subroutine `my_subroutine` from module `my_module` or in directly and indirectly called subroutines and functions:  
```
$> ./FortranCallgraph.py -a globals my_module my_subroutine
```  

* List all basic type arguments and used members of derived type arguments:  
```
$> ./FortranCallgraph.py -a arguments my_module my_subroutine
```  

* List all used members of derived type argument `arg1`:  
```
$> ./FortranCallgraph.py -a arguments -v arg1 my_module my_subroutine
```  

* List both, used globals and arguments:  
```
$> ./FortranCallgraph.py -a all my_module my_subroutine
```

Everything else you need to find out on your own, so far.

## License

[GNU General Public License v3.0](LICENSE)
