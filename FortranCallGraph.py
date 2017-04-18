#!/usr/bin/python

'''
"Achtung! Funktioniert nur mit erwartungskonformen Eingaben"
Created on 10.07.2014

@author: Christian Hovy
'''

import sys;
import argparse;

from source import SubroutineFullName;
from tree import TreeLikeCallGraphPrinter;
from dot import DotFormatCallGraphPrinter;
from lister import SubroutineListingCallGraphPrinter, ModuleListingCallGraphPrinter;
from globals import GlobalVariablesCallGraphAnalysis
from trackvariable import TrackVariableCallGraphAnalysis
from allvariables import AllVariablesCallGraphAnalysis
from dumper import SourceLineDumper, SourceStatementDumper
from linenumbers import DeclarationLineNumberFinder, EndStatementLineNumberFinder, FirstDocumentationLineFinder, LastSpecificationLineFinder, AllLineFinder,\
    LastUseLineFinder, ContainsLineFinder

from config_fortrancallgraph import GRAPH_BUILDER, SOURCE_FILES, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES
import re

GRAPH_PRINTERS = {'tree': TreeLikeCallGraphPrinter(), 
                  'dot': DotFormatCallGraphPrinter(),
                  'list-subroutines': SubroutineListingCallGraphPrinter(),
                  'list-modules': ModuleListingCallGraphPrinter() } 
graphPrintersHelp = 'tree: in a tree-like form; dot: in DOT format for Graphviz; list-subroutines: only list subroutines; list-modules: only list modules containing subroutines from the call graph'

GRAPH_ANALYSIS = {'globals': GlobalVariablesCallGraphAnalysis(SOURCE_FILES, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES),
                  'arguments': TrackVariableCallGraphAnalysis(SOURCE_FILES, EXCLUDE_MODULES, IGNORE_DERIVED_TYPES),
                  'all': AllVariablesCallGraphAnalysis(SOURCE_FILES, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES)}
graphAnalysisHelp = 'arguments: only subroutine arguments; globals: only module variables; all: both arguments and globals'

SUBROUTINE_DUMPER = {'lines': SourceLineDumper(SOURCE_FILES),
                     'statements': SourceStatementDumper(SOURCE_FILES)}
subroutineDumperHelp = 'lines: original source lines; statements: normalized source lines' 

LINE_NUMBER_FINDER = {'first': DeclarationLineNumberFinder(SOURCE_FILES),
                     'last': EndStatementLineNumberFinder(SOURCE_FILES),
                     'doc': FirstDocumentationLineFinder(SOURCE_FILES),
                     'specs': LastSpecificationLineFinder(SOURCE_FILES),
                     'use': LastUseLineFinder(SOURCE_FILES),
                     'contains': ContainsLineFinder(SOURCE_FILES),
                     'all': AllLineFinder(SOURCE_FILES)} 
lineNumberFinderHelp = 'first: the first line, containing the SUBROUTINE/FUNCTION keyword; last: the last line, containing the END keyword; doc: the first line of the leading comment - the same as "first" when no comment exists; specs: the last variable specification; use - the last USE statement; contains: the CONTAINS statement - -1 when there is no such statement; all: all of the others' 


def parseArguments():
    argParser = argparse.ArgumentParser(description="Print or analyse a subroutine's call graph.");
    actionArg = argParser.add_mutually_exclusive_group(required=True)
    actionArg.add_argument('-p', '--printer', choices=GRAPH_PRINTERS.keys(), help='Print the callgraph (' + graphPrintersHelp + ').');
    actionArg.add_argument('-a', '--analysis', choices=GRAPH_ANALYSIS.keys(), help='Analyze variable usage (' + graphAnalysisHelp + ').');
    actionArg.add_argument('-d', '--dump', choices=SUBROUTINE_DUMPER.keys(), help='Dump subroutine or module source code (' + subroutineDumperHelp + '). When no subroutine is given, the whole module is dumped.');
    actionArg.add_argument('-l', '--line', choices=LINE_NUMBER_FINDER.keys(), help='Show some interesting source lines of the subroutine (' + lineNumberFinderHelp + ').');
    argParser.add_argument('-v', '--variable', type=str, help='Restrict the analysis to the given variable which has to be a subroutine argument and of a derived type. Applicable with -a arguments.');
    argParser.add_argument('-po', '--pointersOnly', action="store_true", help='Limit result output to pointer variables. Applicable with -a.');
    argParser.add_argument('-ln', '--lineNumbers', action="store_true", help='Add line numbers to the output. Applicable with -d.');
    argParser.add_argument('-cc', '--clearCache', action="store_true", help='Create a new call graph instead of using a cached one. Applicable with -p or -a.');
    argParser.add_argument('-q', '--quiet', action="store_true", help='Reduce the output. Applicable with -a and -l.');
    argParser.add_argument('-i', '--ignore', type=str, help='Leave out subroutines matching a given regular expression. Applicable with -p and -a.');
    argParser.add_argument('module');
    argParser.add_argument('subroutine', nargs='?', default=None);
    return argParser.parse_args();

def main():
    args = parseArguments()
    moduleName = args.module
    subroutineName = args.subroutine
    subroutineFullName = None
    sourceFileName = None
    
    if not moduleName:
        print >> sys.stderr, 'Missing Module (and Subroutine) name!';
        exit(1);
    elif not subroutineName:
        if SubroutineFullName.validFullName(moduleName):
            subroutineFullName = SubroutineFullName(moduleName)
        elif args.dump is not None or args.line is not None:
            if not SubroutineFullName.validIdentifier(moduleName):
                if args.dump is not None and moduleName.lower().endswith('.f90'):
                    sourceFileName = moduleName
                else:
                    print >> sys.stderr, 'Invalid Module name!';
                    exit(1);
        else: 
            print >> sys.stderr, 'Missing Subroutine name!';
            exit(1);
    elif SubroutineFullName.validParts(moduleName, subroutineName):
        subroutineFullName = SubroutineFullName.fromParts(moduleName, subroutineName)
    else:
        print >> sys.stderr, 'Invalid Module and/or Subroutine name!';
        exit(1);
        
    if subroutineFullName is not None and not SOURCE_FILES.existsSubroutine(subroutineFullName):
        print >> sys.stderr, 'ERROR: Subroutine ' + str(subroutineFullName) + ' not found!';
        exit(2);
    elif sourceFileName is not None and SOURCE_FILES.existsSourceFile(sourceFileName):
        print >> sys.stderr, 'ERROR: Source file ' + sourceFileName + ' not found!';
        exit(2);
    elif subroutineFullName is None and sourceFileName is None and not SOURCE_FILES.existsModule(moduleName):
        print >> sys.stderr, 'ERROR: Module ' + moduleName + ' not found!';
        exit(2);
        
    ignoreRegex = None
    if args.ignore is not None:
        try:
            ignoreRegex = re.compile(args.ignore)
        except re.error:
            print >> sys.stderr, 'Invalid regular expression for -i/--ignore option!';
            exit(1);
    
    if args.printer is not None:
        callGraph = GRAPH_BUILDER.buildCallGraph(subroutineFullName, args.clearCache)
        printer = GRAPH_PRINTERS[args.printer]
        printer.setIgnoreRegex(ignoreRegex)
        printer.printCallGraph(callGraph)
    elif args.analysis is not None:
        callGraph = GRAPH_BUILDER.buildCallGraph(subroutineFullName, args.clearCache)
        analysis = GRAPH_ANALYSIS[args.analysis]
        if args.analysis == 'arguments' and args.variable is not None:
            analysis.setVariableName(args.variable)
        if args.pointersOnly:
            analysis.setPointersOnly(True)
        if args.quiet:
            analysis.setMinimalOutput(True)
        analysis.setIgnoreRegex(ignoreRegex)
        analysis.analyzeCallgraph(callGraph)
    elif args.dump is not None:
        dumper = SUBROUTINE_DUMPER[args.dump]
        if (args.lineNumbers):
            dumper.setPrintLineNumbers(True)
        if subroutineFullName is not None:
            dumper.dumpSubroutine(subroutineFullName);
        elif sourceFileName is not None:
            dumper.dumpSourceFile(sourceFileName)
        else:
            dumper.dumpModule(moduleName)
    elif args.line is not None:
        lineNumberFinder = LINE_NUMBER_FINDER[args.line]
        if args.quiet:
            lineNumberFinder.setMinimalOutput(True)
        if subroutineFullName is not None:
            lineNumberFinder.printLineNumber(subroutineFullName)
        else:
            lineNumberFinder.printLineNumber(moduleName)

if __name__ == "__main__":
    main()