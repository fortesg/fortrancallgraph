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

GRAPH_ANALYSIS = {'globals': GlobalVariablesCallGraphAnalysis(SOURCE_FILES, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES),
                  'arguments': TrackVariableCallGraphAnalysis(SOURCE_FILES, EXCLUDE_MODULES, IGNORE_DERIVED_TYPES),
                  'all': AllVariablesCallGraphAnalysis(SOURCE_FILES, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES)} 

SUBROUTINE_DUMPER = {'lines': SourceLineDumper(SOURCE_FILES),
                     'statements': SourceStatementDumper(SOURCE_FILES)} 

LINE_NUMBER_FINDER = {'first': DeclarationLineNumberFinder(SOURCE_FILES),
                     'last': EndStatementLineNumberFinder(SOURCE_FILES),
                     'doc': FirstDocumentationLineFinder(SOURCE_FILES),
                     'specs': LastSpecificationLineFinder(SOURCE_FILES),
                     'use': LastUseLineFinder(SOURCE_FILES),
                     'contains': ContainsLineFinder(SOURCE_FILES),
                     'all': AllLineFinder(SOURCE_FILES)} 


def parseArguments():
    argParser = argparse.ArgumentParser(description='Build and print call graph.');
    actionArg = argParser.add_mutually_exclusive_group(required=True)
    actionArg.add_argument('-p', '--printer', choices=GRAPH_PRINTERS.keys());
    actionArg.add_argument('-a', '--analysis', choices=GRAPH_ANALYSIS.keys());
    actionArg.add_argument('-d', '--dump', choices=SUBROUTINE_DUMPER.keys());
    actionArg.add_argument('-l', '--line', choices=LINE_NUMBER_FINDER.keys());
    argParser.add_argument('-v', '--variable', type=str);
    argParser.add_argument('-ln', '--lineNumbers', action="store_true");
    argParser.add_argument('-cc', '--clearCache', action="store_true");
    argParser.add_argument('-q', '--quiet', action="store_true");
    argParser.add_argument('-i', '--ignore', type=str);
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
        elif args.dump is not None:
            if not SubroutineFullName.validIdentifier(moduleName):
                if moduleName.lower().endswith('.f90'):
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
        lineNumberFinder.printLineNumber(subroutineFullName)

if __name__ == "__main__":
    main()