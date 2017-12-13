#!/usr/bin/python

'''
@author: Christian Hovy
'''

import sys;
import argparse;
import re

from source import SubroutineFullName, SourceFiles
from tree import TreeLikeCallGraphPrinter;
from dot import DotFormatCallGraphPrinter;
from lister import SubroutineListingCallGraphPrinter, ModuleListingCallGraphPrinter;
from globals import GlobalVariableTracker
from trackvariable import VariableTracker
from allvariables import AllVariablesCallGraphAnalysis
from dumper import SourceLineDumper, SourceStatementDumper
from linenumbers import DeclarationLineNumberFinder, EndStatementLineNumberFinder, FirstDocumentationLineFinder, LastSpecificationLineFinder, AllLineFinder,\
    LastUseLineFinder, ContainsLineFinder
from useprinter import UsedModuleNamePrinter, UsedFileNamePrinter
from assembler import FromAssemblerCallGraphBuilder
from treecache import CachedAssemblerCallGraphBuilder
from fcgconfigurator import loadFortranCallGraphConfiguration, CFG_SOURCE_DIRS, CFG_ASSEMBLER_DIRS, CFG_SPECIAL_MODULE_FILES,\
    CFG_CACHE_DIR, CFG_SOURCE_FILES_PREPROCESSED, CFG_EXCLUDE_MODULES, CFG_IGNORE_GLOBALS_FROM_MODULES, CFG_IGNORE_DERIVED_TYPES

GRAPH_PRINTERS = {'tree': 'in a tree-like form',
                  'dot': 'in DOT format for Graphviz',
                  'list-subroutines': 'only list subroutines',
                  'list-modules': 'only list modules containing subroutines from the call graph'}
def graphPrinter(key):
    if key not in GRAPH_PRINTERS: raise KeyError('No such CallGraphPrinter: ' + str(key))
    elif key == 'tree': return TreeLikeCallGraphPrinter()
    elif key == 'dot':  return DotFormatCallGraphPrinter()
    elif key == 'list-subroutines': return SubroutineListingCallGraphPrinter()
    elif key == 'list-modules': return ModuleListingCallGraphPrinter()
    else: raise NotImplementedError('CallGraphPrinter not yet implemented: ' + str(key))

GRAPH_ANALYSIS = {'arguments': 'only subroutine arguments',
                 'globals': 'only module variables',
                 'all': 'both arguments and globals'}
def graphAnalysis(key, SOURCE_FILES, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES):
    if key not in GRAPH_ANALYSIS: raise KeyError('No such CallGraphAnalyzer: ' + str(key))
    elif key == 'globals': return GlobalVariableTracker(SOURCE_FILES, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES)
    elif key == 'arguments': return VariableTracker(SOURCE_FILES, EXCLUDE_MODULES, IGNORE_DERIVED_TYPES)
    elif key == 'all': return AllVariablesCallGraphAnalysis(SOURCE_FILES, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES)
    else: raise NotImplementedError('CallGraphAnalyzer not yet implemented: ' + str(key))

SUBROUTINE_DUMPER = {'lines': 'original source lines',
                    'statements': 'normalized source lines'} 
def subroutineDumper(key, SOURCE_FILES):
    if key not in SUBROUTINE_DUMPER: raise KeyError('No such SourceDumper: ' + str(key))
    elif key == 'lines': return SourceLineDumper(SOURCE_FILES)
    elif key == 'statements': return SourceStatementDumper(SOURCE_FILES)
    else: raise NotImplementedError('SourceDumper not yet implemented: ' + str(key))

LINE_NUMBER_FINDER = {'first': 'the first line, containing the SUBROUTINE/FUNCTION keyword',
                    'last': 'the last line, containing the END keyword',
                    'doc': 'the first line of the leading comment - the same as "first" when no comment exists',
                    'specs': 'the last variable specification',
                    'use': 'the last USE statement',
                    'contains': 'the CONTAINS statement - -1 when there is no such statement',
                    'all': 'all of the others'}
def lineNumberFinder(key, SOURCE_FILES):
    if key not in LINE_NUMBER_FINDER: raise KeyError('No such LineNumberFinder: ' + str(key))
    elif key == 'first': return DeclarationLineNumberFinder(SOURCE_FILES),
    elif key == 'last': return EndStatementLineNumberFinder(SOURCE_FILES),
    elif key == 'doc': return FirstDocumentationLineFinder(SOURCE_FILES),
    elif key == 'specs': return LastSpecificationLineFinder(SOURCE_FILES),
    elif key == 'use': return LastUseLineFinder(SOURCE_FILES),
    elif key == 'contains': return ContainsLineFinder(SOURCE_FILES),
    elif key == 'all': return AllLineFinder(SOURCE_FILES)
    else: raise NotImplementedError('LineNumberFinder not yet implemented: ' + str(key))

USE_PRINTERS = {'modules': 'module names',
                'files': 'file pathes'} 
def usePrinter(key, SOURCE_FILES):
    if key not in USE_PRINTERS: raise KeyError('No such UsePrinter: ' + str(key))
    elif key == 'modules': return UsedModuleNamePrinter(SOURCE_FILES),
    elif key == 'files': return UsedFileNamePrinter(SOURCE_FILES)
    else: raise NotImplementedError('UsePrinter not yet implemented: ' + str(key))

def optionHelp(helps):
    return ", ".join('%s: %s' % (key, string) for key, string in helps.iteritems())

def parseArguments():
    argParser = argparse.ArgumentParser(description="Print or analyse a subroutine's call graph.");
    actionArg = argParser.add_mutually_exclusive_group(required=True)
    actionArg.add_argument('-p', '--printer', choices=GRAPH_PRINTERS.keys(), help='Print the callgraph (' + optionHelp(GRAPH_PRINTERS) + ').');
    actionArg.add_argument('-a', '--analysis', choices=GRAPH_ANALYSIS.keys(), help='Analyze variable usage (' + optionHelp(GRAPH_ANALYSIS) + ').');
    actionArg.add_argument('-d', '--dump', choices=SUBROUTINE_DUMPER.keys(), help='Dump subroutine or module source code (' + optionHelp(SUBROUTINE_DUMPER) + '). When no subroutine is given, the whole module is dumped.');
    actionArg.add_argument('-l', '--line', choices=LINE_NUMBER_FINDER.keys(), help='Show some interesting source lines of the subroutine (' + optionHelp(LINE_NUMBER_FINDER) + ').');
    actionArg.add_argument('-u', '--use', choices=USE_PRINTERS.keys(), help='Prints use dependencies of a subroutine (' + optionHelp(USE_PRINTERS) + ').');
    argParser.add_argument('-v', '--variable', type=str, help='Restrict the analysis to the given variable which has to be a subroutine argument and of a derived type. Applicable with -a arguments.');
    argParser.add_argument('-ml', '--maxLevel', type=int, help='Limits depth of callgraph output. Applicable with -p.');
    argParser.add_argument('-po', '--pointersOnly', action="store_true", help='Limit result output to pointer variables. Applicable with -a.');
    argParser.add_argument('-ln', '--lineNumbers', action="store_true", help='Add line numbers to the output. Applicable with -d.');
    argParser.add_argument('-cc', '--clearCache', action="store_true", help='Create a new call graph instead of using a cached one. Applicable with -p or -a.');
    argParser.add_argument('-q', '--quiet', action="store_true", help='Reduce the output. Applicable with -a and -l.');
    argParser.add_argument('-i', '--ignore', type=str, help='Leave out subroutines matching a given regular expression. Applicable with -p and -a.');
    argParser.add_argument('-cf', '--configFile', type=str, help='Import configuration from this file.');
    argParser.add_argument('module');
    argParser.add_argument('subroutine', nargs='?', default=None);
    return argParser.parse_args();

def main():
    args = parseArguments()
    config = loadFortranCallGraphConfiguration(args.configFile)
    if config is None:
        exit(3)

    graphBuilder = FromAssemblerCallGraphBuilder(config[CFG_ASSEMBLER_DIRS], config[CFG_SPECIAL_MODULE_FILES])
    if config[CFG_CACHE_DIR]:
        graphBuilder = CachedAssemblerCallGraphBuilder(config[CFG_CACHE_DIR], graphBuilder)
    sourceFiles = SourceFiles(config[CFG_SOURCE_DIRS], config[CFG_SPECIAL_MODULE_FILES], config[CFG_SOURCE_FILES_PREPROCESSED])
    excludeModules = config[CFG_EXCLUDE_MODULES]
    ignoreGlobalsFromModules = config[CFG_IGNORE_GLOBALS_FROM_MODULES]
    ignoreDerivedTypes = config[CFG_IGNORE_DERIVED_TYPES]
    
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
        
    if subroutineFullName is not None and not sourceFiles.existsSubroutine(subroutineFullName):
        print >> sys.stderr, 'ERROR: Subroutine ' + str(subroutineFullName) + ' not found!';
        exit(2);
    elif sourceFileName is not None and sourceFiles.existsSourceFile(sourceFileName):
        print >> sys.stderr, 'ERROR: Source file ' + sourceFileName + ' not found!';
        exit(2);
    elif subroutineFullName is None and sourceFileName is None and not sourceFiles.existsModule(moduleName):
        print >> sys.stderr, 'ERROR: Module ' + moduleName + ' not found!';
        exit(2);
        
    ignoreRegex = None
    if args.ignore is not None:
        try:
            ignoreRegex = re.compile(args.ignore)
        except re.error:
            print >> sys.stderr, 'Invalid regular expression for -i/--ignore option!';
            exit(1);
        
    maxLevel = -1
    if args.maxLevel is not None:
        maxLevel = args.maxLevel
    
    if args.printer is not None:
        callGraph = graphBuilder.buildCallGraph(subroutineFullName, args.clearCache)
        printer = graphPrinter(args.printer)
        printer.setIgnoreRegex(ignoreRegex)
        printer.setMaxLevel(maxLevel)
        printer.printCallGraph(callGraph)
    elif args.analysis is not None:
        callGraph = graphBuilder.buildCallGraph(subroutineFullName, args.clearCache)
        analysis = graphAnalysis(args.analysis, sourceFiles, excludeModules, ignoreGlobalsFromModules, ignoreDerivedTypes)
        if args.analysis == 'arguments' and args.variable is not None:
            analysis.setVariableName(args.variable)
        if args.pointersOnly:
            analysis.setPointersOnly(True)
        if args.quiet:
            analysis.setMinimalOutput(True)
        analysis.setIgnoreRegex(ignoreRegex)
        analysis.analyzeCallgraph(callGraph)
    elif args.dump is not None:
        dumper = subroutineDumper(args.dump, sourceFiles)
        if (args.lineNumbers):
            dumper.setPrintLineNumbers(True)
        if subroutineFullName is not None:
            dumper.dumpSubroutine(subroutineFullName);
        elif sourceFileName is not None:
            dumper.dumpSourceFile(sourceFileName)
        else:
            dumper.dumpModule(moduleName)
    elif args.line is not None:
        lineNumberFinder = lineNumberFinder(args.line, sourceFiles)
        if args.quiet:
            lineNumberFinder.setMinimalOutput(True)
        if subroutineFullName is not None:
            lineNumberFinder.printLineNumber(subroutineFullName)
        else:
            lineNumberFinder.printLineNumber(moduleName)
    elif args.use is not None:
        usePrinter = usePrinter(args.use, sourceFiles)
        usePrinter.printUses(subroutineFullName)

if __name__ == "__main__":
    main()