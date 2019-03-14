#!/usr/bin/python

'''
@author: Christian Hovy
'''

import argparse;
import re

from source import SubroutineFullName, SourceFiles
from tree import TreeLikeCallGraphPrinter;
from dot import DotFormatCallGraphPrinter;
from lister import SubroutineListingCallGraphPrinter, ModuleListingCallGraphPrinter;
from globals import GlobalVariableTracker
from trackvariable import VariableTracker, VariableTrackerSettings
from allvariables import AllVariablesCallGraphAnalysis
from dumper import SourceLineDumper, SourceStatementDumper
from linenumbers import DeclarationLineNumberFinder, EndStatementLineNumberFinder, FirstDocumentationLineFinder, LastSpecificationLineFinder, AllLineFinder,\
    LastUseLineFinder, ContainsLineFinder
from useprinter import UsedModuleNamePrinter, UsedFileNamePrinter
from assembler import GNUx86AssemblerCallGraphBuilder
from treecache import CachedAssemblerCallGraphBuilder
from fcgconfigurator import loadFortranCallGraphConfiguration, CFG_SOURCE_DIRS, CFG_ASSEMBLER_DIRS, CFG_SPECIAL_MODULE_FILES,\
    CFG_CACHE_DIR, CFG_SOURCE_FILES_PREPROCESSED, CFG_EXCLUDE_MODULES, CFG_IGNORE_GLOBALS_FROM_MODULES, CFG_IGNORE_DERIVED_TYPES,\
    CFG_ABSTRACT_TYPES
from printout import printErrorAndExit, printDebug

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
                 'result': 'only function result',
                 'globals': 'only module variables',
                 'all': 'both arguments and globals'}
def graphAnalysis(key, sourceFiles, settings, callGraphBuilder):
    if key not in GRAPH_ANALYSIS: raise KeyError('No such CallGraphAnalyzer: ' + str(key))
    elif key == 'globals': return GlobalVariableTracker(sourceFiles, settings, callGraphBuilder = callGraphBuilder)
    elif key == 'arguments' or key == 'result': return VariableTracker(sourceFiles, settings, callGraphBuilder = callGraphBuilder)
    elif key == 'all': return AllVariablesCallGraphAnalysis(sourceFiles, settings, callGraphBuilder = callGraphBuilder)
    else: raise NotImplementedError('CallGraphAnalyzer not yet implemented: ' + str(key))

SUBROUTINE_DUMPER = {'lines': 'original source lines',
                    'statements': 'normalized source lines'} 
def subroutineDumper(key, sourceFiles):
    if key not in SUBROUTINE_DUMPER: raise KeyError('No such SourceDumper: ' + str(key))
    elif key == 'lines': return SourceLineDumper(sourceFiles)
    elif key == 'statements': return SourceStatementDumper(sourceFiles)
    else: raise NotImplementedError('SourceDumper not yet implemented: ' + str(key))

LINE_NUMBER_FINDER = {'first': 'the first line, containing the SUBROUTINE/FUNCTION keyword',
                      'last': 'the last line, containing the END keyword',
                      'doc': 'the first line of the leading comment - the same as "first" when no comment exists',
                      'specs': 'the last variable specification',
                      'use': 'the last USE statement',
                      'contains': 'the CONTAINS statement - -1 when there is no such statement',
                      'all': 'all of the others'}
def lineNumberFinder(key, sourceFiles):
    if key not in LINE_NUMBER_FINDER: raise KeyError('No such LineNumberFinder: ' + str(key))
    elif key == 'first': return DeclarationLineNumberFinder(sourceFiles)
    elif key == 'last': return EndStatementLineNumberFinder(sourceFiles)
    elif key == 'doc': return FirstDocumentationLineFinder(sourceFiles)
    elif key == 'specs': return LastSpecificationLineFinder(sourceFiles)
    elif key == 'use': return LastUseLineFinder(sourceFiles)
    elif key == 'contains': return ContainsLineFinder(sourceFiles)
    elif key == 'all': return AllLineFinder(sourceFiles)
    else: raise NotImplementedError('LineNumberFinder not yet implemented: ' + str(key))

USE_PRINTERS = {'modules': 'module names',
                'files': 'file pathes'} 
def usePrinter(key, sourceFiles, excludeModules):
    if key not in USE_PRINTERS: raise KeyError('No such UsePrinter: ' + str(key))
    elif key == 'modules': return UsedModuleNamePrinter(sourceFiles, excludeModules)
    elif key == 'files': return UsedFileNamePrinter(sourceFiles, excludeModules)
    else: raise NotImplementedError('UsePrinter not yet implemented: ' + str(key))

def optionHelp(helps):
    return ", ".join('%s: %s' % (key, string) for key, string in helps.items())

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
    argParser.add_argument('module', help='Module name');
    argParser.add_argument('subroutine', nargs='?', default=None, help='Subroutine or function name');
    return argParser.parse_args();

def main():
    args = parseArguments()
    config = loadFortranCallGraphConfiguration(args.configFile)
    if config is None:
        exit(3)

    graphBuilder = GNUx86AssemblerCallGraphBuilder(config[CFG_ASSEMBLER_DIRS], config[CFG_SPECIAL_MODULE_FILES])
    if config[CFG_CACHE_DIR]:
        graphBuilder = CachedAssemblerCallGraphBuilder(config[CFG_CACHE_DIR], graphBuilder)
    sourceFiles = SourceFiles(config[CFG_SOURCE_DIRS], config[CFG_SPECIAL_MODULE_FILES], config[CFG_SOURCE_FILES_PREPROCESSED])
    excludeModules = config[CFG_EXCLUDE_MODULES]
    ignoreGlobalsFromModules = config[CFG_IGNORE_GLOBALS_FROM_MODULES]
    ignoreDerivedTypes = config[CFG_IGNORE_DERIVED_TYPES]
    abstractTypes = config[CFG_ABSTRACT_TYPES]
    
    moduleName = args.module
    subroutineName = args.subroutine
    subroutineFullName = None
    sourceFileName = None
    
    if not subroutineName:
        if SubroutineFullName.validFullName(moduleName):
            subroutineFullName = SubroutineFullName(moduleName)
        elif args.dump is not None or args.line is not None:
            if not SubroutineFullName.validIdentifier(moduleName):
                if args.dump is not None and moduleName.lower().endswith('.f90'):
                    sourceFileName = moduleName
                else:
                    printErrorAndExit(3, 'Invalid Module name!')
        else: 
            printErrorAndExit(4, 'Missing Subroutine name!')
    elif SubroutineFullName.validParts(moduleName, subroutineName):
        subroutineFullName = SubroutineFullName.fromParts(moduleName, subroutineName)
    else:
        printErrorAndExit(5, 'Invalid Module and/or Subroutine name!')
        
    if subroutineFullName is not None and not sourceFiles.existsSubroutine(subroutineFullName):
        printErrorAndExit(6, 'Subroutine ' + str(subroutineFullName) + ' not found!')
    elif sourceFileName is not None and not sourceFiles.existsSourceFile(sourceFileName):
        printErrorAndExit(7, 'Source file ' + sourceFileName + ' not found!')
    elif subroutineFullName is None and sourceFileName is None and not sourceFiles.existsModule(moduleName):
        printErrorAndExit(8, 'Module ' + moduleName + ' not found!')
        
    ignoreRegex = None
    if args.ignore is not None:
        try:
            ignoreRegex = re.compile(args.ignore)
        except re.error:
            printErrorAndExit(9, 'Invalid regular expression for -i/--ignore option!')
        
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
        trackerSettings = VariableTrackerSettings()
        trackerSettings.excludeModules = excludeModules 
        trackerSettings.ignoreGlobalsFromModules = ignoreGlobalsFromModules 
        trackerSettings.ignoreDerivedTypes = ignoreDerivedTypes 
        trackerSettings.abstractTypes = abstractTypes 
        analysis = graphAnalysis(args.analysis, sourceFiles, trackerSettings, graphBuilder)
        if args.analysis == 'arguments' and args.variable is not None:
            analysis.setVariableName(args.variable)
        elif args.analysis == 'result':
            function = sourceFiles.findSubroutine(subroutineFullName)
            if not function.isFunction():
                printErrorAndExit(10, 'Subroutine ' + str(subroutineFullName) + ' not a function!')
            analysis.setVariableName(function.getResultVariable().getName())
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
        finder = lineNumberFinder(args.line, sourceFiles)
        if args.quiet:
            finder.setMinimalOutput(True)
        if subroutineFullName is not None:
            finder.printLineNumber(subroutineFullName)
        else:
            finder.printLineNumber(moduleName)
    elif args.use is not None:
        printer = usePrinter(args.use, sourceFiles, excludeModules)
        printer.printUses(subroutineFullName)

if __name__ == "__main__":
    main()