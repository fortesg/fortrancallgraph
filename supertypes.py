from utils import assertType, REGEX_TYPE, assertTypeAll
from source import SubroutineFullName, Subroutine, SourceFiles, Module
from callgraph import CallGraph
import re

class CallGraphBuilder(object):

    def buildCallGraph(self, rootSubroutine):
        assertType(rootSubroutine, 'rootSubroutine', SubroutineFullName)      
        raise NotImplementedError()
    
    def getModuleFilePath(self, moduleName):    
        assertType(moduleName, 'moduleName', str)
        raise NotImplementedError()

class CallGraphPrinter(object):
    
    def __init__(self):
        self._ignoreRegex = None;
        self._maxLevel = None;

    def setIgnoreRegex(self, regex):
        'Sets the regular expression for ignored routines'
        assertType(regex, 'regex', [str, REGEX_TYPE], True) 
        if isinstance(regex, str):
            self._ignoreRegex = re.compile(regex)
        else:
            self._ignoreRegex = regex
            
    def setMaxLevel(self, level):
        'Sets the maximum depth to which the graph is printed. Negative values mean no limit.'
        assertType(level, 'level', int)
        if level < 0:
            self._maxLevel = None
        else:
            self._maxLevel = level

    def printCallGraph(self, callGraph):
        'Prints the given CallGraph to the standard output. The format is defined by subclasses.'
        assertType(callGraph, 'callGraph', CallGraph) 
        raise NotImplementedError()
    
    
class CallGraphAnalyzer(object):
    
    def __init__(self):
        self._ignoreRegex = None;
        self._minimalOutput = False;
        self._pointersOnly = False;

    def setIgnoreRegex(self, regex):
        'Sets the regular expression for ignored routines and/or global variables'
        assertType(regex, 'regex', [str, REGEX_TYPE], True) 
        if isinstance(regex, str):
            self._ignoreRegex = re.compile(regex)
        else:
            self._ignoreRegex = regex
    
    def setMinimalOutput(self, enabled):   
        assertType(enabled, 'enabled', bool)
        self._minimalOutput = enabled; 
    
    def setPointersOnly(self, enabled):   
        assertType(enabled, 'enabled', bool)
        self._pointersOnly = enabled; 

    def analyzeCallgraph(self, callGraph):
        'Analyzes the given Callgraph. The kind of analysis is defined by subclasses.'
        assertType(callGraph, 'callGraph', CallGraph) 
        raise NotImplementedError()
    
    
class SourceDumper(object):

    def __init__(self, sourceFiles):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        self.__sourceFiles = sourceFiles;
        self._printLineNumbers = False;
        
    def setPrintLineNumbers(self, enabled):
        assertType(enabled, 'enabled', bool)
        self._printLineNumbers = enabled;

    def dumpSubroutine(self, subroutineName):
        'Dumps the given Subroutine. The kind of dump is defined by subclasses.'
        assertType(subroutineName, 'subroutineName', SubroutineFullName) 
        raise NotImplementedError()    
    
    def _findSubroutine(self, subroutineName):
        subroutine = self.__sourceFiles.findSubroutine(subroutineName);
        if subroutine is None:
            raise Exception('Routine ' + str(subroutineName) + 'not found!');
        return subroutine

    def dumpModule(self, moduleName):
        'Dumps the given module. The kind of dump is defined by subclasses.'
        assertType(moduleName, 'moduleName', str) 
        raise NotImplementedError()    
    
    def _findModule(self, moduleName):
        module = self.__sourceFiles.findModule(moduleName)
        if module is None:
            raise Exception('Module ' + str(moduleName) + 'not found!');
        return module

    def dumpSourceFile(self, fileName):
        'Dumps the given source file. The kind of dump is defined by subclasses.'
        assertType(fileName, 'fileName', str) 
        raise NotImplementedError()    
    
    def _findSourceFile(self, fileName):
        return self.__sourceFiles.findSourceFile(fileName);
    
    
class LineNumberFinder(object):
    
    def __init__(self, sourceFiles):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        self.__sourceFiles = sourceFiles;
        self._minimalOutput = False;
        
    def setMinimalOutput(self, enabled):   
        assertType(enabled, 'enabled', bool)
        self._minimalOutput = enabled; 
        
    def printLineNumber(self, name):
        'Print one specific line number for the Subroutine or Module. What kind of line number is defined by subclasses.'
        assertType(name, 'name', [SubroutineFullName, str])

        if isinstance(name, SubroutineFullName):        
            container = self.__sourceFiles.findSubroutine(name);
            if container is None:
                raise Exception('Routine ' + str(name) + 'not found!');
        else:
            container = self.__sourceFiles.findModule(name)
            if container is None:
                raise Exception('Module ' + str(name) + 'not found!');
         
        self._printLineNumber(container)
        
    def _printLineNumber(self, container):
        'Print one specific line number for the Subroutine or Module. What kind of line number is defined by subclasses.'
        assertType(container, 'container', [Subroutine, Module]) 
        
        line = self._findLineNumber(container)
        print str(line),
        if line >= 0 and not self._minimalOutput:
            print ' | ' + container.getLine(line).rstrip(),
        print
    
    def _findLineNumber(self, container):
        'Finds one specific line number for the Subroutine or Module. What kind of line number is defined by subclasses.'
        assertType(container, 'container', [Subroutine, Module]) 
    
        raise NotImplementedError()    


class UseTraversalPassenger(object):
    
    def reset(self):
        raise NotImplementedError()
    
    def getResult(self):
        raise NotImplementedError()
    
    def parseStatement(self, i, statement, j, module):
        assertType(i, 'i', int) 
        assertType(statement, 'statement', str) 
        assertType(j, 'j', int)
        assertType(module, 'module', Module)
        raise NotImplementedError() 
 
 
class UsePrinters(object):
         
    def __init__(self, sourceFiles, excludeModules = []):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertTypeAll(excludeModules, 'excludeModules', str)
         
        self._sourceFiles = sourceFiles
        self._excludeModules = excludeModules
         
    def printUses(self, rootSubroutine):
        assertType(rootSubroutine, 'rootSubroutine', SubroutineFullName)
        raise NotImplementedError() 
