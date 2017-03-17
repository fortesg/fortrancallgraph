from utils import assertType, REGEX_TYPE
from source import SubroutineFullName, Subroutine, SourceFiles
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

    def setIgnoreRegex(self, regex):
        'Sets the regular expression for ignored routines'
        assertType(regex, 'regex', [str, REGEX_TYPE], True) 
        if isinstance(regex, str):
            self._ignoreRegex = re.compile(regex)
        else:
            self._ignoreRegex = regex

    def printCallGraph(self, callGraph):
        'Prints the given CallGraph to the standard output. The format is defined by subclasses.'
        assertType(callGraph, 'callGraph', CallGraph) 
        raise NotImplementedError()
    
    
class CallGraphAnalyzer(object):
    
    def __init__(self):
        self._ignoreRegex = None;
        self._minimalOutput = False;

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
        
    def printLineNumber(self, subroutineName):
        'Print one specific line number for the Subroutine. What kind of line number is defined by subclasses.'
        assertType(subroutineName, 'subroutineName', SubroutineFullName)
        subroutine = self.__sourceFiles.findSubroutine(subroutineName);
        if subroutine is None:
            raise Exception('Routine ' + str(subroutineName) + 'not found!'); 
        self._printLineNumber(subroutine)
        
    def _printLineNumber(self, subroutine):
        'Print one specific line number for the Subroutine. What kind of line number is defined by subclasses.'
        assertType(subroutine, 'subroutine', Subroutine) 
        
        line = self._findLineNumber(subroutine)
        print str(line),
        if line >= 0 and not self._minimalOutput:
            print ' | ' + subroutine.getLine(line).rstrip(),
        print
    
    def _findLineNumber(self, container):
        'Finds one specific line number for the Subroutine. What kind of line number is defined by subclasses.'
        assertType(container, 'container', Subroutine) 
    
        raise NotImplementedError()    

class UseTraversalPassenger(object):
    
    def reset(self):
        raise NotImplementedError()
    
    def getResult(self):
        raise NotImplementedError()
    
    def parseStatement(self, i, statement, j, moduleName):
        assertType(i, 'i', int) 
        assertType(statement, 'statement', str) 
        assertType(j, 'j', int)
        assertType(moduleName, 'moduleName', str)
        raise NotImplementedError() 