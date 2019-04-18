from assertions import assertType
from callgraph import CallGraph
from supertypes import CallGraphPrinter
from printout import printWarning, printLines
from source import SourceFiles

class SubroutineListingCallGraphPrinter(CallGraphPrinter):

    def printCallGraph(self, callGraph):
        assertType(callGraph, 'callGraph', CallGraph)
        
        subroutinesNames = [];
        for subroutine in callGraph.getAllSubroutineNames():
            if self._ignoreRegex is None or not self._ignoreRegex.match(callGraph.getRoot().getSimpleName()):
                subroutinesNames.append(str(subroutine));
            
        printLines(sorted(subroutinesNames))

class ModuleListingCallGraphPrinter(CallGraphPrinter):

    def printCallGraph(self, callGraph):
        assertType(callGraph, 'callGraph', CallGraph)
        
        moduleNames = set();
        for subroutine in callGraph.getAllSubroutineNames():
            moduleNames.add(subroutine.getModuleName());
            
        printLines(sorted(moduleNames))

class FileListingCallGraphPrinter(CallGraphPrinter):

    def __init__(self, sourceFiles):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        self.__sourceFiles = sourceFiles

    def printCallGraph(self, callGraph):
        assertType(callGraph, 'callGraph', CallGraph)
        
        files = set();
        for subroutineName in callGraph.getAllSubroutineNames():
            subroutine = self.__sourceFiles.findSubroutine(subroutineName)
            if subroutine is not None:
                path = self.__sourceFiles.getRelativePath(subroutine.getSourceFile())
                files.add(path)
            else:
                printWarning('File not found for subroutine ' + str(subroutineName), location='FileListingCallGraphPrinter')
            
        printLines(sorted(files))
