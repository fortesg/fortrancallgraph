from utils import assertType
from callgraph import CallGraph
from supertypes import CallGraphPrinter

class SubroutineListingCallGraphPrinter(CallGraphPrinter):

    def printCallGraph(self, callGraph):
        assertType(callGraph, 'callGraph', CallGraph)
        
        subroutinesNames = [];
        for subroutine in callGraph.getAllSubroutineNames():
            if self._ignoreRegex is None or not self._ignoreRegex.match(rootSubroutine.getName()):
                subroutinesNames.append(str(subroutine));
            
        for subroutineName in sorted(subroutinesNames):
            print subroutineName;

class ModuleListingCallGraphPrinter(CallGraphPrinter):

    def printCallGraph(self, callGraph):
        assertType(callGraph, 'callGraph', CallGraph)
        
        moduleNames = set();
        for subroutine in callGraph.getAllSubroutineNames():
            moduleNames.add(subroutine.getModuleName());
            
        for moduleName in sorted(moduleNames):
            print moduleName;
