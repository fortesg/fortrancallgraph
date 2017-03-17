from utils import assertType
from callgraph import CallGraph
from supertypes import CallGraphPrinter

class TreeLikeCallGraphPrinter(CallGraphPrinter):

    def printCallGraph(self, callGraph):
        assertType(callGraph, 'callGraph', CallGraph)
        
        self.__printCallGraphRecursive(callGraph.getRoot(), callGraph, ());
        
    def __printCallGraphRecursive(self, rootSubroutine, callGraph, callStack):
        if self._ignoreRegex is not None and self._ignoreRegex.match(rootSubroutine.getSimpleName()):
            return
        
        level = len(callStack);
        if rootSubroutine in callStack:
            print self.__getTreeLine(level, rootSubroutine) + " [RECURSIVE]";
            return;
        else:
            print self.__getTreeLine(level, rootSubroutine);
            callStack = callStack + (rootSubroutine,);
            for calledSubroutine in callGraph.getCallees(rootSubroutine):
                    self.__printCallGraphRecursive(calledSubroutine, callGraph, callStack);
    
    def __getTreeLine(self, level, subroutine):
        return str(level) + ' ' + (level * '  ') + str(subroutine);