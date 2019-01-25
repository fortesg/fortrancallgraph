from assertions import assertType
from callgraph import CallGraph
from supertypes import CallGraphPrinter
from printout import printLine

class TreeLikeCallGraphPrinter(CallGraphPrinter):

    def printCallGraph(self, callGraph):
        assertType(callGraph, 'callGraph', CallGraph)
        
        self.__printCallGraphRecursive(callGraph.getRoot(), callGraph, ());
        
    def __printCallGraphRecursive(self, rootSubroutine, callGraph, callStack):
        if self._ignoreRegex is not None and self._ignoreRegex.match(rootSubroutine.getSimpleName()):
            return
        
        level = len(callStack);
        if self._maxLevel is None or level <= self._maxLevel:
            if rootSubroutine in callStack:
                printLine(self.__getTreeLine(level, rootSubroutine) + " [RECURSIVE]")
                return;
            else:
                printLine(self.__getTreeLine(level, rootSubroutine))
                callStack = callStack + (rootSubroutine,);
                for calledSubroutine in callGraph.getCallees(rootSubroutine):
                        self.__printCallGraphRecursive(calledSubroutine, callGraph, callStack);

    def __getTreeLine(self, level, subroutine):
        return str(level) + ' ' + (level * '  ') + str(subroutine);