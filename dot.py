from assertions import assertType
from callgraph import CallGraph
from supertypes import CallGraphPrinter
from printout import printLine

class DotFormatCallGraphPrinter(CallGraphPrinter):

    def printCallGraph(self, callGraph):
        assertType(callGraph, 'callGraph', CallGraph)
        
        #TODO: Respect max. level 
        
        printLine('digraph callgraph {')
        for call in callGraph.getAllCalls():
            if self._ignoreRegex is None or not (self._ignoreRegex.match(call[0]) or self._ignoreRegex.match(call[1])): 
                printLine('  ' + str(call[0]) + ' -> ' + str(call[1]) + ';')  
        printLine('}')
