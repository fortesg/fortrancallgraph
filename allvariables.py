#coding=utf8

from assertions import assertType, assertTypeAll
from source import SourceFiles
from callgraph import CallGraph
from supertypes import CallGraphAnalyzer
from trackvariable import VariableTracker
from globals import GlobalVariableTracker
from usetraversal import UseTraversal

class AllVariablesCallGraphAnalysis(CallGraphAnalyzer):

    def __init__(self, sourceFiles, excludeModules = [], ignoredModulesForGlobals = [], ignoredTypes = []):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertTypeAll(excludeModules, 'excludeModules', str)
        assertTypeAll(ignoredModulesForGlobals, 'ignoredModulesForGlobals', str)
        assertTypeAll(ignoredTypes, 'ignoredTypes', str)

        super(AllVariablesCallGraphAnalysis, self).__init__()

        self.__sourceFiles = sourceFiles
        self.__excludeModules = excludeModules
        self.__ignoredModulesForGlobals = ignoredModulesForGlobals
        self.__ignoredTypes = ignoredTypes
    
    def analyzeCallgraph(self, callGraph, quiet = False):
        'Analyzes the given Callgraph. Finds all references to all input variables.'
        assertType(callGraph, 'callGraph', CallGraph) 

        useTraversal = UseTraversal(self.__sourceFiles, self.__excludeModules)
        useTraversal.parseModules(callGraph.getRoot())
        interfaces = useTraversal.getInterfaces()
        types = useTraversal.getTypes()        
        
        argumentTracker = VariableTracker(self.__sourceFiles, self.__excludeModules, self.__ignoredTypes, interfaces, types)
        argumentTracker.setIgnoreRegex(self._ignoreRegex)
        argumentTracker.setMinimalOutput(self._minimalOutput)
        argumentTracker.setPointersOnly(self._pointersOnly)
        argumentTracker.analyzeCallgraph(callGraph)
        
        function = self.__sourceFiles.findSubroutine(callGraph.getRoot())
        if function.isFunction():
            argumentTracker.setVariable(function.getResultVariable())
            argumentTracker.analyzeCallgraph(callGraph)
        
        globalTracker = GlobalVariableTracker(self.__sourceFiles, self.__excludeModules, self.__ignoredModulesForGlobals, self.__ignoredTypes, interfaces, types)
        globalTracker.setIgnoreRegex(self._ignoreRegex)
        globalTracker.setMinimalOutput(self._minimalOutput)
        globalTracker.setPointersOnly(self._pointersOnly)
        globalTracker.analyzeCallgraph(callGraph)
