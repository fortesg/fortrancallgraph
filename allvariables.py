#coding=utf8

from assertions import assertType, assertTypeAll
from source import SourceFiles
from callgraph import CallGraph
from supertypes import CallGraphAnalyzer, CallGraphBuilder
from trackvariable import VariableTracker
from globals import GlobalVariableTracker
from usetraversal import UseTraversal

class AllVariablesCallGraphAnalysis(CallGraphAnalyzer):

    def __init__(self, sourceFiles, excludeModules = [], ignoredModulesForGlobals = [], ignoredTypes = [], abstractTypes = {}, callGraphBuilder = None):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertTypeAll(excludeModules, 'excludeModules', str)
        assertTypeAll(ignoredModulesForGlobals, 'ignoredModulesForGlobals', str)
        assertTypeAll(ignoredTypes, 'ignoredTypes', str)
        assertType(abstractTypes, 'abstractTypes', dict)
        assertType(callGraphBuilder, 'callGraphBuilder', CallGraphBuilder, True)

        super(AllVariablesCallGraphAnalysis, self).__init__()

        self.__sourceFiles = sourceFiles
        self.__excludeModules = excludeModules
        self.__ignoredModulesForGlobals = ignoredModulesForGlobals
        self.__ignoredTypes = ignoredTypes
        self.__abstractTypes = abstractTypes
        self.__callGraphBuilder = callGraphBuilder
    
    def analyzeCallgraph(self, callGraph, quiet = False):  # @UnusedVariable
        'Analyzes the given Callgraph. Finds all references to all input variables.'
        assertType(callGraph, 'callGraph', CallGraph) 

        useTraversal = UseTraversal(self.__sourceFiles, self.__excludeModules, self.__abstractTypes)
        useTraversal.parseModules(callGraph.getRoot())
        interfaces = useTraversal.getInterfaces()
        types = useTraversal.getTypes()        
        
        argumentTracker = VariableTracker(self.__sourceFiles, self.__excludeModules, self.__ignoredTypes, interfaces, types, callGraphBuilder = self.__callGraphBuilder)
        argumentTracker.setIgnoreRegex(self._ignoreRegex)
        argumentTracker.setMinimalOutput(self._minimalOutput)
        argumentTracker.setPointersOnly(self._pointersOnly)
        argumentTracker.analyzeCallgraph(callGraph)
        
        function = self.__sourceFiles.findSubroutine(callGraph.getRoot())
        if function.isFunction():
            argumentTracker.setVariable(function.getResultVariable())
            argumentTracker.analyzeCallgraph(callGraph)
        
        globalTracker = GlobalVariableTracker(self.__sourceFiles, self.__excludeModules, self.__ignoredModulesForGlobals, self.__ignoredTypes, interfaces, types, callGraphBuilder = self.__callGraphBuilder)
        globalTracker.setIgnoreRegex(self._ignoreRegex)
        globalTracker.setMinimalOutput(self._minimalOutput)
        globalTracker.setPointersOnly(self._pointersOnly)
        globalTracker.analyzeCallgraph(callGraph)
