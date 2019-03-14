#coding=utf8

from assertions import assertType
from source import SourceFiles
from callgraph import CallGraph
from supertypes import CallGraphAnalyzer, CallGraphBuilder
from trackvariable import VariableTracker, VariableTrackerSettings
from globals import GlobalVariableTracker
from usetraversal import UseTraversal

class AllVariablesCallGraphAnalysis(CallGraphAnalyzer):

    def __init__(self, sourceFiles, settings, callGraphBuilder = None):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(settings, 'settings', VariableTrackerSettings)
        assertType(callGraphBuilder, 'callGraphBuilder', CallGraphBuilder, True)

        super(AllVariablesCallGraphAnalysis, self).__init__()

        self.__sourceFiles = sourceFiles
        self.__settings = settings
        self.__callGraphBuilder = callGraphBuilder
    
    def analyzeCallgraph(self, callGraph, quiet = False):  # @UnusedVariable
        'Analyzes the given Callgraph. Finds all references to all input variables.'
        assertType(callGraph, 'callGraph', CallGraph) 

        useTraversal = UseTraversal(self.__sourceFiles, self.__settings.excludeModules, self.__settings.abstractTypes)
        useTraversal.parseModules(callGraph.getRoot())
        interfaces = useTraversal.getInterfaces()
        types = useTraversal.getTypes()        
        
        argumentTracker = VariableTracker(self.__sourceFiles, self.__settings, interfaces, types, callGraphBuilder = self.__callGraphBuilder)
        argumentTracker.analyzeCallgraph(callGraph)
        
        function = self.__sourceFiles.findSubroutine(callGraph.getRoot())
        if function.isFunction():
            argumentTracker.setVariable(function.getResultVariable())
            argumentTracker.analyzeCallgraph(callGraph)
        
        globalTracker = GlobalVariableTracker(self.__sourceFiles, self.__settings, interfaces, types, callGraphBuilder = self.__callGraphBuilder)
        globalTracker.analyzeCallgraph(callGraph)
