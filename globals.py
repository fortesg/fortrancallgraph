#coding=utf8

import sys
import re
from utils import assertType, assertTypeAll
from supertypes import CallGraphAnalyzer
from source import SourceFiles
from callgraph import CallGraph
from trackvariable import TrackVariableCallGraphAnalysis, VariableReference
from usetraversal import UseTraversal

class GlobalVariablesCallGraphAnalysis(CallGraphAnalyzer):

    __routineWarnings = set()
    __moduleWarnings = set()

    def __init__(self, sourceFiles, excludeModules = [], ignoredModules = [], ignoredTypes = [], interfaces = None, types = None):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertTypeAll(excludeModules, 'excludeModules', str)
        assertTypeAll(ignoredModules, 'ignoredModules', str)
        assertTypeAll(ignoredTypes, 'ignoredTypes', str)
        assertType(interfaces, 'interfaces', dict, True)
        assertType(types, 'types', dict, True)
        
        super(GlobalVariablesCallGraphAnalysis, self).__init__()
        
        self.__sourceFiles = sourceFiles;
        self.__callGraph = None
        self.__excludeModules = excludeModules
        self.__ignoredModules = ignoredModules;
        self.__ignoredTypes = ignoredTypes;
        self.__interfaces = interfaces;
        self.__types = types;
        self.__variableTracker = None
        self.__usedVariableLists = dict()
        GlobalVariablesCallGraphAnalysis.__routineWarnings = set()
    
    def analyzeCallgraph(self, callGraph):
        'Analyzes the given Callgraph. Finds all references to global variables.'
        if not isinstance(callGraph, CallGraph):
            raise TypeError("Argument callGraph must be of type CallGraph."); 
        
        variableReferences = self.trackGlobalVariables(callGraph)
        
        if not self._minimalOutput:
            for variableReference in variableReferences:
                print str(variableReference);
        else:
            for variableReference in variableReferences:
                if variableReference.getDeclaredIn() is not None:
                    declaredIn = ' {' + variableReference.getDeclaredIn() + '}'
                else:
                    declaredIn = ''
                    
                print variableReference.getExpression() + declaredIn;
                    
    def trackGlobalVariables(self, callGraph):
        assertType(callGraph, 'callGraph', CallGraph)
        
        self.__callGraph = callGraph
        
        if self.__interfaces is None or self.__types is None:
            useTraversal = UseTraversal(self.__sourceFiles, self.__excludeModules)
            useTraversal.parseModules(callGraph.getRoot())
            self.__interfaces = useTraversal.getInterfaces()
            self.__types = useTraversal.getTypes()
        
        self.__variableTracker = TrackVariableCallGraphAnalysis(self.__sourceFiles, self.__excludeModules, self.__ignoredTypes, self.__interfaces, self.__types)
        self.__variableTracker.setIgnoreRegex(self._ignoreRegex)
        variableReferences = self.__analyzeSubroutines(callGraph.getAllSubroutineNames());
        variableReferences = VariableReference.sort(variableReferences)
        self.__variableTracker = None
        self.__callGraph = None
        
        return variableReferences      
                
    def __analyzeSubroutines(self, subroutineNames):
        variableReferences = set();
        for subroutineName in subroutineNames:
            variableReferences.update(self.__analyzeSubroutine(subroutineName));
        
        return variableReferences;

    def __analyzeSubroutine(self, subroutineName):
        if self._ignoreRegex is not None and self._ignoreRegex.match(subroutineName.getSimpleName()) is not None:
            return set()
        
        moduleName = subroutineName.getModuleName()
        variables = self.__getModuleVariables(moduleName)
        variables.update(self.__getAllUsedVariables(subroutineName))
        
        callGraph = self.__callGraph.extractSubgraph(subroutineName) 
        normalVariables = set()
        typeVariables = set()
        for variable in variables.values():
            if not variable.isParameter():
                if variable.hasDerivedType():
                    typeVariables.add(variable)
                else:
                    normalVariables.add(variable)
        typeVariableReferences = set(self.__variableTracker.trackVariables(typeVariables, callGraph))
        normalVariableReferences = set(self.__trackVariables(normalVariables, subroutineName))
        
        references = set()
        for reference in normalVariableReferences | typeVariableReferences:
            variable = reference.getLevel0Variable()
            if variable.isAlias():
                reference.setLevel0Variable(variable.getOriginal())
            references.add(reference)

        return references
    
    def __trackVariables(self, variables, subroutineName):

        variableReferences = [];
        for variable in variables:
            variableReferences += self.__trackVariable(variable, subroutineName);
        
        return variableReferences;
    
    def __trackVariable(self, variable, subroutineName):
        variableName = variable.getName()
        accessRegEx = re.compile(r'^(.*[^a-z0-9_])?' + variableName + r'([^a-z0-9_].*)?', re.IGNORECASE);
        
        variableReferences = [];
        subroutine = self.__findSubroutine(subroutineName);
        if subroutine is not None:
            for lineNumber, statement, _ in subroutine.getStatements():
                if accessRegEx.match(statement) is not None:
                    variableReferences.append(VariableReference(variableName, subroutineName, lineNumber, variable))
                
        return variableReferences;  
    
    def __getModuleVariables(self, moduleName):
        if moduleName in self.__ignoredModules:
            return dict()

        module = self.__findModule(moduleName)
        if module is None:
            return dict()

        if self._ignoreRegex is not None:
            moduleVariables = dict()
            for name, var in module.getVariables().iteritems():
                if self._ignoreRegex.match(name) is None:
                    moduleVariables[name] = var
            return moduleVariables
        else:
            return module.getVariables()

    def __getAllUsedVariables(self, subroutineName):
        usedVariables = dict()
       
        container = self.__findSubroutine(subroutineName)
        while container is not None:
            usedVariables.update(self.__getUsedVariablesOfSubroutineContainer(container))
            container = container.getContainer()
            
        return usedVariables
    
    def __getUsedVariablesOfSubroutineContainer(self, container):
        name = container.getName()
        if name not in self.__usedVariableLists:
            self.__usedVariableLists[name] = self.__findUsedVariablesInSubroutineContainer(container)
                
        return self.__usedVariableLists[name]
    
    def __findUsedVariablesInSubroutineContainer(self, container):
        return self.__findUsedVariablesInStatements(container.getUseStatements())
    
    def __findUsedVariablesInStatements(self, statements):
        useAllRegEx = re.compile(r'^USE[\s\:]+(?P<modulename>[a-z0-9_]+)\s*(\,\s*)?$', re.IGNORECASE)
        useOnlyRegEx = re.compile(r'^USE[\s\:]+(?P<modulename>[a-z0-9_]+)\s*\,\s*ONLY\s*\:\s*(?P<importlist>.*)$', re.IGNORECASE)
        
        usedVariables = dict()        
        for _, statement, _ in statements:
            useOnlyRegExMatch = useOnlyRegEx.match(statement) 
            if useOnlyRegExMatch is not None:
                moduleName = useOnlyRegExMatch.group('modulename')
                if moduleName not in self.__ignoredModules:
                    moduleVariables = self.__getModuleVariables(moduleName)
                    importList = useOnlyRegExMatch.group('importlist').split(',')
                    importList = map(str.strip, importList)
                    for imported in importList:
                        names = imported.split('=>')
                        names = map(str.strip, names)
                        alias = names[0]
                        name = names[len(names) - 1]
                        name = name.lower()
                        if name in moduleVariables:
                            variable = moduleVariables[name]
                            if alias != name:
                                variable = variable.getAlias(alias)
                            usedVariables[alias] = variable
            else: 
                useAllRegExMatch = useAllRegEx.match(statement)
                if useAllRegExMatch is not None:
                    moduleName = useAllRegExMatch.group('modulename')
                    if moduleName not in self.__ignoredModules:
                        usedVariables = self.__getModuleVariables(moduleName)

        return usedVariables

    def __findSubroutine(self, subroutineName):
        subroutine = self.__sourceFiles.findSubroutine(subroutineName);
        if subroutine is None and subroutineName not in GlobalVariablesCallGraphAnalysis.__routineWarnings:
            GlobalVariablesCallGraphAnalysis.__routineWarnings.add(subroutineName)
            print >> sys.stderr, '*** WARNING [GlobalVariablesCallGraphAnalysis] Routine not found: ' + str(subroutineName) + ' ***';
            
        return subroutine

    def __findModule(self, moduleName):
        module = self.__sourceFiles.findModule(moduleName);
        if module is None and moduleName not in GlobalVariablesCallGraphAnalysis.__moduleWarnings:
            GlobalVariablesCallGraphAnalysis.__moduleWarnings.add(moduleName)
            print >> sys.stderr, '*** WARNING [GlobalVariablesCallGraphAnalysis] Module not found: ' + str(moduleName) + ' ***'
            
        return module
                