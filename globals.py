#coding=utf8

import re
from assertions import assertType, assertTypeAll, REGEX_TYPE
from supertypes import CallGraphAnalyzer, CallGraphBuilder
from source import SourceFiles, VariableReference, SourceFile
from callgraph import CallGraph
from trackvariable import VariableTracker, VariableTrackerSettings
from usetraversal import UseTraversal
from typefinder import TypeCollection
from printout import printLine, printWarning

class GlobalVariableTracker(CallGraphAnalyzer):

    __routineWarnings = set()
    __moduleWarnings = set()

    def __init__(self, sourceFiles, settings, interfaces = None, types = None, callGraphBuilder = None):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(interfaces, 'interfaces', dict, True)
        assertType(types, 'types', TypeCollection, True)
        assertType(callGraphBuilder, 'callGraphBuilder', CallGraphBuilder, True)
        
        assertType(settings, 'settings', VariableTrackerSettings)
        assertTypeAll(settings.excludeModules, 'settings.excludeModules', str)
        assertTypeAll(settings.ignoreGlobalsFromModules, 'settings.ignoreGlobalsFromModules', str)
        assertTypeAll(settings.ignoredTypes, 'settings.ignoredTypes', str)        
        assertType(settings.abstractTypes, 'settings.abstractTypes', dict) 
        assertType(settings.ignoreSubroutinesRegex, 'settings.ignoreSubroutinesRegex', REGEX_TYPE, True)  
        assertType(settings.minimalOutput, 'settings.minimalOutput', bool) 
        assertType(settings.pointersOnly, 'settings.pointersOnly', bool) 
        
        super(GlobalVariableTracker, self).__init__()
        
        self.__sourceFiles = sourceFiles
        self.__settings = settings
        self.__callGraph = None
        self.__interfaces = interfaces
        self.__types = types
        self.__variableTracker = None
        self.__usedVariableLists = dict()
        self.__callGraphBuilder = callGraphBuilder
        GlobalVariableTracker.__routineWarnings = set()
    
    def analyzeCallgraph(self, callGraph):
        '''Analyzes the given Callgraph. Finds all references to global variables.'''
        assertType(callGraph, 'callGraph', CallGraph)
        
        variableReferences = self.trackGlobalVariables(callGraph)
        
        if not self.__settings.minimalOutput:
            for variableReference in variableReferences:
                if not self.__settings.pointersOnly or variableReference.isPointer():
                    printLine(variableReference)
        else:
            for variableReference in variableReferences:
                if not self.__settings.pointersOnly or variableReference.isPointer():
                    if variableReference.getDeclaredIn() is not None:
                        declaredIn = ' {' + variableReference.getDeclaredIn().getName() + '}'
                    else:
                        declaredIn = ''
                        
                    printLine(variableReference.getExpression() + declaredIn)
                    
    def trackGlobalVariables(self, callGraph):
        assertType(callGraph, 'callGraph', CallGraph)
        
        self.__callGraph = callGraph
        
        if self.__interfaces is None or self.__types is None:
            useTraversal = UseTraversal(self.__sourceFiles, self.__settings.excludeModules, self.__settings.abstractTypes)
            useTraversal.parseModules(callGraph.getRoot())
            self.__interfaces = useTraversal.getInterfaces()
            self.__types = useTraversal.getTypes()
        
        self.__variableTracker = VariableTracker(self.__sourceFiles, self.__settings, self.__interfaces, self.__types, self.__callGraphBuilder)
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
        if self.__settings.matchIgnoreSubroutineRegex(subroutineName) or (subroutineName.getModuleName().lower in self.__settings.excludeModules):
            return set()
        
        moduleName = subroutineName.getModuleName()
        variables = self.__getModuleVariables(moduleName)
        variables.update(self.__getAllUsedVariables(subroutineName))
        
        callGraph = self.__callGraph.extractSubgraph(subroutineName) 
        normalVariables = set()
        typeVariables = set()
        for variable in variables.values():
            if not variable.isParameter():
                if variable.hasDerivedType() and variable.getDerivedTypeName() not in self.__settings.fullTypes:
                    typeVariables.add(variable)
                else:
                    normalVariables.add(variable)
        self.__variableTracker.clearOutAssignments()
        typeVariableReferences = set(self.__variableTracker.trackVariables(typeVariables, callGraph))
        funtionResultOriginalReferences = []
        outVarAssignments = []
        for aliasVar, originalReference in self.__variableTracker.getOutAssignments():
            if aliasVar.isFunctionResult():
                funtionResultOriginalReferences.append(originalReference)
            elif aliasVar.isOutArgument():
                outVarAssignments.append((aliasVar, originalReference))
        if funtionResultOriginalReferences:
            typeVariableReferences.update(self.__trackFunctionResult(subroutineName, funtionResultOriginalReferences))
        if outVarAssignments:
            typeVariableReferences.update(self.__trackOutVariables(subroutineName, outVarAssignments))
        
        normalVariableReferences = set(self.__trackVariables(normalVariables, subroutineName))
        
        references = set()
        for reference in normalVariableReferences | typeVariableReferences:
            variable = reference.getLevel0Variable()
            if variable.isAlias():
                reference.setLevel0Variable(variable.getOriginal())
            references.add(reference)

        return references
    
    def __trackFunctionResult(self, functionName, originalReferences):
        variables = [originalReferences[0].getLevelNVariable().getAlias(functionName.getSimpleName())] # TODO type-bound-procedures???
        for interface in self.__interfaces.values():
            if functionName.getSimpleName() in interface:
                variables.append(originalReferences[0].getLevelNVariable().getAlias(interface.getName()))
        tracker = VariableTracker(self.__sourceFiles, self.__settings, self.__interfaces, self.__types, self.__callGraphBuilder)
        variableReferences = set()
                            
        for callerName in self.__callGraph.getCallers(functionName):
            callGraph = self.__callGraph.extractSubgraph(callerName)
            functionReferences = set(tracker.trackVariables(variables, callGraph))
            if not functionReferences:
                for originalReference in originalReferences:
                    functionReferences.update(self.__analyzeCallingSubroutineForTypeBoundFunctionResult(callerName, functionName, originalReference))
            for functionReference in functionReferences:
                for originalReference in originalReferences:
                    variableReference = functionReference.cleanCopy()
                    variableReference.setLevel0Variable(originalReference.getLevel0Variable(), originalReference.getMembers())
                    variableReferences.add(variableReference)
            
        return variableReferences
    
    def __trackOutVariables(self, calleeName, assignments):
        variableReferences = set()
        for callerName in self.__callGraph.getCallers(calleeName):
            variableReferences.update(self.__analyzeCallingSubroutineForOutVars(callerName, calleeName, assignments))
                
        return variableReferences
    
    def __analyzeCallingSubroutineForTypeBoundFunctionResult(self, callerName, calleeName, originalReference):
        calleeNameAlternatives = []
        for typE in self.__types:
            if typE.containsSubroutine(calleeName.getSimpleName()):
                calleeNameAlternatives.append(typE.getSubroutineAlias(calleeName.getSimpleName()).lower())
        
        caller = self.__findSubroutine(callerName)
        callee = self.__findSubroutine(calleeName)
        callGraph = self.__callGraph.extractSubgraph(callerName) 
        
        variableReferences = set()
        if caller is not None and callee is not None:
            for lineNumber, statement, _ in caller.getStatements():
                tracker = VariableTracker(self.__sourceFiles, self.__settings, self.__interfaces, self.__types, self.__callGraphBuilder)
                for calleeNameAlternative in calleeNameAlternatives:
                    assignmentRegEx = re.compile(r'^(?P<alias>[a-z0-9_]+)(\(.*\))?\s*\=\>?\s*.*%' + calleeNameAlternative + '\s*\((?P<arguments>.*)\).*$', re.IGNORECASE);
                    assignmentRegExMatch = assignmentRegEx.match(statement)
                    if assignmentRegExMatch is not None:
                        assemblerLineNumber = lineNumber - caller.getSourceFile().getPreprocessorOffset(lineNumber)
                        if calleeName in self.__callGraph.findNextCalleesFromLine(callerName, assemblerLineNumber):
                            alias = assignmentRegExMatch.group('alias')
                            variableReferences.update(tracker.trackAssignment(alias, originalReference, callGraph, lineNumber))
                variableReferences.update(self.__trackOutVariables(callerName, tracker.getOutAssignments()))
            
        return variableReferences
    
    def __analyzeCallingSubroutineForOutVars(self, callerName, calleeName, assignments):
        calleeNameAlternatives = [calleeName.getSimpleName().lower()]
        for interface in self.__interfaces.values():
            if calleeName.getSimpleName() in interface:
                calleeNameAlternatives.append(interface.getName().lower())
        for typE in self.__types:
            if typE.containsSubroutine(calleeName.getSimpleName()):
                calleeNameAlternatives.append(typE.getSubroutineAlias(calleeName.getSimpleName()).lower())
        
        caller = self.__findSubroutine(callerName)
        callee = self.__findSubroutine(calleeName)
        callGraph = self.__callGraph.extractSubgraph(callerName) 

        variableReferences = set()
        if caller is not None and callee is not None:
            for lineNumber, statement, _ in caller.getStatements():
                tracker = VariableTracker(self.__sourceFiles, self.__settings, self.__interfaces, self.__types, self.__callGraphBuilder)
                for calleeNameAlternative in calleeNameAlternatives:
                    procedureRegEx = re.compile(r'^.*(?P<prefix>[^a-z0-9_]+)' + calleeNameAlternative + '\s*\((?P<arguments>.*)\).*$', re.IGNORECASE);
                    procedureRegExMatch = procedureRegEx.match(statement)
                    if procedureRegExMatch is not None:
                        isCall = True
                        if calleeNameAlternative != calleeName.getSimpleName():
                            assemblerLineNumber = lineNumber - caller.getSourceFile().getPreprocessorOffset(lineNumber)
                            isCall = calleeName in self.__callGraph.findNextCalleesFromLine(callerName, assemblerLineNumber)
                        if isCall:
                            arguments = procedureRegExMatch.group('arguments')
                            arguments = SourceFile.removeUnimportantParentheses(arguments)
                            arguments = arguments.split(',')
                            typeBound = procedureRegExMatch.group('prefix')[-1:] == '%'
                            for alias, originalReference in assignments:
                                aliasPosition = callee.getArgumentPosition(alias)
                                if typeBound:
                                    aliasPosition -= 1
                                if aliasPosition >= 0 and aliasPosition < len(arguments):
                                    argument = arguments[aliasPosition]
                                    variableReferences.update(tracker.trackAssignment(argument, originalReference, callGraph, lineNumber))
                            break
                variableReferences.update(self.__trackOutVariables(callerName, tracker.getOutAssignments()))
            
        return variableReferences
        
    def __trackVariables(self, variables, subroutineName):

        variableReferences = [];
        for variable in variables:
            variableReferences += self.__trackVariable(variable, subroutineName);
        
        return variableReferences;
    
    def __trackVariable(self, variable, subroutineName):
        variableName = variable.getName()
        accessRegEx = re.compile(r'^(.*[^a-z0-9_])?' + variableName + r'([^a-z0-9_].*)?', re.IGNORECASE);
        tracker = VariableTracker(self.__sourceFiles, self.__settings, self.__interfaces, self.__types, self.__callGraphBuilder)
        
        variableReferences = [];
        subroutine = self.__findSubroutine(subroutineName);
        if subroutine is not None:
            for lineNumber, statement, _ in subroutine.getStatements():
                if accessRegEx.match(statement) is not None:
                    variableReference = VariableReference(variableName, subroutineName, lineNumber, variable)
                    if variable.hasDerivedType() and variable.getDerivedTypeName() in self.__settings.fullTypes:
                        variableReferences += list(tracker.createReferencesForFullTypeVariable(variableReference, subroutineName, lineNumber))
                    else:
                        variableReferences.append(variableReference)
                        
                
        return variableReferences
    
    def __getModuleVariables(self, moduleName):
        moduleName = moduleName.lower()
        if moduleName in self.__settings.ignoreGlobalsFromModules or moduleName.lower() in self.__settings.excludeModules:
            return dict()

        module = self.__findModule(moduleName)
        if module is None:
            return dict()

        if self.__settings.ignoreSubroutinesRegex is not None:
            moduleVariables = dict()
            for name, var in module.getVariables().items():
                if not self.__settings.matchIgnoreSubroutineRegex(name):
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
                if moduleName not in self.__settings.ignoreGlobalsFromModules:
                    moduleVariables = self.__getModuleVariables(moduleName)
                    importList = useOnlyRegExMatch.group('importlist').split(',')
                    importList = [i.strip() for i in importList]
                    for imported in importList:
                        names = imported.split('=>')
                        names = [n.strip() for n in names]
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
                    if moduleName not in self.__settings.ignoreGlobalsFromModules:
                        usedVariables.update(self.__getModuleVariables(moduleName))

        return usedVariables

    def __findSubroutine(self, subroutineName):
        subroutine = self.__sourceFiles.findSubroutine(subroutineName);
        if subroutine is None and subroutineName not in GlobalVariableTracker.__routineWarnings:
            GlobalVariableTracker.__routineWarnings.add(subroutineName)
            printWarning('Routine not found: ' + str(subroutineName), 'GlobalVariableTracker')
            
        return subroutine

    def __findModule(self, moduleName):
        module = self.__sourceFiles.findModule(moduleName);
        if module is None and moduleName not in GlobalVariableTracker.__moduleWarnings:
            GlobalVariableTracker.__moduleWarnings.add(moduleName)
            printWarning('Module not found: ' + str(moduleName), 'GlobalVariableTracker')
            
        return module
                