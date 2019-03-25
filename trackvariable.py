#coding=utf8

import re
from assertions import assertType, assertTypeAll, REGEX_TYPE
from supertypes import CallGraphAnalyzer, CallGraphBuilder
from source import SourceFiles, Variable, VariableReference, SubroutineFullName, InnerSubroutineName, SourceFile, SubroutineName
from callgraph import CallGraph
from usetraversal import UseTraversal
from typefinder import TypeCollection
from printout import printError, printLine, printWarning

class VariableTracker(CallGraphAnalyzer):

    __warnings = set()

    def __init__(self, sourceFiles, settings, interfaces = None, types = None, callGraphBuilder = None):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(interfaces, 'interfaces', dict, True)
        assertType(types, 'types', TypeCollection, True)
        assertType(callGraphBuilder, 'callGraphBuilder', CallGraphBuilder, True)
        
        assertType(settings, 'settings', VariableTrackerSettings)
        assertTypeAll(settings.excludeModules, 'settings.excludeModules', str)
        assertTypeAll(settings.ignoredTypes, 'settings.ignoredTypes', str)        
        assertType(settings.abstractTypes, 'settings.abstractTypes', dict)
        assertType(settings.ignoreSubroutinesRegex, 'settings.ignoreSubroutinesRegex', REGEX_TYPE, True)
        assertType(settings.minimalOutput, 'settings.minimalOutput', bool)
        assertType(settings.pointersOnly, 'settings.pointersOnly', bool)

        super(VariableTracker, self).__init__()

        self.__sourceFiles = sourceFiles;
        self.__settings = settings
        self.__callGraphBuilder = callGraphBuilder
        
        self.__variable = None;
        self.__variableName = None;
        self.__callGraph = None;
        self.__interfaces = interfaces
        self.__types = types
        self.__excludeFromRecursionVariables = set()
        self.__excludeFromRecursionRoutines = set()
        self.__outAssignments = set()
    
    def getOutAssignments(self):
        return self.__outAssignments
        
    def clearOutAssignments(self):
        self.__outAssignments = set()
    
    def setVariable(self, variable):
        assertType(variable, 'variable', Variable)
        
        self.__variable = variable;
    
    def setVariableName(self, variableName):
        assertType(variableName, 'variableName', str)
        
        self.__variableName = variableName;
    
    def analyzeCallgraph(self, callGraph, quiet = False):
        'Analyzes the given Callgraph. Finds all references to a defined variable.'
        assertType(callGraph, 'callGraph', CallGraph) 
        
        rootSubroutineName = callGraph.getRoot()
        rootSubroutine = self.__sourceFiles.findSubroutine(rootSubroutineName)
        if rootSubroutine is None:
            printError('Subroutine ' + str(rootSubroutineName), 'VariableTracker')
            return None;
        
        if self.__variable is not None:
            self.__variableName = self.__variable.getName()
            variables = {self.__variable}
        elif self.__variableName is not None:
            self.__variable = rootSubroutine.getVariable(self.__variableName)
            if self.__variable is None:
                printError('No variable with name: ' + self.__variableName, 'VariableTracker')
                return None;
            variables = {self.__variable}
        else:
            variables = rootSubroutine.getArguments()

        derivedTypeVariables = []
        primitiveTypeVariables =  []
        for variable in variables:
            if variable.hasDerivedType():
                derivedTypeVariables.append(variable)
            else:
                primitiveTypeVariables.append(variable)
            
        for variable in primitiveTypeVariables:
            if not self.__settings.pointersOnly or variable.isPointer():
                printLine(variable.getName())
        
        variableReferences = self.trackVariables(derivedTypeVariables, callGraph)
        if not quiet:
            if not self.__settings.minimalOutput:
                for variableReference in variableReferences:
                    if not self.__settings.pointersOnly or variableReference.isLevelNPointer():
                        printLine(variableReference)
            else:
                for variableReference in variableReferences:
                    if not self.__settings.pointersOnly or variableReference.isLevelNPointer():
                        printLine(variableReference.getExpression())
    
    def __findTypeVariable(self, variableName, subroutine):
        variable = subroutine.getVariable(variableName)
        if variable != None and variable.hasDerivedType():
            return variable
        return None
    
    def __findTypeArgument(self, argumentName, subroutine):
        argument = subroutine.findArgument(argumentName)
        if argument != None and argument.hasDerivedType():
            return argument
        return None
    
    def trackDerivedTypeArguments(self, callGraph):
        assertType(callGraph, 'callGraph', CallGraph)
        assert self.__interfaces is not None, 'No Interfaces set'
        assert self.__types is not None, 'No Types set'
        
        subroutineName = callGraph.getRoot()
        subroutine = self.__sourceFiles.findSubroutine(subroutineName)
        if subroutine is None:
            VariableTracker.__routineNotFoundWarning(subroutineName)
            return []
        
        variables = subroutine.getDerivedTypeArguments()
        return self.trackVariables(variables, callGraph)
    
    def trackDerivedTypeResult(self, callGraph):
        assertType(callGraph, 'callGraph', CallGraph)
        assert self.__interfaces is not None, 'No Interfaces set'
        assert self.__types is not None, 'No Types set'
        
        subroutineName = callGraph.getRoot()
        subroutine = self.__sourceFiles.findSubroutine(subroutineName)
        if subroutine is None:
            VariableTracker.__routineNotFoundWarning(subroutineName)
            return []
        elif not subroutine.isFunction():
            return []
        
        variable = subroutine.getResultVariable()
        if not variable.hasDerivedType():
            return [] 
        
        return self.trackVariables([variable], callGraph)
    
    def trackVariables(self, variables, callGraph):
        assertTypeAll(variables, 'variables', Variable) 
        assertType(callGraph, 'callGraph', CallGraph) 
        
        if self.__interfaces is None or self.__types is None:
            useTraversal = UseTraversal(self.__sourceFiles, self.__settings.excludeModules, self.__settings.abstractTypes)
            useTraversal.parseModules(callGraph.getRoot())
            self.__interfaces = useTraversal.getInterfaces()
            self.__types = useTraversal.getTypes()
        
        for variable in variables:
            if not variable.isTypeAvailable() and variable.getDerivedTypeName() in self.__types:
                variable.setType(self.__types.getTypeOfVariable(variable))
                
        variableReferences = [];
        for variable in variables:
            variableReferences += self.__trackVariable(variable, callGraph);
        
        return variableReferences;
    
    def trackAssignment(self, alias, originalReference, callGraph, lineNumber):
        assertType(alias, 'alias', str) 
        assertType(originalReference, 'originalReference', VariableReference) 
        assertType(callGraph, 'callGraph', CallGraph) 
        assertType(lineNumber, 'lineNumber', int) 
        
        self.__variable = originalReference.getLevel0Variable();
        if self.__variable.hasDerivedType and self.__variable.getDerivedTypeName() in self.__settings.ignoredTypes:
            return []
        
        self.__callGraph = callGraph;
        subroutineFullName = callGraph.getRoot()
        self.__excludeFromRecursionVariables = {self.__variable} 
        self.__excludeFromRecursionRoutines = {(subroutineFullName, self.__variable)} 
        
        subroutine = self.__sourceFiles.findSubroutine(subroutineFullName)
        if subroutine is not None:
            variableReferences = self.__analyzeAssignment(alias, originalReference, subroutine, lineNumber)
            variableReferences = VariableReference.sort(variableReferences)
        
        return variableReferences
    
    def __trackVariable(self, variable, callGraph, excludeFromRecursionVariables = set(), excludeFromRecursionRoutines = set(), startAtLine = 0):
        self.__variable = variable;
        self.__callGraph = callGraph;
        subroutineFullName = callGraph.getRoot();
        
        if self.__variable.hasDerivedType:
            if self.__variable.getDerivedTypeName() in self.__settings.ignoredTypes:
                return []
            elif variable.getDerivedTypeName() in self.__settings.fullTypes and variable.isTypeAvailable():
                lineNumber = startAtLine
                if lineNumber == 0:
                    subroutine = self.__sourceFiles.findSubroutine(subroutineFullName)
                    if subroutine is not None:
                        lineNumber = subroutine.getDeclarationLineNumber()
                originalReference = VariableReference(variable.getName(), subroutineFullName, lineNumber, variable)
                variableReferences = self.createReferencesForFullTypeVariable(originalReference, subroutineFullName, lineNumber)
                return VariableReference.sort(variableReferences)
        
        self.__excludeFromRecursionVariables = set(excludeFromRecursionVariables) # Copy set, otherwise empty optional will not work (https://stackoverflow.com/questions/25204126/python-function-optional-argument-evaluated-once) 
        self.__excludeFromRecursionVariables.add(variable)
        self.__excludeFromRecursionRoutines = set(excludeFromRecursionRoutines) 
        self.__excludeFromRecursionRoutines.add((subroutineFullName, variable))
        
        variableReferences = self.__analyzeSubroutine(subroutineFullName, startAtLine)
        variableReferences = VariableReference.sort(variableReferences)
        
        return variableReferences;    
                
    def __analyzeSubroutine(self, subroutineName, startAtLine = 0):
        if self.__settings.matchIgnoreSubroutineRegex(subroutineName) or (subroutineName.getModuleName().lower() in self.__settings.excludeModules):
            return set()
        
        variableReferences = set()
        subroutine = self.__sourceFiles.findSubroutine(subroutineName)
        if subroutine is not None:
            for lineNumber, statement, _ in subroutine.getStatements():
                if startAtLine <= 0 or lineNumber >= startAtLine:
                    variableReferences |= self.__analyzeStatement(statement, subroutine, lineNumber)
        else:
            VariableTracker.__routineNotFoundWarning(subroutineName)
                
        return variableReferences;
    
    def __analyzeStatement(self, statement, subroutine, lineNumber):
        variableName = self.__variable.getName()
        variableRegEx = re.compile(r'^((.*[^a-z0-9_%])?)' + variableName + r'(([^a-z0-9_].*)?)$', re.IGNORECASE);
        assignmentRegEx = re.compile(r'(?P<alias>[a-z0-9_]+)(\(.*\))?\s*\=\>?\s*(?P<reference>' + variableName + r'(\([a-z0-9_\,\:]+\))?(%[a-z0-9_%]+)?)$', re.IGNORECASE);
        accessRegEx = re.compile(r'(.*[^a-z0-9_%])?(?P<reference>' + variableName + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)+)', re.IGNORECASE)
        functionCallRegEx = re.compile(r'^.*(?P<function>[^a-z0-9_]+[a-z0-9_]+)\s*\((.*[^a-z0-9_%])?' + variableName + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)*([^a-z0-9_].*)?\).*$', re.IGNORECASE);
        typeBoundFunctionCallRegEx = re.compile(r'^.*\%[a-z0-9_]+\s*\((.*[^a-z0-9_%])?' + variableName + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)*([^a-z0-9_].*)?\).*$', re.IGNORECASE);
        declarationRegEx = re.compile(r'^[A-Z\s]*((SUBROUTINE)|(FUNCTION)).*?$', re.IGNORECASE);
        selectTypeRegEx = re.compile(r'^\s*SELECT\s+TYPE\s*\(\s*' + variableName + r'\s*\)\s*$', re.IGNORECASE);
        innerSubroutineCallRegEx = re.compile(r'^(.*\s+)?CALL\s*(?P<routine>[a-z0-9_]+)\s*\(.*\)$', re.IGNORECASE);
        
        variableReferences = set()
        while variableRegEx.match(statement) is not None:
            replaced = False
            originalStatement = statement
            statement = SourceFile.removeUnimportantParentheses(statement, variableRegEx)
            assignmentRegExMatch = assignmentRegEx.match(statement)
            foundReferences = set()
            if assignmentRegExMatch is not None and self.__isAssignmentToDerivedType(assignmentRegExMatch, subroutine, lineNumber):
                foundReferences = self.__analyzeExplicitAssignment(assignmentRegExMatch, subroutine, lineNumber)
            else:
                accessRegExMatch = accessRegEx.match(statement)
                if accessRegExMatch is not None:
                    (foundReferences, typeBoundOutAssignments) = self.__analyzeAccess(accessRegExMatch, subroutine, lineNumber, originalStatement)
                    if typeBoundOutAssignments:
                        for aliasVar, originalReference in typeBoundOutAssignments:
                            if aliasVar.isFunctionResult():
                                statement = self.__replaceTypeBoundNameByResultVar(statement, originalReference)
                                replaced = True
                                break
                if not foundReferences:
                    functionOutAssignments = set()
                    if typeBoundFunctionCallRegEx.match(statement) is not None and declarationRegEx.match(statement) is None and selectTypeRegEx.match(statement) is None:
                        (foundReferences, functionOutAssignments) = self.__analyzeTypeBoundProcedureCallOnOther(subroutine, statement, lineNumber) 
                    elif functionCallRegEx.match(statement) is not None and declarationRegEx.match(statement) is None and selectTypeRegEx.match(statement) is None:
                        (foundReferences, functionOutAssignments) = self.__analyzeFunctionCall(subroutine, statement, lineNumber)
                    if functionOutAssignments:
                        for aliasVar, originalReference in functionOutAssignments:
                            if aliasVar.isFunctionResult():
                                statement = self.__replaceFunctionNameByResultVar(statement, originalReference)
                                replaced = True
                                break
            variableReferences.update(foundReferences)
            if not replaced:
                statement = re.sub(variableRegEx, r'\1@@@@@\3', statement, 1)
            
        innerSubroutineCallRegExMatch = innerSubroutineCallRegEx.match(statement)
        if innerSubroutineCallRegExMatch is not None:
            self.__analyzeInnerSubroutineCall(innerSubroutineCallRegExMatch, subroutine, lineNumber)
            
        return variableReferences
    
    def __replaceTypeBoundNameByResultVar(self, statement, variableReference):
        accessRegEx = re.compile(r'(.*[^a-z0-9_%])?(?P<reference>' + self.__variable.getName() + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)+)', re.IGNORECASE)
        accessRegExMatch = accessRegEx.match(statement)
        if accessRegExMatch is None:
            return statement

        referenceExpression = accessRegExMatch.group('reference')
        return re.sub(re.escape(referenceExpression), variableReference.getExpression(), statement, 1)
    
    def __replaceFunctionNameByResultVar(self, statement, variableReference):
        #TODO Teste mehrere Function Calls in einem statement
        functionRegEx = re.compile(r'^(?P<outside>.*[^a-z0-9_]+)(?P<routine>[a-z0-9_]+)(?P<before>\s*\((.*[^a-z0-9_%])?)(?P<reference>' + self.__variable.getName() + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)*)(?P<after>([^a-z0-9_=].*)?\)).*$', re.IGNORECASE)
        functionRegExMatch = functionRegEx.match(statement)
        if functionRegExMatch is None:
            return statement

        functionExpression = functionRegExMatch.group('routine')
        before = functionRegExMatch.group('before')
        paranthCount = before.count('(') - before.count(')')
        if paranthCount <= 0:
            return statement
        functionExpression += before
        functionExpression += functionRegExMatch.group('reference')

        for c in functionRegExMatch.group('after'):
            if paranthCount > 0:
                functionExpression += c
                if c == '(':
                    paranthCount += 1
                elif c == ')':
                    paranthCount -= 1
            else:
                break
            
        outsideRegEx = re.compile(r'^.*[^a-z0-9_](?P<prefix>([a-z0-9_]+%)+)$', re.IGNORECASE)
        outsideRegExMatch = outsideRegEx.match(functionRegExMatch.group('outside'))
        if outsideRegExMatch:
            functionExpression = outsideRegExMatch.group('prefix') + functionExpression

        return re.sub(re.escape(functionExpression), variableReference.getExpression(), statement, 1)
    
    def __isAssignmentToDerivedType(self, regExMatch, subroutine, lineNumber):
        originalReference = VariableReference(regExMatch.group('reference'), subroutine.getName(), lineNumber, self.__variable)
        variable = self.__findLevelNVariable(originalReference)
        return variable is not None and variable.hasDerivedType()
    
    def __analyzeExplicitAssignment(self, regExMatch, subroutine, lineNumber):
        alias = regExMatch.group('alias')
        originalReference = VariableReference(regExMatch.group('reference'), subroutine.getName(), lineNumber, self.__variable)
        return self.__analyzeAssignment(alias, originalReference, subroutine, lineNumber)
    
    def __analyzeAssignment(self, alias, originalReference, subroutine, lineNumber):
        if subroutine.hasVariable(alias):
            aliasVar = subroutine.getVariable(alias)
            if not aliasVar.isTypeAvailable():
                aliasType = self.__types.getTypeOfVariable(aliasVar)
                if aliasType is not None:
                    aliasVar.setType(aliasType)
            
            if not originalReference.isRecursive():
                variable = self.__findLevelNVariable(originalReference)
                if variable is not None and variable.hasDerivedType() and aliasVar not in self.__excludeFromRecursionVariables:
                    newSubroutineAnalyzer = VariableTracker(self.__sourceFiles, self.__settings, self.__interfaces, self.__types, callGraphBuilder = self.__callGraphBuilder)
                    # TODO Support several statements in the same line
                    variableReferences = newSubroutineAnalyzer.__trackVariable(aliasVar, self.__callGraph, self.__excludeFromRecursionVariables, self.__excludeFromRecursionRoutines, lineNumber + 1)
                    for variableReference in variableReferences:
                        variableReference.setLevel0Variable(self.__variable, originalReference.getMembers())
                    for asgmtAlias, asgmtReference in newSubroutineAnalyzer.__outAssignments:
                        asgmtReference.setLevel0Variable(self.__variable, asgmtReference.getMembers())
                        self.__outAssignments.add((asgmtAlias, asgmtReference))
                    if aliasVar.isOutArgument() or aliasVar.isFunctionResult():
                        self.__outAssignments.add((aliasVar, originalReference))                    
                        
                    return variableReferences
            else:
                VariableTracker.__printWarningOnce('Ignored assignment to recursive data structure: ' + str(originalReference))
                
        return set();
                
    def __analyzeAccess(self, regExMatch, subroutine, lineNumber, originalStatement):
        ref = regExMatch.group('reference').strip()
        ref = re.sub(r'\([^\)]*\)', '', ref)
        variableReference = VariableReference(ref, subroutine.getName(), lineNumber, self.__variable)
        if not variableReference.isRecursive(): 
            if not variableReference.containsProcedure():
                variable = self.__findLevelNVariable(variableReference)
                if variable is None or not variable.hasDerivedType():
                    return ({variableReference}, set())
                elif variable.getDerivedTypeName() in self.__settings.fullTypes:
                    return (self.createReferencesForFullTypeVariable(variableReference, subroutine.getName(), lineNumber), set())
            else:
                return self.__analyzeTypeBoundProcedureCallOnThis(variableReference, subroutine, lineNumber, originalStatement)
        else:
            VariableTracker.__printWarningOnce('Ignored access to recursive data structure: ' + str(variableReference))
        
        return (set(), set())   
    
    def createReferencesForFullTypeVariable(self, originalReference, subroutineName, lineNumber):
        variable = self.__findLevelNVariable(originalReference)
        if not variable.hasDerivedType() or not variable.getDerivedTypeName() in self.__settings.fullTypes:
            return set()
        elif not variable.isTypeAvailable():
            if variable.getDerivedTypeName() in self.__types:
                typE = self.__types.getTypeOfVariable(variable)
                if typE is None:
                    return set()
                else:
                    variable.setType(typE)
        
        return self.createReferencesForFullTypeRecursive(originalReference, variable.getType(), subroutineName, lineNumber, [])
    
    def createReferencesForFullTypeRecursive(self, originalReference, typE, subroutineName, lineNumber, typeHierarchy):
        
        typeHierarchy.append(typE)
        variableReferences = set()
        for member in typE.getMembers():
            memberExpression = originalReference.getExpression() + '%' + member.getName()
            memberReference = VariableReference(memberExpression, subroutineName, lineNumber, originalReference.getLevel0Variable())
            if member.hasBuiltInType():
                variableReferences.add(memberReference)
            elif member.isTypeAvailable() and member.getType() not in typeHierarchy:
                variableReferences.update(self.createReferencesForFullTypeRecursive(memberReference, member.getType(), subroutineName, lineNumber, typeHierarchy))
        return variableReferences
            
        
    def __analyzeTypeBoundProcedureCallOnThis(self, originalReference, subroutine, lineNumber, originalStatement):
        subReference = originalReference.getSubReferenceBeforeFirstProcedure()

        calledRoutineFullName = None
        typE = subReference.getLevelNVariable().getType()
        if typE.isAbstract():
            calledRoutineFullName = self.__findDeferredProcedure(typE, originalReference.findFirstProcedureAlias())
        if calledRoutineFullName is None:
            calledRoutineFullName = self.__findCalledTypeBoundProcedure(originalReference, subroutine)
        
        subGraph = None
        if calledRoutineFullName is not None:
            if calledRoutineFullName in self.__callGraph:
                subGraph = self.__callGraph.extractSubgraph(calledRoutineFullName)
            else:
                subGraph = self.__callGraphBuilder.buildCallGraph(calledRoutineFullName)

        if calledRoutineFullName is not None and subGraph is not None:
            calledSubroutine = self.__sourceFiles.findSubroutine(calledRoutineFullName);
            if calledSubroutine is not None:
                variableNameInCalledSubroutine = calledSubroutine.getArgumentNames()[0]
                variableInCalledSubroutine = self.__findTypeArgument(variableNameInCalledSubroutine, calledSubroutine)
    
                if variableInCalledSubroutine is not None:
                    if (calledRoutineFullName, variableInCalledSubroutine) not in self.__excludeFromRecursionRoutines:
                        typE = self.__types.getTypeOfVariable(variableInCalledSubroutine)
                        if typE is not None:
                            variableInCalledSubroutine.setType(typE)
                        calledSubroutineAnalyzer = VariableTracker(self.__sourceFiles, self.__settings, self.__interfaces, self.__types, callGraphBuilder = self.__callGraphBuilder)
                        variableReferences = calledSubroutineAnalyzer.__trackVariable(variableInCalledSubroutine, subGraph, excludeFromRecursionRoutines = self.__excludeFromRecursionRoutines);
                        for variableReference in variableReferences:
                            variableReference.setLevel0Variable(self.__variable, subReference.getMembers())
                        outAssignments = calledSubroutineAnalyzer.__outAssignments
                        for assignment in outAssignments:
                            assignment[1].setLevel0Variable(self.__variable, subReference.getMembers())
                            
                        routineAlias = originalReference.findFirstProcedureAlias()
                        argumentsRegEx = re.compile(r'.*%' + routineAlias + '\((?P<arguments>.*)\).*', re.IGNORECASE)
                        argumentsRegExMatch = argumentsRegEx.match(originalStatement)
                        if argumentsRegExMatch is not None:
                            arguments = argumentsRegExMatch.group('arguments')
                            arguments = SourceFile.removeUnimportantParentheses(arguments)
                            arguments = '@@@, ' + arguments
                            variableReferences += self.__analyzeOutArguments(outAssignments, arguments, subroutine, calledSubroutine, lineNumber)
        
                        return (variableReferences, outAssignments)
                else:
                    VariableTracker.__printWarningOnce('No type argument ' + self.__variable.getName() + ' => ' + variableNameInCalledSubroutine, subroutine.getName().getModuleName(), lineNumber)
            else:
                VariableTracker.__routineNotFoundWarning(calledRoutineFullName, subroutine.getName(), lineNumber)
        else:
            VariableTracker.__routineNotFoundWarning(originalReference.getExpression(), subroutine.getName(), lineNumber)
        
        return (set(), set())   
    
    def __findLevelNVariable(self, variableReference):
        # Kann aus unbekannten Gründen nicht ersetzt werden durch VariableReference.getLevelNVariable()
        variable = variableReference.getLevel0Variable()
        if variableReference.getLevel() > 0:
            for level in range(1, variableReference.getLevel() + 1):
                if variable.isTypeAvailable():
                    typE = variable.getType()
                else: 
                    typeName = variable.getDerivedTypeName()
                    if typeName in self.__types:
                        typE = self.__types.getTypeOfVariable(variable) # typE because type is built-in symbol
                    else:
                        return None
                variableName = variableReference.getVariableName(level)
                if typE.hasMember(variableName):
                    variable = typE.getMember(variableName)
                else:
                    return None
        
        return variable
    
    def __analyzeTypeBoundProcedureCallOnOther(self, subroutine, statement, lineNumber):
        #TODO Teste mehrere Function Calls in einem statement
        functionRegEx = re.compile(r'^(?P<procedure>.*\%[a-z0-9_]+)\s*\((?P<arguments>(?P<before>(.*[^a-z0-9_%])?)(?P<reference>' + self.__variable.getName() + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)*)(?P<after>([^a-z0-9_=].*)?))\).*$', re.IGNORECASE);
        functionRegExMatch = functionRegEx.match(statement)
        if functionRegExMatch is None:
            return (set(), set())

        procedurePrefix = functionRegExMatch.group('procedure').lower()
        procedure = ''
        paranthesisCount = 0
        for c in reversed(procedurePrefix):
            if c == ')':
                paranthesisCount += 1
            elif paranthesisCount > 0 and c == '(':
                paranthesisCount -= 1
            elif paranthesisCount == 0:
                if c.isalnum() or c == '_' or c == '%':
                    procedure += c
                else:
                    break
        procedure = procedure[::-1]
        
        variable0Name = procedure[:procedure.find('%')]
        if subroutine.hasVariable(variable0Name):
            variable0 = subroutine.getVariable(variable0Name)
            type0 = self.__types.getTypeOfVariable(variable0)
            if type0 is not None:
                variable0.setType(type0) 
            reference = VariableReference(procedure, subroutine.getName(), lineNumber, variable0)
            if reference.lastIsProcedure():
                calledRoutineName = reference.findFirstProcedure()
                if isinstance(calledRoutineName, list):
                    calledRoutineName = self.__findCalledTypeBoundProcedure(reference, subroutine)
                    if calledRoutineName is None:
                        return (set(), set()) 
                originalReference = VariableReference(functionRegExMatch.group('reference'), subroutine.getName(), lineNumber, self.__variable)
                before = functionRegExMatch.group('before').strip()
                virtualBefore = '@@@, ' + before
                arguments = functionRegExMatch.group('arguments').strip()
                virtualArguments = arguments.replace(before, virtualBefore)
                return self.__analyzeCall(calledRoutineName, originalReference, virtualBefore, virtualArguments, subroutine, lineNumber, False) # Warum stand hier mal False? Weil es sonst zu viele Fehlermeldungen gäbe, wegen den eingebauten Funktionen und den Arrayzugriffen, die syntakisch gleich aussehen
            
        return (set(), set())
    
    def __analyzeFunctionCall(self, subroutine, statement, lineNumber):
        #TODO Teste mehrere Function Calls in einem statement
        functionRegEx = re.compile(r'^.*[^a-z0-9_]+(?P<routine>[a-z0-9_]+)\s*\((?P<arguments>(?P<before>(.*[^a-z0-9_%])?)(?P<reference>' + self.__variable.getName() + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)*)(?P<after>([^a-z0-9_=].*)?))\).*$', re.IGNORECASE);
        functionRegExMatch = functionRegEx.match(statement)
        if functionRegExMatch is None:
            return (set(), set())

        before = functionRegExMatch.group('before')
        if before.count(')') == before.count('(') + 1:
            return (set(), set())
        
        calledRoutineName = functionRegExMatch.group('routine').strip().lower()
        originalReference = VariableReference(functionRegExMatch.group('reference'), subroutine.getName(), lineNumber, self.__variable)
        
        before = functionRegExMatch.group('before').strip()
        arguments = functionRegExMatch.group('arguments').strip()
        return self.__analyzeCall(calledRoutineName, originalReference, before, arguments, subroutine, lineNumber, False) # Warum steht hier False? Weil es sonst zu viele Fehlermeldungen gaebe, wegen den eingebauten Funktionen und den Arrayzugriffen, die syntakisch gleich aussehen 
    
    def __analyzeCall(self, calledRoutineName, originalReference, before, arguments, subroutine, lineNumber, warnIfNotFound = True):
        if self.__settings.matchIgnoreSubroutineRegex(calledRoutineName):
            return (set(), set())
        
        subroutineName = subroutine.getName()
        variable = self.__findLevelNVariable(originalReference)
        if variable is None or not variable.hasDerivedType():
            return (set(), set())
        
        if originalReference.isRecursive():
            VariableTracker.__printWarningOnce('Ignored argument with recursive data structure: ' + str(originalReference))
            return (set(), set())

        if isinstance(calledRoutineName, SubroutineFullName):
            calledRoutineFullName = calledRoutineName
        else:
            calledRoutineFullName = self.__findCalledSubroutineFullName(calledRoutineName, subroutine, lineNumber)
            
        if calledRoutineFullName is not None and calledRoutineFullName.getModuleName().lower() not in self.__settings.excludeModules:
            if calledRoutineFullName in self.__callGraph:
                subGraph = self.__callGraph.extractSubgraph(calledRoutineFullName)
            elif self.__callGraphBuilder is not None:
                subGraph = self.__callGraphBuilder.buildCallGraph(calledRoutineFullName)
            
            before = SourceFile.removeUnimportantParentheses(before)
            arguments = SourceFile.removeUnimportantParentheses(arguments)
            
            calledSubroutine = self.__sourceFiles.findSubroutine(calledRoutineFullName);
            if calledSubroutine is not None:

                if before.endswith('='):
                    variableNameInCalledSubroutine = before[before.rfind(',') + 1:-1]
                else:
                    position = before.count(',');
                    variableNameInCalledSubroutine = calledSubroutine.getArgumentNames()[position]
                variableInCalledSubroutine = self.__findTypeArgument(variableNameInCalledSubroutine, calledSubroutine)
    
                if variableInCalledSubroutine is not None:
                    if (calledRoutineFullName, variableInCalledSubroutine) not in self.__excludeFromRecursionRoutines:
                        typE = self.__types.getTypeOfVariable(variableInCalledSubroutine)
                        if typE is not None:
                            variableInCalledSubroutine.setType(typE)
                        calledSubroutineAnalyzer = VariableTracker(self.__sourceFiles, self.__settings, self.__interfaces, self.__types, callGraphBuilder = self.__callGraphBuilder)
                        variableReferences = calledSubroutineAnalyzer.__trackVariable(variableInCalledSubroutine, subGraph, excludeFromRecursionRoutines = self.__excludeFromRecursionRoutines);
                        for variableReference in variableReferences:
                            variableReference.setLevel0Variable(self.__variable, originalReference.getMembers())
                        outAssignments = calledSubroutineAnalyzer.__outAssignments
                        for assignment in outAssignments:
                            assignment[1].setLevel0Variable(self.__variable, originalReference.getMembers())
                        variableReferences += self.__analyzeOutArguments(outAssignments, arguments, subroutine, calledSubroutine, lineNumber)
        
                        return (variableReferences, outAssignments)
                else:
                    VariableTracker.__printWarningOnce('No type argument ' + self.__variable.getName() + ' => ' + variableNameInCalledSubroutine, subroutineName.getModuleName(), lineNumber)
            else:
                VariableTracker.__routineNotFoundWarning(calledRoutineFullName, subroutine.getName(), lineNumber)
        elif warnIfNotFound:
            VariableTracker.__routineNotFoundWarning(str(calledRoutineName), subroutine.getName(), lineNumber)
        
        return (set(), set())
    
    def __analyzeOutArguments(self, assignments, arguments, callingSubroutine, calledSubroutine, lineNumber):
        variableReferences = set()
        arguments = arguments.split(',')
        arguments = [arg.strip() for arg in arguments]
        for alias, originalReference in assignments:
            if alias.isOutArgument():
                aliasPosition = calledSubroutine.getArgumentPosition(alias)
                if aliasPosition >= 0 and aliasPosition < len(arguments):
                    argument = arguments[aliasPosition]
                    variableReferences.update(self.__analyzeAssignment(argument, originalReference, callingSubroutine, lineNumber))
                    
        return variableReferences
                    
    
    def __findCalledTypeBoundProcedure(self, variableReference, subroutine):
        lineNumber = variableReference.getLineNumber()
        procedures = variableReference.findFirstProcedure()
        if isinstance(procedures, str):
            return self.__findCalledSubroutineFullName(procedures, subroutine, lineNumber)
        else:
            for candidate in self.__findNextCalleesFromLine(subroutine, lineNumber):
                if candidate.getSimpleName() in procedures:
                    return candidate
        
        return None
        

    def __findCalledSubroutineFullName(self, calledSubroutineSimpleName, callerSubroutine, lineNumber):
        
        callerSubroutineName = callerSubroutine.getName()
        callerModule = callerSubroutine.getModule()
        
        # Get from CallGraph
        calledSubroutineFullName = self.__callGraph.findCalleeBySimpleName(calledSubroutineSimpleName, callerSubroutineName)
        if calledSubroutineFullName is not None:
            return calledSubroutineFullName
        elif calledSubroutineSimpleName in self.__interfaces:
            interface = self.__interfaces[calledSubroutineSimpleName]
            for candidate in self.__findNextCalleesFromLine(callerSubroutine, lineNumber):
                if candidate.getSimpleName() in interface:
                    return candidate

        # Find elsewhere
        if calledSubroutineSimpleName in callerModule:
            return SubroutineFullName.fromParts(callerModule.getName(), calledSubroutineSimpleName)
        else:
            for use in callerSubroutine.getModule().getUses():
                if use[-1] == calledSubroutineSimpleName:
                    if self.__sourceFiles.existsModule(use[0]):
                        usedModule = self.__sourceFiles.findModule(use[0])
                        if calledSubroutineSimpleName in usedModule:
                            return SubroutineFullName.fromParts(usedModule.getName(), calledSubroutineSimpleName)
                        break
        
        return None   
    
    def __findDeferredProcedure(self, typE, procedureAlias):
        if typE.hasAssignedImplementation():
            implementation = typE.getAssignedImplementation()
            procedure = implementation.getProcedure(procedureAlias)
            if isinstance(procedure, str):
                module = implementation.getModule()
                if module is not None:
                    subroutineFullName = SubroutineFullName.fromParts(module.getName(), procedure)
                    if module.hasSubroutine(subroutineFullName):
                        return subroutineFullName
        return None
    
    def __findNextCalleesFromLine(self, callerSubroutine, lineNumber):
        lineNumber -= self.__findLineNumberOffset(callerSubroutine, lineNumber)
        return self.__callGraph.findNextCalleesFromLine(callerSubroutine.getName(), lineNumber)
    
    def __findLineNumberOffset(self, subroutine, lineNumber):
        sourceFile = subroutine.getSourceFile()
        if sourceFile is not None:
            return sourceFile.getPreprocessorOffset(lineNumber)
        return 0
    
    def __analyzeInnerSubroutineCall(self, regExMatch, subroutine, lineNumber):
        calledSubroutineSimpleName = regExMatch.group('routine').strip().lower()
        if self.__settings.matchIgnoreSubroutineRegex(calledSubroutineSimpleName):
            return set()
        
        calledInnerSubroutineName = self.__callGraph.findCalleeBySimpleName(calledSubroutineSimpleName, subroutine.getName())
        if isinstance(calledInnerSubroutineName, InnerSubroutineName):
            if calledInnerSubroutineName in subroutine and (calledInnerSubroutineName, self.__variable) not in self.__excludeFromRecursionRoutines:
                subGraph = self.__callGraph.extractSubgraph(calledInnerSubroutineName);
                calledSubroutineAnalyzer = VariableTracker(self.__sourceFiles, self.__settings, self.__interfaces, self.__types, callGraphBuilder = self.__callGraphBuilder)
            
                return calledSubroutineAnalyzer.__trackVariable(self.__variable, subGraph, excludeFromRecursionRoutines = self.__excludeFromRecursionRoutines)
            else:
                VariableTracker.__routineNotFoundWarning(calledInnerSubroutineName, subroutine.getName(), lineNumber, text = 'Inner Subroutine not found')
                
        return set();    
    
    @staticmethod
    def __routineNotFoundWarning(subroutineName, callerName = None, lineNumber = 0, text = ''):
        if text:
            warning = text
        else:
            warning = 'Routine not found'
        warning += ': ' + str(subroutineName)
        if callerName is not None:
            moduleName = callerName.getModuleName()
        else:
            moduleName = ''

        VariableTracker.__printWarningOnce(warning, moduleName, lineNumber)
            
    @staticmethod
    def __printWarningOnce(warning, moduleName = '', lineNumber = 0):
        if moduleName:
            warning += ' (' + moduleName
            if lineNumber:
                warning += ':' + str(lineNumber)
            warning += ')'
        
        if warning not in VariableTracker.__warnings:
            VariableTracker.__warnings.add(warning)
            printWarning(warning, 'Variable Tracker')

class VariableTrackerSettings(object):
    
    def __init__(self):
        self.excludeModules = []
        self.ignoreGlobalsFromModules = []
        self.ignoredTypes   = []
        self.fullTypes = []
        self.abstractTypes = {}
        self.ignoreSubroutinesRegex = '' 
        self.minimalOutput = False 
        self.pointersOnly = False 
        #TODO Add more settings

    def __setattr__(self, name, value):
        if name == 'ignoreSubroutinesRegex':
            if not value:
                value = None
            elif isinstance(value, str):
                value = re.compile(value)
        elif isinstance(value, list):
            value = [e.lower() for e in value]
        super(VariableTrackerSettings, self).__setattr__(name, value)
        
    def matchIgnoreSubroutineRegex(self, subroutineName):
        if self.ignoreSubroutinesRegex is None:
            return False
        if isinstance(subroutineName, SubroutineName):
            subroutineName = subroutineName.getSimpleName()
        return self.ignoreSubroutinesRegex.match(subroutineName) is not None
