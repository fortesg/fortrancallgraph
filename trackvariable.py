#coding=utf8

import re
import sys
from assertions import assertType, assertTypeAll
from supertypes import CallGraphAnalyzer
from source import SourceFiles, Variable, VariableReference, SubroutineFullName, InnerSubroutineName
from callgraph import CallGraph
from usetraversal import UseTraversal
from typefinder import TypeCollection
from _ast import alias

class VariableTracker(CallGraphAnalyzer):

    __routineWarnings = set()

    def __init__(self, sourceFiles, excludeModules = [], ignoredTypes = [], interfaces = None, types = None):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertTypeAll(excludeModules, 'excludeModules', str)
        assertTypeAll(ignoredTypes, 'ignoredTypes', str)
        assertType(interfaces, 'interfaces', dict, True)
        assertType(types, 'types', TypeCollection, True)

        super(VariableTracker, self).__init__()

        self.__sourceFiles = sourceFiles;
        self.__variable = None;
        self.__variableName = None;
        self.__callGraph = None;
        self.__interfaces = interfaces;
        self.__types = types;
        self.__excludeModules = map(str.lower, excludeModules)
        self.__ignoredTypes = map(str.lower, ignoredTypes)
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
            print  >> sys.stderr, '*** ERROR [VariableTracker] Subroutine ' + str(rootSubroutineName) + ' not found. ***';
            return None;
        
        if self.__variable is not None:
            variables = {self.__variable}
        else:
            if self.__variableName is not None:
                self.__variable = self.__findTypeArgument(self.__variableName, rootSubroutine)
                if self.__variable is None:
                    print  >> sys.stderr, '*** ERROR [VariableTracker] No type argument with name: ' + self.__variableName + '. ***';
                    return None;
                variables = {self.__variable}
            else:
                variables = rootSubroutine.getDerivedTypeArguments()
                
        if self.__variableName is None : 
            variableNames = map(Variable.getName, variables)    
            for argument in rootSubroutine.getArguments():
                if argument.getName() not in variableNames and (not self._pointersOnly or argument.isPointer()):
                    print argument.getName()
        
        variableReferences = self.trackVariables(variables, callGraph)
        if not quiet:
            if not self._minimalOutput:
                for variableReference in variableReferences:
                    if not self._pointersOnly or variableReference.isLevelNPointer():
                        print str(variableReference);
            else:
                for variableReference in variableReferences:
                    if not self._pointersOnly or variableReference.isLevelNPointer():
                        print variableReference.getExpression()
    
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
    
    def trackVariables(self, variables, callGraph):
        assertTypeAll(variables, 'variables', Variable) 
        assertType(callGraph, 'callGraph', CallGraph) 
        
        if self.__interfaces is None or self.__types is None:
            useTraversal = UseTraversal(self.__sourceFiles, self.__excludeModules)
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
    
    def __trackVariable(self, variable, callGraph, excludeFromRecursionVariables = set(), excludeFromRecursionRoutines = set(), startAtLine = 0):
        self.__variable = variable;
        if self.__variable.hasDerivedType and self.__variable.getDerivedTypeName() in self.__ignoredTypes:
            return set()
        
        self.__callGraph = callGraph;
        subroutineFullName = callGraph.getRoot();
        
        self.__excludeFromRecursionVariables = set(excludeFromRecursionVariables) # Copy set, otherwise empty optional will not work (https://stackoverflow.com/questions/25204126/python-function-optional-argument-evaluated-once) 
        self.__excludeFromRecursionVariables.add(variable)
        self.__excludeFromRecursionRoutines = set(excludeFromRecursionRoutines) 
        self.__excludeFromRecursionRoutines.add((subroutineFullName, variable))
        
        variableReferences = self.__analyzeSubroutine(subroutineFullName, startAtLine)
        variableReferences = VariableReference.sort(variableReferences)
        
        return variableReferences;    
                
    def __analyzeSubroutine(self, subroutineName, startAtLine = 0):
        if (self._ignoreRegex is not None and self._ignoreRegex.match(subroutineName.getSimpleName()) is not None) or (subroutineName.getModuleName().lower() in self.__excludeModules):
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
        assignmentRegEx = re.compile(r'(?P<alias>[a-z0-9_]+)\s*\=\>?\s*(?P<reference>' + variableName + r'(\([a-z0-9_\,\:]+\))?(%[a-z0-9_%]+)?)$', re.IGNORECASE);
        accessRegEx = re.compile(r'(.*[^a-z0-9_%])?(?P<reference>' + variableName + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)+)', re.IGNORECASE);
        functionCallRegEx = re.compile(r'^.*(?P<function>[^a-z0-9_]+[a-z0-9_]+)\s*\((.*[^a-z0-9_%])?' + variableName + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)*([^a-z0-9_].*)?\).*$', re.IGNORECASE);
        typeBoundFunctionCallRegEx = re.compile(r'^.*\%[a-z0-9_]+\s*\((.*[^a-z0-9_%])?' + variableName + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)*([^a-z0-9_].*)?\).*$', re.IGNORECASE);
        declarationRegEx = re.compile(r'^[A-Z\s]*((SUBROUTINE)|(FUNCTION)).*?$', re.IGNORECASE);
        selectTypeRegEx = re.compile(r'^\s*SELECT\s+TYPE\s*\(\s*' + variableName + r'\s*\)\s*$', re.IGNORECASE);
        innerSubroutineCallRegEx = re.compile(r'^(.*\s+)?CALL\s*(?P<routine>[a-z0-9_]+)\s*\(.*\)$', re.IGNORECASE);
        
        variableReferences = set()
        functionResultReference = None
        while variableRegEx.match(statement) is not None:
            statement = self.__removeUnimportantParentheses(statement, variableRegEx)
            assignmentRegExMatch = assignmentRegEx.match(statement)
            if assignmentRegExMatch is not None and self.__isAssignmentToDerivedType(assignmentRegExMatch, subroutine, lineNumber):
                variableReferences.update(self.__analyzeAssignment(assignmentRegExMatch, subroutine, lineNumber))
            else:
                accessRegExMatch = accessRegEx.match(statement)
                foundReferences = {} 
                if accessRegExMatch is not None:
                    foundReferences = self.__analyzeAccess(accessRegExMatch, subroutine, lineNumber)
                    variableReferences.update(foundReferences)
                if not foundReferences:
                    if typeBoundFunctionCallRegEx.match(statement) is not None and declarationRegEx.match(statement) is None and selectTypeRegEx.match(statement) is None:
                        variableReferences.update(self.__analyzeTypeBoundProcedureCallOnOther(subroutine, statement, lineNumber)[0])
                    elif functionCallRegEx.match(statement) is not None and declarationRegEx.match(statement) is None and selectTypeRegEx.match(statement) is None:
                        (functionCallVariableReferences, outAssignments) = self.__analyzeFunctionCall(subroutine, statement, lineNumber)
                        variableReferences.update(functionCallVariableReferences)
                        for aliasVar, originalReference in outAssignments:
                            if aliasVar.isFunctionResult():
                                functionResultReference = originalReference
                                break
            if functionResultReference is not None:
                statement = self.__replaceFunctionNameByResultVar(statement, functionResultReference)
                functionResultReference = None      
            else:
                statement = re.sub(variableRegEx, r'\1@@@@@\3', statement, 1)
            
        innerSubroutineCallRegExMatch = innerSubroutineCallRegEx.match(statement)
        if innerSubroutineCallRegExMatch is not None:
            self.__analyzeInnerSubroutineCall(innerSubroutineCallRegExMatch, subroutine, lineNumber)
            
        return variableReferences
    
    def __replaceFunctionNameByResultVar(self, statement, variableReference):
        #TODO Teste mehrere Function Calls in einem statement
        functionRegEx = re.compile(r'^.*[^a-z0-9_]+(?P<routine>[a-z0-9_]+)(?P<before>\s*\((.*[^a-z0-9_%])?)(?P<reference>' + self.__variable.getName() + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)*)(?P<after>([^a-z0-9_=].*)?\)).*$', re.IGNORECASE);
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
        
        return re.sub(re.escape(functionExpression), variableReference.getExpression(), statement, 1)
    
    def __removeUnimportantParentheses(self, statement, regEx):
        clean = ''
        pString = ''
        pCount = 0
        for c in statement:
            if c == '(':
                pCount += 1
            
            if pCount == 0:
                clean += c
            else:
                pString += c
                
            if c == ')':
                pCount -= 1
                if pCount == 0:
                    pString = pString[1:-1]
                    if regEx.match(pString) is not None:
                        clean += '(' + self.__removeUnimportantParentheses(pString, regEx) + ')'
                    pString = ''
        return clean
        
    
    def __isAssignmentToDerivedType(self, regExMatch, subroutine, lineNumber):
        originalReference = VariableReference(regExMatch.group('reference'), subroutine.getName(), lineNumber, self.__variable)
        variable = self.__findLevelNVariable(originalReference)
        return variable is not None and variable.hasDerivedType()
    
    def __analyzeAssignment(self, regExMatch, subroutine, lineNumber):
        alias = regExMatch.group('alias')
        
        if subroutine.hasVariable(alias):
            aliasVar = subroutine.getVariable(alias)
            if not aliasVar.isTypeAvailable():
                aliasType = self.__types.getTypeOfVariable(aliasVar)
                if aliasType is not None:
                    aliasVar.setType(aliasType)
            originalReference = VariableReference(regExMatch.group('reference'), subroutine.getName(), lineNumber, self.__variable)
            return self.__followAssignment(aliasVar, originalReference, lineNumber)
                
        return set();
    
    def __followAssignment(self, aliasVar, originalReference, lineNumber):
        if not originalReference.isRecursive():
            variable = self.__findLevelNVariable(originalReference)
            if variable is not None and variable.hasDerivedType() and aliasVar not in self.__excludeFromRecursionVariables:
                newSubroutineAnalyzer = VariableTracker(self.__sourceFiles, self.__excludeModules, self.__ignoredTypes, self.__interfaces, self.__types);
                newSubroutineAnalyzer.setIgnoreRegex(self._ignoreRegex)
                variableReferences = newSubroutineAnalyzer.__trackVariable(aliasVar, self.__callGraph, self.__excludeFromRecursionVariables, self.__excludeFromRecursionRoutines, lineNumber + 1)
                for variableReference in variableReferences:
                    variableReference.setLevel0Variable(self.__variable, originalReference.getMembers())
                for asgmtAlias, asgmtReference in newSubroutineAnalyzer.__outAssignments:
                    self.__outAssignments.add((asgmtAlias, asgmtReference.setLevel0Variable(self.__variable, asgmtReference.getMembers())))
                if aliasVar.isOutArgument() or aliasVar.isFunctionResult():
                    self.__outAssignments.add((aliasVar, originalReference))                    
                    
                return variableReferences
        else:
            print >> sys.stderr, '*** WARNING [VariableTracker] Ignored assignment to recursive data structure: ' + str(originalReference) + ') ***';
                
        return set();
                
    def __analyzeAccess(self, regExMatch, subroutine, lineNumber):
        ref = regExMatch.group('reference').strip()
        ref = re.sub(r'\([^\)]*\)', '', ref)
        variableReference = VariableReference(ref, subroutine.getName(), lineNumber, self.__variable)
        if not variableReference.isRecursive(): 
            if not variableReference.containsProcedure():
                variable = self.__findLevelNVariable(variableReference)
                if variable is None or not variable.hasDerivedType():
                    return {variableReference}
            else:
                return self.__analyzeTypeBoundProcedureCallOnThis(variableReference, subroutine, lineNumber)
        else:
            print >> sys.stderr, '*** WARNING [VariableTracker] Ignored access to recursive data structure: ' + str(variableReference) + ') ***';
        
        return set();    
    
    def __analyzeTypeBoundProcedureCallOnThis(self, variableReference, subroutine, lineNumber, warnIfNotFound = True):
        calledRoutineFullName = self.__findCalledTypeBoundProcedure(variableReference, subroutine)
        
        subReference = variableReference.getSubReferenceBeforeFirstProcedure()
        if calledRoutineFullName is not None:
            subGraph = self.__callGraph.extractSubgraph(calledRoutineFullName);

            calledSubroutine = self.__sourceFiles.findSubroutine(calledRoutineFullName);
            if calledSubroutine is not None:
                variableNameInCalledSubroutine = calledSubroutine.getArgumentNames()[0]
                variableInCalledSubroutine = self.__findTypeArgument(variableNameInCalledSubroutine, calledSubroutine)
    
                if variableInCalledSubroutine is not None:
                    if (calledRoutineFullName, variableInCalledSubroutine) not in self.__excludeFromRecursionRoutines:
                        typE = self.__types.getTypeOfVariable(variableInCalledSubroutine)
                        if typE is not None:
                            variableInCalledSubroutine.setType(typE)
                        calledSubroutineAnalyzer = VariableTracker(self.__sourceFiles, self.__excludeModules, self.__ignoredTypes, self.__interfaces, self.__types);
                        calledSubroutineAnalyzer.setIgnoreRegex(self._ignoreRegex)
                        variableReferences = calledSubroutineAnalyzer.__trackVariable(variableInCalledSubroutine, subGraph, excludeFromRecursionRoutines = self.__excludeFromRecursionRoutines);
                        for variableReference in variableReferences:
                            variableReference.setLevel0Variable(self.__variable, subReference.getMembers())
        
                        return variableReferences
                else:
                    print >> sys.stderr, '*** WARNING [VariableTracker] No type argument ' + self.__variable.getName() + ' => ' + variableNameInCalledSubroutine + ' (' + subroutine.getName().getModuleName() + ':' + str(lineNumber) + ') ***';
            else:
                VariableTracker.__routineNotFoundWarning(calledRoutineFullName, subroutine.getName(), lineNumber)
        elif warnIfNotFound:
            VariableTracker.__routineNotFoundWarning(variableReference.getExpression(), subroutine.getName(), lineNumber)
        
        return set();   
    
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
        functionRegEx = re.compile(r'^(?P<procedure>.*\%[a-z0-9_]+)\s*\((?P<before>(.*[^a-z0-9_%])?)(?P<reference>' + self.__variable.getName() + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)*)(?P<after>([^a-z0-9_=].*)?)\).*$', re.IGNORECASE);
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
                after = functionRegExMatch.group('after').strip()
                return self.__analyzeCall(calledRoutineName, originalReference, "@@@, " + before, after, subroutine, lineNumber, False) # Warum stand hier mal False? Weil es sonst zu viele Fehlermeldungen gäbe, wegen den eingebauten Funktionen und den Arrayzugriffen, die syntakisch gleich aussehen
            
        return (set(), set())
    
    def __analyzeFunctionCall(self, subroutine, statement, lineNumber):
        #TODO Teste mehrere Function Calls in einem statement
        functionRegEx = re.compile(r'^.*[^a-z0-9_]+(?P<routine>[a-z0-9_]+)\s*\((?P<before>(.*[^a-z0-9_%])?)(?P<reference>' + self.__variable.getName() + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)*)(?P<after>([^a-z0-9_=].*)?)\).*$', re.IGNORECASE);
        functionRegExMatch = functionRegEx.match(statement)
        if functionRegExMatch is None:
            return (set(), set())

        before = functionRegExMatch.group('before')
        if before.count(')') == before.count('(') + 1:
            return (set(), set())
        
        calledRoutineName = functionRegExMatch.group('routine').strip().lower()
        originalReference = VariableReference(functionRegExMatch.group('reference'), subroutine.getName(), lineNumber, self.__variable)
        
        before = functionRegExMatch.group('before').strip()
        after = functionRegExMatch.group('after').strip()
        return self.__analyzeCall(calledRoutineName, originalReference, before, after, subroutine, lineNumber, False) # Warum steht hier False? Weil es sonst zu viele Fehlermeldungen gaebe, wegen den eingebauten Funktionen und den Arrayzugriffen, die syntakisch gleich aussehen 
    
    def __analyzeCall(self, calledRoutineName, originalReference, before, after, subroutine, lineNumber, warnIfNotFound = True):
        if self._ignoreRegex is not None and self._ignoreRegex.match(calledRoutineName):
            return (set(), set())
        
        subroutineName = subroutine.getName()
        variable = self.__findLevelNVariable(originalReference)
        if variable is None or not variable.hasDerivedType():
            return (set(), set())
        
        if originalReference.isRecursive():
            print >> sys.stderr, '*** WARNING [VariableTracker] Ignored argument with recursive data structure: ' + str(originalReference) + ') ***';
            return (set(), set())

        if isinstance(calledRoutineName, SubroutineFullName):
            calledRoutineFullName = calledRoutineName
        else:
            calledRoutineFullName = self.__findCalledSubroutineFullName(calledRoutineName, subroutine, lineNumber)
            
        if calledRoutineFullName is not None and calledRoutineFullName.getModuleName().lower() not in self.__excludeModules:
            subGraph = self.__callGraph.extractSubgraph(calledRoutineFullName);
            
            parathesisRegEx = re.compile(r'\([^\(\)]*\)');
            before = parathesisRegEx.sub('', before);
            
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
                        calledSubroutineAnalyzer = VariableTracker(self.__sourceFiles, self.__excludeModules, self.__ignoredTypes, self.__interfaces, self.__types);
                        calledSubroutineAnalyzer.setIgnoreRegex(self._ignoreRegex)
                        variableReferences = calledSubroutineAnalyzer.__trackVariable(variableInCalledSubroutine, subGraph, excludeFromRecursionRoutines = self.__excludeFromRecursionRoutines);
                        outAssignments = calledSubroutineAnalyzer.__outAssignments
                        for variableReference in variableReferences:
                            variableReference.setLevel0Variable(self.__variable, originalReference.getMembers())
                        for assignment in outAssignments:
                            assignment[1].setLevel0Variable(self.__variable, originalReference.getMembers())
                        for asgmtAlias, asgmtReference in outAssignments:
                            if asgmtAlias.isOutArgument():
                                aliasPosition = subroutine.getArgumentPosition(asgmtAlias)
                                if aliasPosition >= 0:
                                    pass #TODO
        
                        return (variableReferences, outAssignments)
                else:
                    print  >> sys.stderr, '*** WARNING [VariableTracker] No type argument ' + self.__variable.getName() + ' => ' + variableNameInCalledSubroutine + ' (' + subroutineName.getModuleName() + ':' + str(lineNumber) + ') ***';
            else:
                VariableTracker.__routineNotFoundWarning(calledRoutineFullName, subroutine.getName(), lineNumber)
        elif warnIfNotFound:
            VariableTracker.__routineNotFoundWarning(str(calledRoutineName), subroutine.getName(), lineNumber)
        
        return (set(), set())

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
        calledSubroutineFullName = self.__callGraph.findCalleeBySimpleName(calledSubroutineSimpleName, callerSubroutineName)
        if calledSubroutineFullName is not None:
            return calledSubroutineFullName
        else:
            aliases = callerSubroutine.getSourceFile().getUseAliases()
            if calledSubroutineSimpleName in aliases:
                alias = aliases[calledSubroutineSimpleName]
                if SubroutineFullName.validParts(*alias):
                    calledSubroutineFullName = SubroutineFullName.fromParts(*alias)
                    if calledSubroutineFullName in self.__callGraph:
                        return calledSubroutineFullName
             
            if calledSubroutineSimpleName in self.__interfaces:
                interface = self.__interfaces[calledSubroutineSimpleName]
                for candidate in self.__findNextCalleesFromLine(callerSubroutine, lineNumber):
                    if candidate.getSimpleName() in interface:
                        return candidate
        
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
        if self._ignoreRegex is not None and self._ignoreRegex.match(calledSubroutineSimpleName):
            return set()
        
        calledInnerSubroutineName = self.__callGraph.findCalleeBySimpleName(calledSubroutineSimpleName, subroutine.getName())
        if isinstance(calledInnerSubroutineName, InnerSubroutineName):
            if calledInnerSubroutineName in subroutine and (calledInnerSubroutineName, self.__variable) not in self.__excludeFromRecursionRoutines:
                subGraph = self.__callGraph.extractSubgraph(calledInnerSubroutineName);
                calledSubroutineAnalyzer = VariableTracker(self.__sourceFiles, self.__excludeModules, self.__ignoredTypes, self.__interfaces, self.__types)
                calledSubroutineAnalyzer.setIgnoreRegex(self._ignoreRegex)
            
                return calledSubroutineAnalyzer.__trackVariable(self.__variable, subGraph, excludeFromRecursionRoutines = self.__excludeFromRecursionRoutines)
            else:
                VariableTracker.__routineNotFoundWarning(calledInnerSubroutineName, subroutine.getName(), lineNumber, text = 'Inner Subroutine not found')
                
        return set();    
        
    @staticmethod
    def __routineNotFoundWarning(subroutineName, callerName = None, lineNumber = 0, text = ''):
        if subroutineName not in VariableTracker.__routineWarnings:
        
            VariableTracker.__routineWarnings.add(subroutineName)
            
            warning = '*** WARNING [VariableTracker] '
            if text:
                warning += text
            else:
                warning += 'Routine not found'
            warning += ': ' + str(subroutineName)
            if callerName is not None and callerName.getModuleName():
                warning += ' (' + str(callerName.getModuleName())
                if lineNumber:
                    warning += ':' + str(lineNumber)
                warning += ')'
            warning += ' ***'
            
            print  >> sys.stderr, warning