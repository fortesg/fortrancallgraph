#coding=utf8

import re
import sys
from utils import assertType, assertTypeAll
from supertypes import CallGraphAnalyzer
from source import SourceFiles, Variable, VariableReference, SubroutineFullName, InnerSubroutineName
from callgraph import CallGraph
from usetraversal import UseTraversal
from typefinder import TypeCollection

class TrackVariableCallGraphAnalysis(CallGraphAnalyzer):

    __routineWarnings = set()

    def __init__(self, sourceFiles, excludeModules = [], ignoredTypes = [], interfaces = None, types = None):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertTypeAll(excludeModules, 'excludeModules', str)
        assertTypeAll(ignoredTypes, 'ignoredTypes', str)
        assertType(interfaces, 'interfaces', dict, True)
        assertType(types, 'types', TypeCollection, True)

        super(TrackVariableCallGraphAnalysis, self).__init__()

        self.__sourceFiles = sourceFiles;
        self.__variable = None;
        self.__variableName = None;
        self.__callGraph = None;
        self.__interfaces = interfaces;
        self.__types = types;
        self.__excludeModules = map(str.lower, excludeModules)
        self.__ignoredTypes = map(str.lower, ignoredTypes)
        self.__excludeFromRecursion = set()
    
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
            print  >> sys.stderr, '*** ERROR [TrackVariableCallGraphAnalysis]: Subroutine ' + str(rootSubroutineName) + ' not found. ***';
            return None;
        
        if self.__variable is not None:
            variables = {self.__variable}
        else:
            if self.__variableName is not None:
                self.__variable = self.__findTypeArgument(self.__variableName, rootSubroutine)
                if self.__variable is None:
                    print  >> sys.stderr, '*** ERROR [TrackVariableCallGraphAnalysis]: No type argument with name: ' + self.__variableName + '. ***';
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
            TrackVariableCallGraphAnalysis.__routineNotFoundWarning(subroutineName)
            return []
        
        variables = subroutine.getDerivedTypeArguments()
        return self.trackVariables(variables, callGraph)
    
    def trackVariables(self, variables, callGraph):
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
            variableReferences += self.__trackVariable(variable, callGraph, set());
        
        return variableReferences;
    
    def __trackVariable(self, variable, callGraph, excludeFromRecursion = set()):
        self.__variable = variable;
        if self.__variable.hasDerivedType and self.__variable.getDerivedTypeName() in self.__ignoredTypes:
            return set()
        
        self.__excludeFromRecursion = excludeFromRecursion
        self.__excludeFromRecursion.add(variable)
        
        
        self.__callGraph = callGraph;
        subroutine = callGraph.getRoot();
        variableReferences = self.__analyzeSubroutine(subroutine)
        variableReferences = VariableReference.sort(variableReferences)
        
        return variableReferences;    
                
    def __analyzeSubroutine(self, subroutineName):
        if (self._ignoreRegex is not None and self._ignoreRegex.match(subroutineName.getSimpleName()) is not None) or (subroutineName.getModuleName().lower() in self.__excludeModules):
            return set()
        
        variableName = self.__variable.getName()
        variableRegEx = re.compile(r'^((.*[^a-z0-9_])?)' + variableName + r'(([^a-z0-9_].*)?)$', re.IGNORECASE);
        assignmentRegEx = re.compile(r'(?P<alias>[a-z0-9_]+)\s*\=\>?\s*(?P<reference>' + variableName + r'(\([a-z0-9_\,\:]+\))?(%[a-z0-9_%]+)?)([^a-z0-9_].*)?$', re.IGNORECASE);
        accessRegEx = re.compile(r'(.*[^a-z0-9_])?(?P<reference>' + variableName + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)+)', re.IGNORECASE);
        functionCallRegEx = re.compile(r'^.*[^a-z0-9_]+[a-z0-9_]+\s*\((.*[^a-z0-9_])?' + variableName + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)*([^a-z0-9_].*)?\).*$', re.IGNORECASE);
        declarationRegEx = re.compile(r'^[A-Z\s]*((SUBROUTINE)|(FUNCTION)).*?$', re.IGNORECASE);
        selectTypeRegEx = re.compile(r'^\s*SELECT\s+TYPE\s*\(\s*' + variableName + r'\s*\)\s*$', re.IGNORECASE);
        innerSubroutineCallRegEx = re.compile(r'^(.*\s+)?CALL\s*(?P<routine>[a-z0-9_]+)\s*\(.*\)$', re.IGNORECASE);
        #TODO auch Inner Functions, aber wie?
        
        variableReferences = set();
        subroutine = self.__sourceFiles.findSubroutine(subroutineName)
        if subroutine is not None:
            for lineNumber, statement, _ in subroutine.getStatements():
                while variableRegEx.match(statement) is not None:
                    statement = self.__removeUnimportantParentheses(statement, variableRegEx)
                    assignmentRegExMatch = assignmentRegEx.match(statement)
                    if assignmentRegExMatch is not None and self.__isAssignmentToDerivedType(assignmentRegExMatch, subroutine, lineNumber):
                        variableReferences.update(self.__analyzeAssignment(assignmentRegExMatch, subroutine, lineNumber))
                    else:
                        accessRegExMatch = accessRegEx.match(statement) 
                        if accessRegExMatch is not None:
                            variableReferences.update(self.__analyzeAccess(accessRegExMatch, lineNumber))
                        if functionCallRegEx.match(statement) is not None and declarationRegEx.match(statement) is None and selectTypeRegEx.match(statement) is None:
                            variableReferences.update(self.__analyzeFunctionCall(subroutine, statement, lineNumber))
                    statement = re.sub(variableRegEx, r'\1@@@@@\3', statement, 1)
                    
                innerSubroutineCallRegExMatch = innerSubroutineCallRegEx.match(statement)
                if innerSubroutineCallRegExMatch is not None:
                    self.__analyzeInnerSubroutineCall(innerSubroutineCallRegExMatch, subroutine, statement, lineNumber)
        else:
            TrackVariableCallGraphAnalysis.__routineNotFoundWarning(subroutineName)
                
        return variableReferences;
    
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
            originalReference = VariableReference(regExMatch.group('reference'), subroutine.getName(), lineNumber, self.__variable)
            variable = self.__findLevelNVariable(originalReference)
            if variable is not None and variable.hasDerivedType() and aliasVar not in self.__excludeFromRecursion:
                newSubroutineAnalyzer = TrackVariableCallGraphAnalysis(self.__sourceFiles, self.__excludeModules, self.__ignoredTypes, self.__interfaces, self.__types);
                newSubroutineAnalyzer.setIgnoreRegex(self._ignoreRegex)
                variableReferences = newSubroutineAnalyzer.__trackVariable(aliasVar, self.__callGraph, self.__excludeFromRecursion);
                for variableReference in variableReferences:
                    variableReference.setLevel0Variable(self.__variable, originalReference.getMembers())
                    
                return variableReferences
                
        return set();
                
    def __analyzeAccess(self, regExMatch, lineNumber):
        ref = regExMatch.group('reference').strip()
        ref = re.sub(r'\([^\)]*\)', '', ref)
        variableReference = VariableReference(ref, self.__callGraph.getRoot(), lineNumber, self.__variable)
        variable = self.__findLevelNVariable(variableReference)
        if variable is None or not variable.hasDerivedType():
            return {variableReference}
                    
        return set();       
    
    def __findLevelNVariable(self, variableReference):
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
    
    def __analyzeFunctionCall(self, subroutine, statement, lineNumber):
        #TODO Teste mehrere Function Calls in einem statement
        functionRegEx = re.compile(r'^.*[^a-z0-9_]+(?P<routine>[a-z0-9_]+)\s*\((?P<before>(.*[^a-z0-9_])?)(?P<reference>' + self.__variable.getName() + r'((\([a-z0-9_\,\:]+\))?%[a-z0-9_]+)*)(?P<after>([^a-z0-9_=].*)?)\).*$', re.IGNORECASE);
        functionRegExMatch = functionRegEx.match(statement)
        if functionRegExMatch is None:
            return set()

        before = functionRegExMatch.group('before')
        if before.count(')') == before.count('(') + 1:
            return set()
        
        return self.__analysizeCallRegExMatch(functionRegExMatch, subroutine, statement, lineNumber, False) # Warum stand hier mal False? Weil es sonst zu viele Fehlermeldungen gäbe, wegen den eingebauten Funktionen und den Arrayzugriffen, die syntakisch gleich aussehen 
    
    def __analysizeCallRegExMatch(self, regExMatch, subroutine, statement, lineNumber, warnIfNotFound = True):
        subroutineName = subroutine.getName()
        calledRoutineName = regExMatch.group('routine').strip().lower()
        if self._ignoreRegex is not None and self._ignoreRegex.match(calledRoutineName):
            return set()
        
        originalReference = VariableReference(regExMatch.group('reference'), subroutine.getName(), lineNumber, self.__variable)
        variable = self.__findLevelNVariable(originalReference)
        if variable is None or not variable.hasDerivedType():
            return set()

        calledRoutineFullName = self.__findCalledSubroutineFullName(calledRoutineName, subroutine, lineNumber)

        
        if calledRoutineFullName is not None:
            subGraph = self.__callGraph.extractSubgraph(calledRoutineFullName);
            
            before = regExMatch.group('before').strip();
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
                    calledSubroutineAnalyzer = TrackVariableCallGraphAnalysis(self.__sourceFiles, self.__excludeModules, self.__ignoredTypes, self.__interfaces, self.__types);
                    calledSubroutineAnalyzer.setIgnoreRegex(self._ignoreRegex)
                    variableReferences = calledSubroutineAnalyzer.__trackVariable(variableInCalledSubroutine, subGraph);
                    for variableReference in variableReferences:
                        variableReference.setLevel0Variable(self.__variable, originalReference.getMembers())
    
                    return variableReferences
                
                elif warnIfNotFound:
                    print  >> sys.stderr, '*** WARNING [TrackVariableCallGraphAnalysis]: No type argument ' + self.__variable.getName() + ' => ' + variableNameInCalledSubroutine + ' (' + subroutineName.getModuleName() + ':' + str(lineNumber) + ') ***';
            elif warnIfNotFound:
                TrackVariableCallGraphAnalysis.__routineNotFoundWarning(calledRoutineFullName, subroutine.getName(), lineNumber)
        elif warnIfNotFound:
            TrackVariableCallGraphAnalysis.__routineNotFoundWarning(calledRoutineName, subroutine.getName(), lineNumber)
        
        return set();

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
                for candidate in self.__callGraph.findNextCalleesFromLine(callerSubroutineName, lineNumber):
                    if candidate.getSimpleName() in interface:
                        return candidate
        
        return None
    
    def __analyzeInnerSubroutineCall(self, regExMatch, subroutine, statement, lineNumber):
        calledSubroutineSimpleName = regExMatch.group('routine').strip().lower()
        if self._ignoreRegex is not None and self._ignoreRegex.match(calledSubroutineSimpleName):
            return set()
        
        calledInnerSubroutineName = self.__callGraph.findCalleeBySimpleName(calledSubroutineSimpleName, subroutine.getName())
        if isinstance(calledInnerSubroutineName, InnerSubroutineName):
            if calledInnerSubroutineName in subroutine:
                subGraph = self.__callGraph.extractSubgraph(calledInnerSubroutineName);
                calledSubroutineAnalyzer = TrackVariableCallGraphAnalysis(self.__sourceFiles, self.__excludeModules, self.__ignoredTypes, self.__interfaces, self.__types)
                calledSubroutineAnalyzer.setIgnoreRegex(self._ignoreRegex)
            
                return calledSubroutineAnalyzer.__trackVariable(self.__variable, subGraph)
            else:
                TrackVariableCallGraphAnalysis.__routineNotFoundWarning(calledInnerSubroutineName, subroutine.getName(), lineNumber, text = 'Inner Subroutine not found')
                
        return set();    
        
    @staticmethod
    def __routineNotFoundWarning(subroutineName, callerName = None, lineNumber = 0, text = ''):
        if subroutineName not in TrackVariableCallGraphAnalysis.__routineWarnings:
        
            TrackVariableCallGraphAnalysis.__routineWarnings.add(subroutineName)
            
            warning = '*** WARNING [TrackVariableCallGraphAnalysis] '
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