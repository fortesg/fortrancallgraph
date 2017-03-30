# coding=utf8

import os.path;
import re
import sys
from utils import assertType, assertTypeAll
from operator import attrgetter

IDENTIFIER_REG_EX = re.compile('^[a-z0-9_]{1,63}$', re.IGNORECASE)

class Type(object):
    
    def __init__(self, typeName, declaredIn, extends = None):
        assertType(typeName, 'typeName', str)
        assertType(declaredIn, 'declaredIn', [str, SubroutineFullName])
        assertType(extends, 'extends', Type, True)
        
        self.__typeName = typeName
        self.__declaredIn = declaredIn
        self.__extends = extends
        self.__members = {}
        
    def __str__(self, *args, **kwargs):
        string = 'TYPE ' + self.__typeName + '\n'
        for member in self.__members:
            string += '  ' + str(member) + '\n'
            
        return string.strip()
        
    def addMember(self, member):
        assertType(member, 'member', Variable)
        self.__members[member.getName().lower()] = member
        
    def addMembers(self, members):
        assertType(members, 'members', list)
        assertTypeAll(members, 'members', Variable)
        for member in members:
            self.addMember(member)
        
    def hasMember(self, name):
        return name.lower() in self.__members or ( self.__extends is not None and self.__extends.hasMember(name)) 
        
    def getMember(self, name):
        name = name.lower()
        if not self.hasMember(name):
            return None
        
        if name in self.__members:
            return self.__members[name]
        else:
            return self.__extends.getMember(name)
        
    def getName(self):
        return self.__typeName
    
    def getExtends(self):
        return self.__extends
    
    def setExtends(self, extends):
        assertType(extends, 'extends', Type, True)
        self.__extends = extends
    
    def getDeclaredIn(self):
        return self.__declaredIn
    
    def getMembers(self):
        return self.__members.values()    

class Interface(object):
    
    def __init__(self, interfaceName):
        assertType(interfaceName, 'interfaceName', str)
        
        self.__interfaceName = interfaceName
        self.__procedures = []
        
    def __str__(self, *args, **kwargs):
        string = 'INTERFACE ' + self.__interfaceName + "\n"
        for procedure in self.__procedures:
            string += '  MODULE PROCEDURE ' + str(procedure) + "\n"
            
        return string.strip()    
        
    def addProcedure(self, procedureName):
        assertType(procedureName, 'procedureName', str)
        self.__procedures.append(procedureName.lower())
        
    def __contains__(self, procedure):
        assertType(procedure, 'procedure', str)
        return procedure.lower() in self.__procedures
           
    def getName(self):
        return self.__interfaceName
    
    def getProcedures(self):
        return list(self.__procedures)    

class Variable(object):

    __types = '((LOGICAL)|(INTEGER)|(DOUBLE PRECISION)|(REAL)|(COMPLEX)|(CHARACTER\*?)|(TYPE)|(CLASS))\s*(\*\s*\d+)?\s*(\(\s*[a-z0-9_=\,\*\: \+\-\/]+\s*\))?'
    __declarationReg = re.compile(r'^(?P<typespecifier>' + __types + '\s*(,.*\:\:)?)(?P<varlist>.+)$', re.IGNORECASE)
    __typeRegEx = re.compile(r'^' + __types + '\s*$', re.IGNORECASE)
    __dimensionRegEx = re.compile(r'^DIMENSION\s*\((?P<dimension>.*)\)\s*$', re.IGNORECASE)
    __intentRegEx = re.compile(r'^INTENT\s*\(\s*(?P<intent>(in)|(out)|(inout))\s*\)\s*$', re.IGNORECASE)
    __intents = ['in', 'out', 'inout', '']
    
    def __init__(self, variableName, typeName, parameter=False, allocatable=False, pointer=False, target=False, dimension=0, intent='', optional=False, public=False, private=False):
        if not Variable.validIdentifier(variableName):
            raise ValueError("variableName is not a valid identifier: " + variableName);
        if not Variable.validType(typeName):
            raise ValueError('Invalid type name: ' + typeName)
        assertType(parameter, 'parameter', bool)
        assertType(allocatable, 'allocatable', bool)
        assertType(pointer, 'pointer', bool)
        assertType(target, 'target', bool)
        assertType(dimension, 'dimension', int)
        assertType(intent, 'intent', str)
        intent = intent.lower()
        if not Variable.validIntent(intent):
            raise ValueError('Invalid intent: ' + intent)
        assertType(optional, 'optional', bool)
        assertType(public, 'public', bool)
        assertType(private, 'private', bool)
        
        self.__name = variableName
        self.__typeName = typeName.replace(' ', '')
        self.__type = None
        self.__parameter = parameter
        self.__allocatable = allocatable
        self.__pointer = pointer
        self.__target = target
        self.__dimension = dimension
        self.__intent = intent
        self.__optional = optional
        self.__public = public
        self.__private = private
        self.__declaredIn = None
        self.__originalName = self.__name
        
    def __eq__(self, other):
        if (other is None or not isinstance(other, Variable)):
            return False;
        else:
            return self.__name == other.__name and       \
                   self.__typeName == other.__typeName and        \
                   self.__parameter == other.__parameter and \
                   self.__declaredIn == other.__declaredIn;
        
    def __ne__(self, other):
        return not self == other
    
    def __hash__(self):
        return hash(self.__name)    
        
    def __str__(self):
        string = self.getTypeName()
        if self.isParameter():
            string += ', PARAMETER'
        if self.getDimension() > 0:
            string += ', ' + 'DIMENSION(' + (',:' * self.getDimension()).strip(',') + ')'
        if self.isAllocatable():
            string += ', ALLOCATABLE'
        if self.isPointer():
            string += ', POINTER'
        if self.isTarget():
            string += ', TARGET'
        if self.isArgument():
            string += ', INTENT(' + self.getIntent() + ')'
        if self.isOptionalArgument():
            string += ', OPTIONAL'
        string += ' :: ' + self.getName()
        
        return string
    
    def getName(self):
        return self.__name
    
    def setName(self, name):
        self.__name = name
    
    def getTypeName(self):
        return self.__typeName
    
    def setTypeName(self, typeName):
        if not Variable.validType(typeName):
            raise ValueError('Invalid type name: ' + typeName)
        if self.isTypeAvailable():
            raise ValueError('setTypeName only allowed when derived type not yet set')
        
        self.__typeName = typeName.replace(' ', '')
    
    def hasDerivedType(self):
        return self.__typeName.upper().startswith('TYPE(') or self.__typeName.upper().startswith('CLASS(')
    
    def hasClassType(self):
        return self.__typeName.upper().startswith('CLASS(')
    
    def getDerivedTypeName(self):
        if not self.hasDerivedType():
            return None 
        return self.__typeName[self.__typeName.find('(') + 1:-1]
    
    def setType(self, typE):
        assertType(typE, 'typE', Type)
        if not self.hasDerivedType():
            raise ValueError("You are only allowed to set derived types.")
        if not typE.getName() == self.getDerivedTypeName():
            raise ValueError("Type doesn't match type name.")
        
        self.__type = typE
    
    def isTypeAvailable(self):
        return self.hasDerivedType() and self.__type is not None
    
    def getType(self):
        return self.__type
    
    def hasBuiltInType(self):
        return not self.hasDerivedType()
    
    def isParameter(self):
        return self.__parameter
    
    def setAllocatable(self, allocatable):
        assertType(allocatable, 'allocatable', bool)
        self.__allocatable = allocatable
    
    def isAllocatable(self):
        return self.__allocatable
    
    def isPointer(self):
        return self.__pointer
    
    def setPointer(self, pointer):
        assertType(pointer, 'pointer', bool)
        self.__pointer = pointer
    
    def isTarget(self):
        return self.__target
    
    def setTarget(self, target):
        assertType(target, 'target', bool)
        self.__target = target
    
    def getDimension(self):
        return self.__dimension
    
    def isScalar(self):
        return self.getDimension() == 0
    
    def isArray(self):
        return self.getDimension() > 0
    
    def isArgument(self):
        return self.__intent != ''
    
    def isInArgument(self):
        return self.__intent == 'in' or self.__intent == 'inout'; 
    
    def isOutArgument(self):
        return self.__intent == 'out' or self.__intent == 'inout';
    
    def getIntent(self):
        return self.__intent
    
    def setIntent(self, intent):
        assertType(intent, 'intent', str)
        intent = intent.lower()
        if not Variable.validIntent(intent):
            raise ValueError('Invalid intent: ' + intent)
        self.__intent = intent
    
    def isOptionalArgument(self):
        return self.isArgument() and self.__optional
    
    def isPublic(self):
        return self.__public
    
    def isPrivate(self):
        return self.__private
    
    def setDeclaredIn(self, declaredIn, debugModuleName = '', debugLineNumber = 0):
        assertType(declaredIn, 'declaredIn', [str, SubroutineFullName, Type])
        assertType(debugModuleName, 'debugModuleName', str)    
        assertType(debugLineNumber, 'debugLineNumber', int)   
        
        if self.isArgument() and not isinstance(declaredIn, SubroutineFullName):
            msg = 'Arguments can only be declared in subroutines! Var: ' + str(self) + ", declaredIn: " + str(declaredIn) + " (" + debugModuleName + ":"
            if debugLineNumber > 0:
                msg += str(debugLineNumber)
            msg += ")"
            raise ValueError(msg)
        
        self.__declaredIn = declaredIn
        
    def isModuleVar(self):
        return isinstance(self.__declaredIn, str)
    
    def getDeclaredIn(self):
        return self.__declaredIn
    
    def setOriginalName(self, originalName):
        self.__originalName = originalName
        
    def getOriginalName(self):
        return self.__originalName
    
    def getAlias(self, name):
        alias = Variable(name, self.__typeName, self.__parameter, self.__allocatable, self.__pointer, self.__target, self.__dimension, self.__intent, self.__optional)
        alias.setDeclaredIn(self.__declaredIn)
        alias.setOriginalName(self.__name)
        if self.isTypeAvailable():
            alias.setType(self.getType())
        
        return alias
    
    def isAlias(self):
        return self.__originalName != self.__name
    
    def getOriginal(self):
        original = Variable(self.__originalName, self.__typeName, self.__parameter, self.__allocatable, self.__pointer, self.__target, self.__dimension, self.__intent, self.__optional)
        original.setDeclaredIn(self.__declaredIn)
        if self.isTypeAvailable():
            original.setType(self.getType())
        
        return original
    
    @staticmethod
    def validIdentifier(identifierWannabe):
        return isinstance(identifierWannabe, str) and IDENTIFIER_REG_EX.match(identifierWannabe) is not None
    
    @staticmethod
    def validType(typeWannabe):
        return isinstance(typeWannabe, str) and Variable.__typeRegEx.match(typeWannabe) is not None
    
    @staticmethod
    def validIntent(intentWannabe):
        return isinstance(intentWannabe, str) and intentWannabe in Variable.__intents
    
    @staticmethod
    def validVariableDeclaration(declarationStatementWannabe):
        return isinstance(declarationStatementWannabe, str) and \
           Variable.__declarationReg.match(declarationStatementWannabe) is not None and \
           ' FUNCTION ' not in declarationStatementWannabe.upper()
    
    @staticmethod
    def fromDeclarationStatement(declarationStatement, debugModuleName = '', debugLineNumber = 0):
        if not Variable.validVariableDeclaration(declarationStatement):
            raise ValueError("declarationStatement is not a valid variable declaration statement.");
        assertType(debugModuleName, 'debugModuleName', str)    
        assertType(debugLineNumber, 'debugLineNumber', int)    
        
        debug = debugModuleName != '' or debugLineNumber > 0
        
        result = []        

        varRegExMatch = Variable.__declarationReg.match(declarationStatement)
        typeSpecifiers = Variable.__extractListedElements(varRegExMatch.group('typespecifier'))
        parameter = False
        allocatable = False
        pointer = False
        target = False
        dimension = 0
        intent = ''
        optional = False
        public = False
        private = False
        for typeSpecifier in typeSpecifiers[1:]:
            typeSpecifierUpper = typeSpecifier.upper()
            if typeSpecifierUpper == 'PARAMETER':
                parameter = True
            elif typeSpecifierUpper == 'ALLOCATABLE':
                allocatable = True
            elif typeSpecifierUpper == 'POINTER':
                pointer = True
            elif typeSpecifierUpper == 'TARGET':
                target = True
            elif typeSpecifierUpper == 'OPTIONAL':
                optional = True
            elif typeSpecifierUpper == 'PUBLIC':
                public = True
            elif typeSpecifierUpper == 'PRIVATE':
                private = True
            else:
                intentRegExMatch = Variable.__intentRegEx.match(typeSpecifier)
                if intentRegExMatch != None:
                    intent = intentRegExMatch.group('intent')
                else:
                    dimensionRegExMatch = Variable.__dimensionRegEx.match(typeSpecifier)
                    if dimensionRegExMatch != None:
                        dimension = Variable.__extractDimension(dimensionRegExMatch.group('dimension'))

        variables = Variable.__extractListedElements(varRegExMatch.group('varlist'))
        
        for var in variables:
            varName = var
            varDimension = dimension
            equalsPos = varName.find('=')
            if equalsPos >= 0:
                varName = varName[:equalsPos].strip()
            bracketPos = varName.find('(')
            if bracketPos >= 0:
                varDimension = Variable.__extractDimension(varName[bracketPos + 1:-1])
                varName = varName[:bracketPos].strip()
            if debug and not Variable.validIdentifier(varName):
                msg = 'Parse Error: Found invalid variable indentifier: "' + varName + '" in statement: ' + declarationStatement + " (" + debugModuleName + ":"
                if debugLineNumber > 0:
                    msg += str(debugLineNumber)
                msg += ")"
                raise Exception(msg)
            result.append(Variable(varName, typeSpecifiers[0], parameter, allocatable, pointer, target, varDimension, intent, optional, public, private))
            
        return result
    
    @staticmethod
    def __extractListedElements(spec):
        spec = spec.strip(' :')
        elements = []
        bracketCount = 0
        element = ''
        for part in spec.split(','):
            for c in part:
                if c == '(': bracketCount += 1
                if c == ')': bracketCount -= 1
            element += ',' + part
            if bracketCount == 0:
                element = element.strip(' ,')
                if element != '':
                    elements.append(element)
                    element = ''
            
        return elements
    
    @staticmethod
    def __extractDimension(dimSpec):
        dimSpec = Variable.__removeBrackets(dimSpec)
        return len(dimSpec.split(','))
    
    @staticmethod
    def __removeBrackets(text):
        
        regEx = re.compile(r'.*\([^\(\)]*\).*')
        
        while regEx.match(text) is not None:
            text = re.sub(r'\([^\(\)]*\)', '', text)
            
        return text
    
class VariableReference(object):
    
    def __init__(self, expression, subroutine, lineNumber, level0Variable):
        assertType(expression, 'expression', str)
        assertType(subroutine, 'subroutine', SubroutineName)
        assertType(lineNumber, 'lineNumber', int)
        assertType(level0Variable, 'level0Variable', Variable)

        self.expression = re.sub(r'\([^\)]*\)', '', expression)
        self.level = self.expression.count('%');
        self.__subroutine = subroutine;
        self.__lineNumber = lineNumber;
        self.__originalName = None

        if level0Variable.getName() != self.getVariableName(0):
            raise ValueError("Variable name doesn't match: " + level0Variable.getName() + ' != ' + self.getVariableName(0))
        self.__level0Variable = level0Variable
    
    def __eq__(self, other):
        if (other is None or not isinstance(other, VariableReference)):
            return False;
        else:
            return self.expression == other.expression and \
                   self.__level0Variable == other.__level0Variable;
    
    def __ne__(self, other):
        return not self == other
        
    def __hash__(self):
        return hash(self.expression);
        
    def __str__(self):
        string = self.expression + ' (' + str(self.__subroutine)
        if self.__originalName is not None:
            string += ' => ' + self.__originalName
        string += ', ' + str(self.__lineNumber) + ')'
        
        return string
    
    def getExpression(self, level = -1):
        assertType(level, 'level', int)
        if level > self.getLevel():
            raise ValueError("level must not be higher than reference' highest level")
        
        if level < 0:
            return self.expression
        else:
            return '%'.join(self.expression.split('%', level + 1)[:level + 1])
    
    def getExpressionWithArray(self):
        exp = ''
        for level in self.getLevels():
            exp += self.getVariableName(level)
            dim = 0
            var = self.getVariable(level)
            if var is not None:
                dim = var.getDimension()
            if dim > 0:
                exp += '(' + ':,' * (dim - 1) + ':)'
            exp += '%'
        exp = exp.strip('%')
        return exp
    
    def getLevel(self):
        return self.level
    
    def getLevels(self, decrementing = False):
        assertType(decrementing, 'decrementing', bool)
        
        if decrementing:
            return range(self.getLevel(), -1, -1)
        else:
            return range(self.getLevel() + 1) 
        
    def getSubReference(self, level = -1):
        return VariableReference(self.getExpression(level), self.__subroutine, self.__lineNumber, self.__level0Variable)
    
    def getVariableName(self, level = 0):
        if level < 0 or level > self.getLevel():
            raise ValueError('Level out of range')
        
        currentLevel = 0
        start = 0
        end = self.expression.find('%')
        while currentLevel < level:
            currentLevel += 1
            start = end + 1
            end = self.expression.find('%', start)
        
        if end < 0:
            return self.expression[start:]
        else:
            return self.expression[start:end]
    
    def getLevel0Variable(self):
        return self.__level0Variable
    
    def getDeclaredIn(self):
        if self.__level0Variable is None:
            return None
        return self.__level0Variable.getDeclaredIn()
    
    def getVariable(self, level = 0):
        if level < 0 or level > self.getLevel():
            raise ValueError('Level out of range')

        var = self.getLevel0Variable()
        for l in range(1, level + 1):
            if not var.isTypeAvailable():
                return None
            typE = var.getType()
            varName = self.getVariableName(l)
            if not typE.hasMember(varName):
                return None
            var = typE.getMember(varName)
        
        return var
    
    def getLevelNVariable(self):
        return self.getVariable(self.getLevel())

    def getDimension(self, level = 0):
        var = self.getVariable(level)
        if var is None:
            return -1
        return var.getDimension()
    
    def getLevelNDimension(self):
        return self.getDimension(self.getLevel())
    
    def getTotalDimensions(self):
        dim = 0
        for level in self.getLevels():
            dimL = self.getDimension(level)
            if dimL < 0:
                return -1
            dim += dimL
        return dim
    
    def isOneVariableArray(self):
        for level in self.getLevels():
            variable = self.getVariable(level)
            if variable is not None and variable.isArray():
                return True
            
        return False
    
    def isReferencable(self):
        for level in self.getLevels(True):
            variable = self.getVariable(level)
            if variable is not None and (variable.isAllocatable() or variable.isPointer() or variable.isArray()):
                for cLevel in range(level - 1, -1, -1):
                    cVariable = self.getVariable(cLevel)
                    if cVariable is not None and cVariable.isArray():
                        return False
        return True
    
    def getMembers(self):
        if self.getLevel() == 0:
            return ''

        return self.expression[self.expression.find('%') + 1:]
    
    def setLevel0Variable(self, variable, members = ''):
        assertType(variable, 'variable', Variable)
        if self.getLevel() > 0:
            assert variable.hasDerivedType(), 'Level0Variable of VariableReferences with Level > 0 needs to have derived type.'
        assertType(members, 'members', str)

        if members != '':
            members = '%' + members
        
        self.__level0Variable = variable
        self.__renameLevel0Variable(self.__level0Variable.getName() + members)
        self.level = self.expression.count('%');
        
    def __renameLevel0Variable(self, newName):
        assertType(newName, 'newName', str)
        if newName != self.getVariableName(0):
            if self.__originalName is None and newName != self.__originalName:
                self.__originalName = self.getVariableName(0)
            percentPos = self.expression.find('%')
            if percentPos >= 0:
                newName += self.expression[percentPos:]
            self.expression = newName
            
    def cleanCopy(self):
        return VariableReference(self.expression, self.__subroutine, self.__lineNumber, self.__level0Variable)
        
    @staticmethod        
    def sort(variableReferences):
        if any(not isinstance(ref, VariableReference) for ref in variableReferences):
            raise ValueError('Some element is not a VariableReference')
        return sorted(variableReferences, key=attrgetter('level', 'expression'))

class SubroutineName(object):
    
    def __init__(self, name):
        assertType(name, 'name', str)
        self._name = name
        
    def __str__(self):
        return self._name;
    
    def __eq__(self, other):
        if type(other) != type(self):
            return False;
        return other._name == self._name;
    
    def __ne__(self, other):
        return not self == other
    
    def __hash__(self):
        return hash(self._name)
    
    def getSimpleName(self):
        raise NotImplementedError()
    
    def getModuleName(self):
        raise NotImplementedError()      
    
    @staticmethod
    def validIdentifier(identifierWannabe):
        return isinstance(identifierWannabe, str) and IDENTIFIER_REG_EX.match(identifierWannabe) is not None
    

class SubroutineFullName(SubroutineName):
    
    __fullNameRegEx = re.compile('^__[a-z0-9_]{1,63}_MOD_[a-z0-9_]{1,63}$', re.IGNORECASE)
    
    def __init__(self, name):
        if not SubroutineFullName.validFullName(name):
            raise ValueError("Not a valid SubroutineFullName: " + name + " (type: " + str(type(name)) + ")");
        
        super(SubroutineFullName, self).__init__(name.lower().replace('_mod_', '_MOD_'))
        
    def getSimpleName(self):
        modPos = self._name.find('_MOD_')
        return self._name[modPos + 5:];
    
    def getModuleName(self):
        modPos = self._name.find('_MOD_')
        return self._name[2:modPos]        
    
    @staticmethod
    def validFullName(subroutineName):
        if isinstance(subroutineName, SubroutineFullName):
            return True
        
        return isinstance(subroutineName, str) and SubroutineFullName.__fullNameRegEx.match(subroutineName) is not None

    @staticmethod
    def validParts(moduleName, simpleName):
        return SubroutineName.validIdentifier(moduleName) and SubroutineName.validIdentifier(simpleName)
    
    @staticmethod
    def fromParts(moduleName, simpleName):
        if not SubroutineFullName.validParts(moduleName, simpleName):
            raise ValueError('Invalid parts. (moduleName: ' + moduleName + ', simpleName: ' + simpleName + ')')
        
        return SubroutineFullName('__' + moduleName.lower() + '_MOD_' + simpleName.lower())


class InnerSubroutineName(SubroutineName): 
        
    __innerNameRegEx = re.compile('^[a-z0-9_]{1,63}\.\d+$', re.IGNORECASE)
    
    def __init__(self, name, hostName):
        if not InnerSubroutineName.validInnerSubroutineName(name) and not SubroutineName.validIdentifier(name):
            raise ValueError("Not a valid InnerSubroutineName: " + name + " (type: " + str(type(name)) + ")");
        assertType(hostName, 'hostName', SubroutineFullName)
        
        super(InnerSubroutineName, self).__init__(name.lower())
        self.__hostName = hostName
        
    def __eq__(self, other):
        if type(other) != type(self):
            return False
        elif other.__getDotPosition() < 0 or self.__getDotPosition() < 0:
            return other.getSimpleName() == self.getSimpleName() and other.getHostName() == self.getHostName()
        else:
            return other._name == self._name
    
    def __hash__(self):
        return hash((self.getSimpleName(), self.getModuleName()))
    
    def getSimpleName(self):
        dotPos = self.__getDotPosition()
        if dotPos < 0:
            return self._name
        return self._name[:self.__getDotPosition()]
    
    def __getDotPosition(self):
        return self._name.find('.')
    
    def getHostName(self):
        return self.__hostName
    
    def getModuleName(self):
        return self.getHostName().getModuleName()
    
    @staticmethod
    def validInnerSubroutineName(subroutineName):
        if isinstance(subroutineName, InnerSubroutineName):
            return True
        
        return isinstance(subroutineName, str) and InnerSubroutineName.__innerNameRegEx.match(subroutineName) is not None

        
class SubroutineContainer(object):

    def __init__(self, lines):
        if self.__class__ == SubroutineContainer:
            raise NotImplementedError()
        
        assertTypeAll(lines, 'lines', tuple)
        
        self.__lines = lines
        self.__statements = None
        self.__subroutines = None 
        self.__lastUseStatementIndex = None
        self.__containsStatementIndex = None
        
    def __iter__(self):
        return iter(self.getLines());
        
    def __contains__(self, name):
        return self.getSubroutine(name) is not None 
    
    def getName(self):
        raise NotImplementedError()
    
    def isInner(self):
        return isinstance(self.getName(), InnerSubroutineName)
    
    def hasContainer(self):
        raise NotImplementedError()
    
    def getContainer(self):
        raise NotImplementedError()
    
    def getLines(self):
        return self.__lines;
    
    def getLine(self, lineNumber):
        assertType(lineNumber, 'lineNumber', int)
        
        first = self.getFirstLineNumber()
        last = self.getLastLineNumber()
        if lineNumber < first or lineNumber > last:
            raise ValueError('lineNumber out of range [' + str(first) + ', ' + str(last) + ']: ' + str(lineNumber))
        
        return self.getLines()[lineNumber - first][1]
    
    def getFirstLineNumber(self):
        return self.getLines()[0][0];
    
    def getLastLineNumber(self):
        return self.getLines()[-1][0];
    
    def getStatements(self):
        if self.__statements is None:
            self.__statements = SourceFile.linesToStatements(self.getLines())
        return self.__statements
    
    def getSubroutines(self):
        if self.isInner():
            return dict()
        
        if self.__subroutines is None:
            self.__subroutines = self.__findSubroutines()
        
        return self.__subroutines    
    
    def getSubroutine(self, name):
        raise NotImplementedError()
    
    def __findSubroutines(self):
        subroutineRegEx = re.compile(r'\s*(((ELEMENTAL)|(PURE)|(RECURSIVE))\s+)*SUBROUTINE\s+(?P<name>[a-z0-9_]{1,63})', re.IGNORECASE);
        functionRegEx = re.compile(r'\s*(((ELEMENTAL)|(PURE)|(RECURSIVE)|(INTEGER)|(LOGICAL)|(DOUBLE(\s+PRECISION)?)|(REAL(\(.*\))?)|(CHARACTER(\(.*\))?)|(TYPE(\(.*\))?)|(CLASS(\(.*\))?))\s+)*FUNCTION\s+(?P<name>[a-z0-9_]{1,63})', re.IGNORECASE);
        endRegEx = re.compile(r'\s*END\s*((SUBROUTINE)|(FUNCTION))', re.IGNORECASE);
        
        lines = self.getLines()
        offset = lines[0][0] - 1
        subroutines = dict();
                
        subroutineStack = 0;
        name = None;
        subroutineLines = None;
        firstLine = -1
        statements = self.getStatementsAfterContains()
        for i, (sn, line, _) in enumerate(statements):
            regExMatch = subroutineRegEx.match(line);
            if regExMatch is None:
                regExMatch = functionRegEx.match(line);
            if regExMatch is not None:
                subroutineStack += 1;
                if subroutineStack == 1:
                    name = regExMatch.group('name');
                    if i > 0:
                        lastStatementBeforeLine = statements[i - 1][0]
                    else:
                        lastStatementBeforeLine = self.getContainsLineNumber()
                    firstLine = sn
                    for ln, line in reversed(lines[lastStatementBeforeLine - offset:sn - offset - 1]):
                        line = line.strip()
                        if len(line) == 0:
                            if firstLine < sn:
                                break;
                        else:
                            firstLine = ln;
            elif endRegEx.match(line) is not None:
                subroutineStack -= 1
                if subroutineStack == 0:
                    fullName = self._createSubroutineName(name)
                    subroutineLines = lines[(firstLine - offset - 1):(sn - offset)]
                    subroutines[name.lower()] = Subroutine(fullName, subroutineLines, self);

        return subroutines;
    
    def _createSubroutineName(self, name):
        raise NotImplementedError()
    
    def getDeclarationLineNumber(self):
        return self.getStatements()[0][0];
    
    def getDeclaration(self):
        return self.getStatements()[0][1];
    
    def getStatementsBeforeContains(self):
        '''NOT including CONTAINS statement'''
        return self.getStatements()[:self.__getContainsStatementIndex()]
        
    def getStatementsAfterContains(self):
        '''NOT including CONTAINS statement'''
        if self.__getContainsStatementIndex() < 0:
            return []
        
        return self.getStatements()[self.__getContainsStatementIndex() + 1:]
        
    def getContainsLineNumber(self):
        if self.__getContainsStatementIndex() < 0:
            return -1
        
        return self.getStatements()[self.__getContainsStatementIndex()][0]
    
    def __getContainsStatementIndex(self):
        if self.__containsStatementIndex is None:
            self.__containsStatementIndex = self.__findContainsStatementIndex()
            
        return self.__containsStatementIndex
    
    def __findContainsStatementIndex(self):
        lastUseIndex = self.__getLastUseStatementIndex()
        for i, (_, statement, _) in enumerate(self.getStatements()[lastUseIndex + 1:]):
            if statement.upper() == 'CONTAINS':
                return i + lastUseIndex + 1
        
        return -1

    def getUseStatements(self):
        '''Including last USE statement'''
        return self.getStatements()[1:self.__getLastUseStatementIndex() + 1]

    def getStatementsAfterUse(self):
        '''NOT including last USE statement'''
        return self.getStatements()[self.__getLastUseStatementIndex() + 1:]
    
    def getLastUseLineNumber(self):
        return self.getStatements()[self.__getLastUseStatementIndex()][2]
    
    def __getLastUseStatementIndex(self):
        if self.__lastUseStatementIndex is None:
            self.__lastUseStatementIndex = self.__findLastUseStatementIndex()
            
        return self.__lastUseStatementIndex
        
    def __findLastUseStatementIndex(self):
        useRegEx = re.compile(r'^USE[\,\s\:].*$', re.IGNORECASE); 
        index = 0
        for _, statement, _ in self.getStatements()[1:]:
            if useRegEx.match(statement) is not None:
                index += 1
            else:
                break

        return index   
    
class Subroutine(SubroutineContainer):
    def __init__(self, name, lines, container):
        assertType(name, 'name', SubroutineName)
        if isinstance(name, InnerSubroutineName):
            assertType(container, 'container', Subroutine)
        else:
            assertType(container, 'container', Module)
        
        super(Subroutine, self).__init__(lines)
        
        self.__name = name
        self.__container = container
        self.__variables = None
    
    def getName(self):
        return self.__name;
    
    def hasContainer(self):
        return self.getContainer() is not None
    
    def getContainer(self):
        return self.__container
    
    def getSimpleName(self):
        return self.getName().getSimpleName()
    
    def getModuleName(self):
        return self.getName().getModuleName()

    def isInnerSubroutine(self):
        return isinstance(self.getContainer(), Subroutine)
    
    def getModule(self):
        if self.isInnerSubroutine():
            return self.getContainer().getModule()
        return self.getContainer()
    
    def getSourceFile(self):
        return self.getModule().getSourceFile()
  
    def getSubroutine(self, name):
        assertType(name, 'name', InnerSubroutineName)
        
        subroutines = self.getSubroutines()
        if name.getSimpleName() in subroutines:
            return subroutines[name.getSimpleName()]

        return None

    
    def getLastSpecificationLineNumber(self):
        useRegEx = re.compile(r'^USE\s+([a-z0-9_]+).*$', re.IGNORECASE); 
        additionRegEx = re.compile(r'^((CONTIGUOUS)|(DIMENSION))\s*\:\:\s*([a-z0-9_]+).*$', re.IGNORECASE); 
        statements = self.getStatementsAfterUse()
        lastLine = self.getLastUseLineNumber()
        i = 2
        while i < len(statements):
            statement = statements[i - 1][1]
            if Variable.validVariableDeclaration(statement) or useRegEx.match(statement) is not None or additionRegEx.match(statement) is not None:
                lastLine = statements[i - 1][2]
            else:
                break;
            i = i + 1
        
        return lastLine
    

    def getArgumentNames(self):
        declaration = self.getDeclaration()
        argumentsListing = declaration[(declaration.find('(') + 1):(declaration.find(')'))]
        arguments = argumentsListing.split(',')
        arguments = map(str.strip, arguments)
        return arguments
    
    def getArguments(self):
        argumentNames = self.getArgumentNames()
        arguments = len(argumentNames) * [None]
        for variable in self.getVariables():
            if variable.isArgument():
                arguments[argumentNames.index(variable.getName())] = variable
        return arguments
    
    def findArgument(self, name):
        assertType(name, 'name', str)
        if not Variable.validIdentifier(name):
            raise ValueError("Not a valid identifier: " + name);

        for argument in self.getArguments():
            if argument.getName() == name:
                return argument
        return None
    
    def getInArguments(self):
        return [a for a in self.getArguments() if a.isInArgument()]
    
    def getOutArguments(self):
        return [a for a in self.getArguments() if a.isOutArgument()]
    
    def getDerivedTypeArguments(self):
        return [a for a in self.getArguments() if a.hasDerivedType()]
    
    def getBuiltInTypeArguments(self):
        return [a for a in self.getArguments() if a.hasBuiltInType()]

    def hasVariable(self, name):
        name = name.lower()
        for variable in self.getVariables():
            if variable.getName().lower() == name:
                return True
            
        return False
            
    def getVariable(self, name):
        name = name.lower()
        for variable in self.getVariables():
            if variable.getName().lower() == name:
                return variable
            
        return None
    
    def getVariables(self):
        if self.__variables is None:
            self.__variables = self.__findVariables()
            
        return self.__variables
        
    def __findVariables(self):
        argumentNames = self.getArgumentNames()
        foundOne = False
        variables = []
        for i, statement, _ in self.getStatements():
            if Variable.validVariableDeclaration(statement):
                for variable in Variable.fromDeclarationStatement(statement):
                    variables.append(variable)
                foundOne = True
            elif foundOne == True:
                break;
                
        for variable in variables:
            variable.setDeclaredIn(self.getName(), self.getModuleName(), i)
            if not variable.isArgument() and variable.getName() in argumentNames:
                variable.setIntent('inout')
        return variables
    
    def _createSubroutineName(self, name):
        return InnerSubroutineName(name, self.getName())
    
class Module(SubroutineContainer):
    def __init__(self, name, lines, sourceFile):
        assertType(sourceFile, 'sourceFile', SourceFile)
        
        super(Module, self).__init__(lines)
        
        self.__name = name.lower()
        self.__sourceFile = sourceFile
        self.__variableList = None
        self.__publicElements = None
        self.__useAliases = None
        
    def getName(self):
        return self.__name;
    
    def hasContainer(self):
        return False
    
    def getContainer(self):
        return None
    
    def getSubroutine(self, name):
        assertType(name, 'name', SubroutineName)
        
        subroutines = self.getSubroutines()
        if isinstance(name, InnerSubroutineName):
            host = self.getSubroutine(name.getHostName())
            if host is not None:
                return host.getSubroutine(name)
        elif name.getSimpleName() in subroutines:
            return subroutines[name.getSimpleName()]

        return None

    def getSourceFile(self):
        return self.__sourceFile
        
    def isPublic(self):
        for _, statement, _ in self.getStatementsBeforeContains():
            if statement.upper() == 'PRIVATE':
                return False
            if statement.upper() == 'PUBLIC':
                return True

        return True

    def hasVariable(self, name):
        return name.lower() in self.getVariables()
            
    def getVariable(self, name):
        if self.hasVariable(name):
            return self.getVariables()[name.lower()]
        return None
    
    def getVariables(self):
        if self.__variableList is None:
            self.__variableList = self.__findVariables()
        return self.__variableList
     
    def __findVariables(self):
        typeRegEx = re.compile(r'^((TYPE)|(CLASS))\s*(,\s*[A-Z]+(\([a-z0-9_]+\))?\s*)*((\:\:)|\s)\s*([a-z0-9_]+)$', re.IGNORECASE);
        endTypeRegEx = re.compile(r'^END\s+TYPE(\s+[a-z0-9_]+)?$', re.IGNORECASE);
        interfaceRegEx = re.compile(r'^INTERFACE(\s+([a-z0-9_]+))?$', re.IGNORECASE);
        endInterfaceRegEx = re.compile(r'^END\s+INTERFACE(\s+[a-z0-9_]+)?$', re.IGNORECASE);
         
        moduleVariables = dict()        
        inType = False
        inInterface = False
        for i, statement, _ in self.getStatementsBeforeContains():
            if typeRegEx.match(statement) is not None:
                inType = True
            elif endTypeRegEx.match(statement) is not None:
                inType = False
            elif interfaceRegEx.match(statement) is not None:
                inInterface = True
            elif endInterfaceRegEx.match(statement) is not None:
                inInterface = False
            elif not inType and not inInterface and Variable.validVariableDeclaration(statement):
                for variable in Variable.fromDeclarationStatement(statement):
                    variable.setDeclaredIn(self.getName(), self.getName(), i)
                    moduleVariables[variable.getName().lower()] = variable
                     
        return moduleVariables
    
    def getPublicElements(self):
        if self.__publicElements is None:
            self.__publicElements = self.__findPublicElements()
        return self.__publicElements
    
    def __findPublicElements(self):
        publicRegEx = re.compile(r'^PUBLIC\s*((\:\:)|\s)\s*(?P<elementList>[a-z0-9_,\s]+)$', re.IGNORECASE);
        
        elements = []
        for _, statement, _ in self.getStatementsBeforeContains():
            publicRegExMatch = publicRegEx.match(statement)
            if publicRegExMatch is not None:
                elementList = publicRegExMatch.group('elementList')
                for element in elementList.split(','):
                    element = element.strip()
                    elements.append(element.lower())
                    
        return elements
    
    def getUseAliases(self):
        if self.__useAliases is None:
            self.__useAliases = self.__findUseAliases()
        return self.__useAliases
        
    def __findUseAliases(self):
        
        useOnlyRegEx = re.compile(r'^USE(\s*\,\s*INTRINSIC)?[\s\:]+(?P<modulename>[a-z0-9_]+)\s*\,\s*ONLY\s*\:\s*(?P<importlist>.*)$', re.IGNORECASE)
        lastUseLine = self.getLastUseLineNumber()

        aliases = dict()
        for _, statement, j in self.getStatements():
            if j > lastUseLine:
                break
            
            useOnlyRegExMatch = useOnlyRegEx.match(statement) 
            if useOnlyRegExMatch is not None:
                moduleName = useOnlyRegExMatch.group('modulename')
                importList = useOnlyRegExMatch.group('importlist').split(',')
                for imported in importList:
                    if imported.find('=>') > -1:
                        names = imported.split('=>')
                        aliases[names[0].strip()] = (moduleName, names[1].strip())
                        
        return aliases
    
    def _createSubroutineName(self, name):
        return SubroutineFullName.fromParts(self.getName(), name)

class SourceFile(object):
    def __init__(self, path):
        if not os.path.isfile(path) and os.access(path, os.R_OK):
            raise IOError("Not a readable file: " + path);
        
        self.__path = path
        self.__modules = self.__extractModules()
        
    def __str__(self):
        return self.__path;
        
    def __contains__(self, name):
        if isinstance(name, str):
            return name.lower() in self.__modules
        elif isinstance(name, SubroutineFullName):
            moduleName = name.getModuleName().lower()
            if moduleName not in self:
                return False
            else:
                return name in self.__modules[moduleName]
        
        return False
    
    def getPath(self):
        return self.__path
    
    def getLines(self):
        lines = [];
        openFile = open(self.__path);
        i = 1;
        for line in openFile:
            lines.append((i, line));
            i = i + 1;
        openFile.close();
        
        return lines;
    
    def getStatements(self):
        return SourceFile.linesToStatements(self.getLines())
    
    def getModules(self):
        return self.__modules
    
    def __getAnyModule(self):
        if self.__modules:
            return self.__modules.values()[0]
        return None
    
    def getModule(self, moduleName):
        assertType(moduleName, 'moduleName', str)
        moduleName = moduleName.lower()
        if moduleName in self.__modules:
            return self.__modules[moduleName.lower()];
        
        return None
    
    def getSubroutine(self, subroutineName):
        assertType(subroutineName, 'moduleName', SubroutineName)
        return self.getModule(subroutineName.getModuleName()).getSubroutine(subroutineName);
    
    def __extractModules(self):
        moduleRegEx = re.compile(r'\s*MODULE\s+(?P<name>[a-z0-9_]{1,63})', re.IGNORECASE);
        endRegEx = re.compile(r'\s*END\s*MODULE', re.IGNORECASE);
        
        lines = self.getLines()
        statements = SourceFile.linesToStatements(lines)
        modules = dict()
                
        inModule = False
        name = None
        firstLine = -1
        for i, (sn, line, _) in enumerate(statements):
            if not inModule:
                regExMatch = moduleRegEx.match(line);
                if regExMatch is not None:
                    inModule = True;
                    name = regExMatch.group('name');
                    firstLine = sn
                    
                    if sn > 1: 
                        if i > 0:
                            lastStatementBeforeLine = statements[i - 1][0]
                        else:
                            lastStatementBeforeLine = 0
                        for ln, line in reversed(lines[lastStatementBeforeLine:sn - 1]):
                            line = line.strip()
                            if len(line) == 0:
                                if firstLine < sn:
                                    break;
                            else:
                                firstLine = ln;
            else:
                if endRegEx.match(line) is not None:
                    inModule = False;
                    moduleLines = lines[firstLine - 1:sn]
                    modules[name.lower()] = Module(name, moduleLines, self)

        return modules;

    
    def getModuleName(self):
        '''DEPRECATED'''
        if self.__modules:
            return self.__modules.values()[0].getName()
        return ''
    
    def getContainsLineNumber(self):
        '''DEPRECATED'''
        if self.__modules:
            return self.__getAnyModule().getContainsLineNumber()
        return -1
    
    def getLastUseLineNumber(self):
        '''DEPRECATED'''
        if self.__modules:
            return self.__getAnyModule().getLastUseLineNumber()
        return -1
    
    def isPublic(self):
        '''DEPRECATED'''
        if self.__modules:
            return self.__getAnyModule().isPublic()
        return False
    
    def getModuleVariables(self):
        '''DEPRECATED'''
        if self.__modules:
            return self.__getAnyModule().getVariables()
        return dict()
    
    def getPublicElements(self):
        '''DEPRECATED'''
        if self.__modules:
            return self.__getAnyModule().getPublicElements()
        return []
    
    def getUseAliases(self):
        '''DEPRECATED'''
        if self.__modules:
            return self.__getAnyModule().getUseAliases()
        return dict()

    
    @staticmethod
    def linesToStatements(lines):
        statements = [];
        statement = '';
        j = lines[0][0];
        for i, line in lines:
            line = SourceFile.__removeCommentFromLine(line).strip();
            if line and not line.startswith('#'):
                statement += ' ' + line.strip('&').strip()
                if not line.endswith('&'):
                    statement = statement.lstrip();
                    statement = SourceFile.__removeUnnecessaryBlanksFromStatement(statement)
                    statement = SourceFile.__removeMultipleBlanksFromStatement(statement)
                    statement = SourceFile.__removeStringsFromStatement(statement)
                    statements.append((j, statement.strip(), i));
                    statement = '';
                    j = i + 1;
            else: 
                if statement == '':
                    j = j + 1;
        return statements;
    
    @staticmethod
    def __removeCommentFromLine(line):
        'TODO Testen!!!'
        cleanLine = '';
        quotation = '';
        for index, char in enumerate(line):
            if not quotation:
                if char == '!':
                    return cleanLine;
                elif char == '"' or char == "'":
                    quotation = char; 
            elif char == quotation and index != '\\':
                quotation = '';
            cleanLine += char;
        
        return cleanLine;
    
    @staticmethod
    def __removeUnnecessaryBlanksFromStatement(statement):
        statement = re.sub(r' *([%\(,\:=]) *', r'\1', statement)
        statement = re.sub(r' *\)', r')', statement)
        return statement
    
    @staticmethod
    def __removeMultipleBlanksFromStatement(statement):
        return re.sub(r'  +', ' ', statement);
    
    @staticmethod
    def __removeStringsFromStatement(statement):
        cleanStatement = ''
        inString = False
        escape = False
        delimiter = ''
        for c in statement:
            if not inString:
                cleanStatement += c
                if c == "'" or c == '"':
                    inString = True
                    escape = False
                    delimiter = c;
            else:
                if c == delimiter and not escape:
                    cleanStatement += c
                    inString = False
                escape = not escape and c == '\\'
              
        return cleanStatement

class SourceFiles(object):
    def __init__(self, baseDir, specialModuleFiles={}):
        assertType(specialModuleFiles, 'specialModuleFiles', dict)
        if not os.path.isdir(baseDir):
            raise IOError("Not a directory: " + baseDir)
        
        self.__baseDir = baseDir
        self.__filesByPath = dict()
        self.__filesByModules = dict()
        self.setSpecialModuleFiles(specialModuleFiles)
        
    def getSpecialModuleFiles(self):
        return self.__specialModuleFiles
    
    def setSpecialModuleFiles(self, specialModuleFiles):
        assertType(specialModuleFiles, 'specialModuleFiles', dict)
        
        self.__specialModuleFiles = dict()
        for module, file in specialModuleFiles.iteritems():
            self.__specialModuleFiles[module.lower()] = file

        self.__filesByModules = dict() # Clear Module Cache
        
    def getBaseDir(self):
        return self.__baseDir
    
    def existsSubroutine(self, subroutineName):
        assertType(subroutineName, 'subroutineName', SubroutineName)
        
        sourceFile = self.findModuleFile(subroutineName.getModuleName())
        return sourceFile is not None and subroutineName in sourceFile
        
    def findSubroutine(self, subroutineName):    
        assertType(subroutineName, 'subroutineName', SubroutineName)
        
        sourceFile = self.findModuleFile(subroutineName.getModuleName())
        if sourceFile is None:
            print >> sys.stderr, '*** WARNING [SourceFiles]: Module file not found for subroutine: ' + str(subroutineName) + '. ***'
            return None
        else:
            return sourceFile.getSubroutine(subroutineName)

    def existsModule(self, moduleName):
        assertType(moduleName, 'moduleName', str)
        
        sourceFile = self.findModuleFile(moduleName)
        return sourceFile is not None and moduleName in sourceFile
        
    def findModule(self, moduleName):    
        assertType(moduleName, 'moduleName', str)
        
        sourceFile = self.findModuleFile(moduleName)
        if sourceFile is None:
            return None
        else:
            return sourceFile.getModule(moduleName)
        
    def findModuleFile(self, moduleName):
        assertType(moduleName, 'moduleName', str)
        
        if moduleName not in self.__filesByModules:
            fileName = self.__getModuleFileName(moduleName)
            sourceFile = self.findSourceFile(fileName)
            self.__filesByModules[moduleName] = sourceFile
            
        return self.__filesByModules[moduleName]
    
    def existsSourceFile(self, fileName):
        assertType(fileName, 'fileName', str)
        
        sourceFile = self.findSourceFile(fileName)
        return sourceFile is not None    
    
    def findSourceFile(self, fileName):
        path = self.__findFile(fileName)
        if path is None:
            sourceFile = None
        elif path in self.__filesByPath:
            sourceFile = self.__filesByPath[path]
        else:
            sourceFile = SourceFile(path) 
            self.__filesByPath[path] = sourceFile
        
        return sourceFile
        
    def __getModuleFileName(self, moduleName):
        if moduleName.lower() in self.__specialModuleFiles:
            return self.__specialModuleFiles[moduleName.lower()]
        else:
            return moduleName + '.f90'
        
    def __findFile(self, fileName):
        for root, _, files in os.walk(self.__baseDir):
            for name in files:
                if name.replace('.F90', '.f90') == fileName.replace('.F90', '.f90'):
                    return os.path.join(root, name)
        return None
    
    def clearCache(self):
        self.__filesByPath = dict()
        self.__filesByModules = dict()
