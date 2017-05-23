# coding=utf8

import re
from utils import assertType
from source import Type, Variable
from supertypes import UseTraversalPassenger

class TypeFinder(UseTraversalPassenger):

    def __init__(self):
        self.reset()
        
    def reset(self):      
        self.__types = dict()
        self.__currentType = None
        self.__memberTypesSet = False
        self.__extends = dict()
        self.__extendedTypesSet = False
        
    def getResult(self):
        if not self.__memberTypesSet:
            for typE in self.__types.values():
                for member in typE.getMembers():
                    if member.hasDerivedType() and member.getDerivedTypeName() in self.__types:
                        member.setType(self.__types[member.getDerivedTypeName()])
            self.__memberTypesSet = True
        if not self.__extendedTypesSet:
            for typE, extends in self.__extends.iteritems():
                if extends in self.__types:
                    typE.setExtends(self.__types[extends])
            self.__extendedTypesSet = True
            
        return self.__types

    def parseStatement(self, i, statement, j, moduleName):
        assertType(i, 'i', int) 
        assertType(statement, 'statement', str) 
        assertType(j, 'j', int)
        assertType(moduleName, 'moduleName', str)
        
        typeRegEx = re.compile(r'^((TYPE)|(CLASS))\s*(,\s*EXTENDS\((?P<extends>[a-z0-9_]+)\)\s*)?((\:\:)|\s)\s*(?P<typename>[a-z0-9_]+)$', re.IGNORECASE);
        endTypeRegEx = re.compile(r'^END\s+((TYPE)|(CLASS))(\s+[a-z0-9_]+)?$', re.IGNORECASE);
        
        if self.__currentType is None:
            typeRegExMatch = typeRegEx.match(statement)
            if typeRegExMatch is not None and statement.upper() != 'CLASS DEFAULT':
                typeName = typeRegExMatch.group('typename').lower()
                self.__currentType = Type(typeName, moduleName)
                if 'extends' in typeRegExMatch.groupdict() and typeRegExMatch.group('extends') is not None:
                    self.__extends[self.__currentType] = typeRegExMatch.group('extends')
        else:
            if Variable.validVariableDeclaration(statement):
                members = Variable.fromDeclarationStatement(statement, moduleName, i)
                for member in members:
                    member.setDeclaredIn(self.__currentType, moduleName, i)
                self.__currentType.addMembers(members)
            else:
                endTypeRegExMatch = endTypeRegEx.match(statement)
                if endTypeRegExMatch is not None:
                    self.__types[self.__currentType.getName()] = self.__currentType
                    self.__currentType = None

    def __extractListedElements(self, spec):
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