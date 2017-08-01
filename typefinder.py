# coding=utf8

import re
from utils import assertType
from source import Type, Variable, Module
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

    def parseStatement(self, i, statement, j, module):
        assertType(i, 'i', int) 
        assertType(statement, 'statement', str) 
        assertType(j, 'j', int)
        assertType(module, 'module', Module)
        
        moduleName = module.getName()
        typeRegEx = re.compile(r'^((TYPE)|(CLASS))\s*(,\s*((PUBLIC)|(PRIVATE))\s*)?(,\s*EXTENDS\((?P<extends>[a-z0-9_]+)\)\s*)?(,\s*((PUBLIC)|(PRIVATE))\s*)?((\:\:)|\s)\s*(?P<typename>[a-z0-9_]+)$', re.IGNORECASE);
        endTypeRegEx = re.compile(r'^END\s*((TYPE)|(CLASS))(\s+[a-z0-9_]+)?$', re.IGNORECASE);
        
        if self.__currentType is None:
            typeRegExMatch = typeRegEx.match(statement)
            if typeRegExMatch is not None and statement.upper() != 'CLASS DEFAULT':
                typeName = typeRegExMatch.group('typename').lower()
                self.__currentType = Type(typeName, module)
                if 'extends' in typeRegExMatch.groupdict() and typeRegExMatch.group('extends') is not None:
                    self.__extends[self.__currentType] = typeRegExMatch.group('extends')
        else:
            if Variable.validVariableDeclaration(statement):
                members = Variable.fromDeclarationStatement(statement, moduleName, i)
                self.__currentType.addMembers(members)
            else:
                endTypeRegExMatch = endTypeRegEx.match(statement)
                if endTypeRegExMatch is not None:
                    name = self.__currentType.getName()
#                     if name in self.__types:
#                         if isinstance(self.__types[name], list):
#                             self.__types[name].append(self.__currentType)
#                         else:
#                             self.__types[name] = [self.__types[name], self.__currentType]
#                     else:
                    self.__types[name] = self.__currentType
                        
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

class TypeCollection:

    def __init__(self):
        self.__typeDict = dict()
        self.__typeSet = set()
        self.__memberTypesSet = False
        self.__extends = dict()
        self.__extendedTypesSet = False
        
    def __contains__(self, name):
        return name in self.__typeDict and len(self.__typeDict[name]) > 0
    
    def __iter__(self):
        return iter(self.__typeSet);
        
    def addType(self, typE):
        assertType(typE, 'typE', Type)
        
        name = typE.getName()
        if name not in self:
            self.__typeDict[name] = [typE]
        else:
            self.__typeDict[name].append(typE)
        self.__typeSet.add(typE)

    def getTypeByName(self, name, index = 0):
        assertType(name, 'name', str)
        assertType(index, 'index', int)
        
        if name in self.__typeDict:
            types = self.__typeDict[name]
            if index < len(types):
                return types[index]
        
        return None

    def getTypeOfVariable(self, var):
        assertType(var, 'var', Variable)

        #TODO Testen!!!
        
        if var.isTypeAvailable():
            return var.getType()
        
        if var.hasDerivedType():
            typeName = var.getDerivedTypeName()
            declaredIn = var.getDeclaredIn()
            if declaredIn is not None: 
                module = declaredIn.getModule()
                 
            # Type imported with alias?
            if module is not None:
                for useModuleName, useImports in module.getUses():
                    for alias, original in useImports:
                        if alias == typeName:
                            if original in self:
                                for typE in self.__typeDict[original]:
                                    if typE.getModule() == module:
                                        return typE
            # Else
            if typeName in self:
                types = self.__typeDict[typeName]
                if len(types) > 1:
                        if module is not None:
                            # Declared in the same module?
                            for typE in types:
                                if typE.getModule() == module:
                                    return typE
                            # Type explictly imported in variable's module?
                            for useModuleName, useImports in module.getUses():
                                for alias, original in useImports:
                                    if original == typeName:
                                        for typE in types:
                                            if typE.getModule() == useModuleName:
                                                return typE 
                            # Type imported by wildcard in variable's module?
                            for useModuleName, useImports in module.getUses():
                                for alias, original in useImports:
                                    if alias == '*':
                                        for typE in types:
                                            if typE.getModule() == useModuleName:
                                                return typE 
                        elif isinstance(module, Module):
                            for typE in types:
                                if typE.getDeclaredIn() == module.getName():
                                    return typE
                    #TODO Warning
                return types[0]
                
        return None
    
    def setMemberTypes(self):
        for typE in self:
            for member in typE.getMembers():
                memberType = self.getTypeOfVariable(member)
                if memberType is not None:
                    member.setType(memberType)
            