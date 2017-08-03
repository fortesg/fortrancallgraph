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
        self.__currentExtends = None
        self.__collection = TypeCollection()
        
    def getResult(self):
        self.__collection.finalize()
        return self.__collection

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
                    self.__currentExtends = typeRegExMatch.group('extends')
        else:
            if Variable.validVariableDeclaration(statement):
                members = Variable.fromDeclarationStatement(statement, moduleName, i)
                self.__currentType.addMembers(members)
            else:
                endTypeRegExMatch = endTypeRegEx.match(statement)
                if endTypeRegExMatch is not None:
                    self.__collection.addType(self.__currentType, self.__currentExtends)
                    self.__currentType = None
                    self.__currentExtends = None

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
        self.__extends = dict()
        
    def __contains__(self, name):
        return name in self.__typeDict and len(self.__typeDict[name]) > 0
    
    def __iter__(self):
        return iter(self.__typeSet)
     
    def __len__(self):
        return len(self.__typeSet)
        
    def addType(self, typE, extendsName = None):
        assertType(typE, 'typE', Type)
        assertType(extendsName, 'extendsName', str, True)
        
        name = typE.getName()
        if name not in self:
            self.__typeDict[name] = [typE]
        else:
            self.__typeDict[name].append(typE)
        self.__typeSet.add(typE)
        
        if extendsName is not None:
            self.__extends[typE] = extendsName
        
    def finalize(self):
        self.__setMemberTypes()
        self.__setExtendedTypes()

    def getType(self, typeName, usingModule = None):
        assertType(typeName, 'typeName', str)
        assertType(usingModule, 'usingModule', Module, True)
        
        # Type imported with alias?
        if usingModule is not None:
            for alias, (usedModuleName, original) in usingModule.getUseAliases().iteritems():
                if alias == typeName:
                    if original in self:
                        for typE in self.__typeDict[original]:
                            if typE.getModule() == usedModuleName:
                                return typE
        # Else
        if typeName in self:
            types = self.__typeDict[typeName]
            if len(types) == 1:
                return types[0]
            elif usingModule is not None:
                # Declared in the same usingModule?
                for typE in types:
                    if typE.getModule() == usingModule:
                        return typE
                # Type imported?
                for use in usingModule.getUses():
                    if len(use) == 1 or use[1] == typeName: 
                        for typE in types:
                            if typE.getModule().getName() == use[0]:
                                return typE 
            elif isinstance(usingModule, Module):
                for typE in types:
                    if typE.getDeclaredIn() == usingModule.getName():
                        return typE
            #TODO Warning
            
        return None

    def getTypeOfVariable(self, var):
        assertType(var, 'var', Variable)

        #TODO Testen!!!
        if var.isTypeAvailable():
            return var.getType()
        elif var.hasDerivedType():
            typeName = var.getDerivedTypeName()
            declaredIn = var.getDeclaredIn()
            if declaredIn is not None: 
                module = declaredIn.getModule()
            else:
                module = None

            return self.getType(typeName, module)
                
        return None
    
    def __setMemberTypes(self):
        for typE in self:
            for member in typE.getMembers():
                if member.hasDerivedType() and not member.isTypeAvailable():
                    memberType = self.getTypeOfVariable(member)
                    if memberType is not None:
                        member.setType(memberType)
                    
    def __setExtendedTypes(self):
        for typE, extends in self.__extends.iteritems():
            extendsType = self.getType(extends, typE.getModule())
            if extendsType is not None:
                typE.setExtends(extendsType)
        self.__extends = dict()
            