# coding=utf8

import re
from assertions import assertType
from source import Type, Variable, Module
from supertypes import UseTraversalPassenger
from printout import printWarning

class TypeFinder(UseTraversalPassenger):

    def __init__(self, abstractTypes = {}):
        assertType(abstractTypes, 'abstractTypes', dict)
        self.reset()
        self.__abstractTypes = abstractTypes
        
    def reset(self):      
        self.__currentType = None
        self.__currentExtends = None
        self.__collection = TypeCollection()
        
    def getResult(self):
        self.__collection.finalize(self.__abstractTypes)
        return self.__collection

    def parseStatement(self, i, statement, j, module):
        assertType(i, 'i', int) 
        assertType(statement, 'statement', str) 
        assertType(j, 'j', int)
        assertType(module, 'module', Module)
        
        moduleName = module.getName()
        endTypeRegEx = Type.END_REGEX
        procedureRegEx = re.compile(r'^PROCEDURE\s*(\(.*\))?\s*(\,.*)?\:\:\s*(?P<alias>[a-z0-9_]+)\s*(\=\>\s*(?P<procedure>[a-z0-9_]+))?$', re.IGNORECASE)
        genericRegEx = re.compile(r'^GENERIC\s*\:\:\s*(?P<alias>[a-z0-9_]+)\s*\=\>\s*(?P<procedures>[a-z0-9_]+(\,[a-z0-9_]+)*)$', re.IGNORECASE)
        
        if self.__currentType is None:
            if Type.validTypeDeclaration(statement):
                self.__currentType = Type.fromDeclarationStatement(statement, moduleName, i)
                self.__currentType.setDeclaredIn(module)
        elif Variable.validVariableDeclaration(statement):
            members = Variable.fromDeclarationStatement(statement, moduleName, i)
            self.__currentType.addMembers(members)
        else:
            procedureRegExMatch = procedureRegEx.match(statement)
            if procedureRegExMatch is not None:
                alias = procedureRegExMatch.group('alias')
                if procedureRegExMatch.group('procedure') is not None:
                    procedure = procedureRegExMatch.group('procedure')
                else:
                    procedure = alias
                self.__currentType.addProcedure(alias, procedure)
            else:
                genericRegExMatch = genericRegEx.match(statement)
                if genericRegExMatch is not None:
                    alias = genericRegExMatch.group('alias')
                    procedures = genericRegExMatch.group('procedures').split(',')
                    self.__currentType.addProcedure(alias, procedures)
                else:
                    endTypeRegExMatch = endTypeRegEx.match(statement)
                    if endTypeRegExMatch is not None:
                        self.__collection.addType(self.__currentType)
                        self.__currentType = None

class TypeCollection(object):

    def __init__(self):
        self.__typeDict = dict()
        self.__typeSet = set()
        
    def __contains__(self, name):
        return name in self.__typeDict and len(self.__typeDict[name]) > 0
    
    def __iter__(self):
        return iter(self.__typeSet)
     
    def __len__(self):
        return len(self.__typeSet)
    
    def __getitem__(self, key):
        return self.getType(key)
        
    def addType(self, typE):
        assertType(typE, 'typE', Type)
        
        name = typE.getName().lower()
        if name not in self:
            self.__typeDict[name] = [typE]
        else:
            self.__typeDict[name].append(typE)
        self.__typeSet.add(typE)
        
    def finalize(self, abstractTypes):
        assertType(abstractTypes, 'abstractTypes', dict)
        self.__setMemberTypes()
        self.__setExtendedTypes()
        self.__assignImplementations(abstractTypes)

    def getType(self, typeName, usingModule = None):
        assertType(typeName, 'typeName', str)
        assertType(usingModule, 'usingModule', Module, True)
        
        typeName = typeName.lower()
        
        # Type imported with alias?
        if usingModule is not None:
            useAliases = usingModule.getUseAliases()
            for alias, (usedModuleName, original) in useAliases.items():
                if alias == typeName:
                    if original in self:
                        for typE in self.__typeDict[original]:
                            if typE.getModule().getName() == usedModuleName:
                                return typE
        # Else
        if typeName in self:
            types = self.__typeDict[typeName]
            if len(types) == 1:
                return types[0]
            elif usingModule is not None:
                # Declared in the same usingModule?
                for typE in types:
                    if typE.getModule().getName() == usingModule.getName():
                        return typE
                # Type imported?
                for use in usingModule.getUses():
                    if len(use) == 1 or use[1] == typeName: 
                        for typE in types:
                            if typE.getModule().getName() == use[0]:
                                return typE 
            #TODO Warning
            
        return None

    def getTypeOfVariable(self, var):
        assertType(var, 'var', Variable)

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
        for typE in self.__typeSet:
            extendsName = typE.getExtendsName()
            if extendsName:
                extendsType = self.getType(extendsName, typE.getModule())
                if extendsType is not None:
                    typE.setExtends(extendsType)
        self.__extends = dict()
        
    def __assignImplementations(self, abstractTypes):
        for abstractName, implementationName in abstractTypes.items():
            abstractType = self.getType(abstractName)
            if abstractType is None:
                printWarning('No such type: ' + abstractName, location = 'TypeCollection')
            elif not abstractType.isAbstract():
                printWarning('Type not abstract: ' + abstractName, location = 'TypeCollection')
            else:
                implementationType = self.getType(implementationName)
                if implementationType is None:
                    printWarning('No such type: ' + implementationName, location = 'TypeCollection')
                elif not implementationType.isSubtypeOf(abstractType):
                    printWarning('Type ' + implementationName + ' is not a subtyp of ' + abstractName, location = 'TypeCollection')
                else:
                    abstractType.assignImplementation(implementationType)
            