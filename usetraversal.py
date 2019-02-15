# coding=utf8

import re
from assertions import assertType, assertTypeAll
from source import SubroutineFullName, SourceFiles
from interfaces import InterfaceFinder
from typefinder import TypeFinder
from supertypes import UseTraversalPassenger
from printout import printWarning

class UseTraversal(object):
    
    __moduleWarnings = set()

    def __init__(self, sourceFiles, excludeModules = [], abstractTypes = {}):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertTypeAll(excludeModules, 'excludeModules', str)
        assertType(abstractTypes, 'abstractTypes', dict)
        
        self.__sourceFiles = sourceFiles;
        self.__excludeModules = [m.lower() for m in excludeModules]
        self.__visitedModules = set()
        self.__interfaceFinder = InterfaceFinder()
        self.__typeFinder = TypeFinder(abstractTypes)
        self.__passengers = [self.__interfaceFinder, self.__typeFinder]
    
    def addPassenger(self, passenger):
        assertType(passenger, 'passenger', UseTraversalPassenger)
        
        self.__passengers.append(passenger)

    def parseModules(self, rootSubroutine):      
        assertType(rootSubroutine, 'rootSubroutine', SubroutineFullName)
        
        self.__reset()
        self.__parseModulesRecursive(rootSubroutine.getModuleName())
        
    def __parseModulesRecursive(self, moduleName, parent = ''):
        useRegEx = re.compile(r'^USE[\s\:]+(?P<modulename>[a-z0-9_]+)\s*(\,.*)?$', re.IGNORECASE);
        usedModules = set()
        
        if moduleName in self.__visitedModules:
            return

        self.__visitedModules.add(moduleName)
        module = self.__sourceFiles.findModule(moduleName)
        if module is not None:
            for i, statement, j in module.getStatements():
                useRegExMatch = useRegEx.match(statement)
                if useRegExMatch is not None:
                    usedModuleName = useRegExMatch.group('modulename')
                    if usedModuleName.lower() not in self.__excludeModules: 
                        usedModules.add(usedModuleName)
                else:
                    self.__parseStatement(i, statement, j, module)
        elif moduleName not in UseTraversal.__moduleWarnings:
            UseTraversal.__moduleWarnings.add(moduleName)
            warning = 'Source file not found for module: ' + moduleName
            if parent:
                warning += ' (in: ' + parent + ')'
            printWarning(warning, 'UseTraversal')

        for usedModule in usedModules:
            self.__parseModulesRecursive(usedModule, moduleName)
            
    def __parseStatement(self, i, statement, j, moduleName):
        for passenger in self.__passengers:
            passenger.parseStatement(i, statement, j, moduleName)
            
    def getInterfaces(self):
        return self.__interfaceFinder.getResult()
            
    def getTypes(self):
        return self.__typeFinder.getResult()
        
    def __reset(self):
        self.__visitedModules = set()
        for passenger in self.__passengers:
            passenger.reset()
        