# coding=utf8
from supertypes import UsePrinters, UseTraversalPassenger
from utils import assertTypeAll, assertType
from source import SourceFiles, SubroutineFullName
from usetraversal import UseTraversal

class UseCollector(UseTraversalPassenger):
 
    def __init__(self):
        self.reset()
         
    def reset(self):      
        self.__moduleNames = set()
         
    def getResult(self):
        return self.__moduleNames
 
    def parseStatement(self, i, statement, j, moduleName):
        assertType(moduleName, 'moduleName', str)
         
        self.__moduleNames.add(moduleName)
        

class UsedModuleNamePrinter(UsePrinters):
    
    def __init__(self, sourceFiles, excludeModules = []):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertTypeAll(excludeModules, 'excludeModules', str)
        
        super(UsedModuleNamePrinter, self).__init__(sourceFiles, excludeModules)
    
    def printUses(self, rootSubroutine):
        assertType(rootSubroutine, 'rootSubroutine', SubroutineFullName)
         
        useTraversal = UseTraversal(self._sourceFiles, self._excludeModules)
        useCollector = UseCollector()
        useTraversal.addPassenger(useCollector)
        useTraversal.parseModules(rootSubroutine)
         
        modules = useCollector.getResult()
        modules.add(rootSubroutine.getModuleName())
        self._printModules(modules)
        
    def _printModules(self, modules):
        print '\n'.join(sorted(modules))
        

class UsedFileNamePrinter(UsedModuleNamePrinter):
    
    def __init__(self, sourceFiles, excludeModules = []):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertTypeAll(excludeModules, 'excludeModules', str)
        
        super(UsedFileNamePrinter, self).__init__(sourceFiles, excludeModules)
    
    def _printModules(self, modules):
        
        files = set()
        for moduleName in modules:
            sourceFile = self._sourceFiles.findModuleFile(moduleName)
            if sourceFile is not None:
                path = self._sourceFiles.getRelativePath(sourceFile)
                files.add(path)
        
        print '\n'.join(sorted(files))