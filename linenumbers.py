#coding=utf8

from utils import assertType
from supertypes import LineNumberFinder
from source import SourceFiles, Subroutine

class DeclarationLineNumberFinder(LineNumberFinder):
    
    def _findLineNumber(self, subroutine):
        assertType(subroutine, 'subroutine', Subroutine) 
        
        return subroutine.getDeclarationLineNumber()

class EndStatementLineNumberFinder(LineNumberFinder):
    
    def _findLineNumber(self, subroutine):
        assertType(subroutine, 'subroutine', Subroutine) 
        
        return subroutine.getLastLineNumber()

    
class FirstDocumentationLineFinder(LineNumberFinder):
    
    def _findLineNumber(self, subroutine):
        assertType(subroutine, 'subroutine', Subroutine) 
        
        return subroutine.getFirstLineNumber()
    
class LastSpecificationLineFinder(LineNumberFinder):

    def _findLineNumber(self, subroutine):
        assertType(subroutine, 'subroutine', Subroutine)
        
        return subroutine.getLastSpecificationLineNumber()
    
class ContainsLineFinder(LineNumberFinder):

    def _findLineNumber(self, container):
        assertType(container, 'container', Subroutine)
        
        return container.getContainsLineNumber()
    
class LastUseLineFinder(LineNumberFinder):

    def _findLineNumber(self, container):
        assertType(container, 'container', Subroutine)
        
        return container.getLastUseLineNumber()
    
    
class AllLineFinder(LineNumberFinder):
    
    def __init__(self, sourceFiles):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        
        super(AllLineFinder, self).__init__(sourceFiles)
        
        self.__lineNumberFinders = {
            'First line (incl. documentation)': FirstDocumentationLineFinder(sourceFiles),
            'Declaration line': DeclarationLineNumberFinder(sourceFiles),
            'Last USE statement line': LastUseLineFinder(sourceFiles),
            'Last line of specification part': LastSpecificationLineFinder(sourceFiles),
            'CONTAINS statement line': ContainsLineFinder(sourceFiles),
            'Last line (end statement)': EndStatementLineNumberFinder(sourceFiles)
        } 
        
    def setMinimalOutput(self, enabled):   
        assertType(enabled, 'enabled', bool)
        
        super(AllLineFinder, self).setMinimalOutput(enabled)
        
        for lineNumberFinder in self.__lineNumberFinders.values():
            lineNumberFinder.setMinimalOutput(enabled)

    def _printLineNumber(self, subroutine):
        assertType(subroutine, 'subroutine', Subroutine)
        
        for description, lineNumberFinder in self.__lineNumberFinders.items():
            if not self._minimalOutput:
                print description + ': '
            lineNumberFinder._printLineNumber(subroutine)
