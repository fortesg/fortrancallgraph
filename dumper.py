from assertions import assertType
from supertypes import SourceDumper
from source import SubroutineFullName
from printout import printInline, printLine

class SourceLineDumper(SourceDumper):

    def dumpSubroutine(self, subroutineName):
        assertType(subroutineName, 'subroutineName', SubroutineFullName)

        subroutine = self._findSubroutine(subroutineName);
        if subroutine is not None:
            numberLength = len(str(subroutine.getLastLineNumber()));
            for i, line in subroutine.getLines():
                if (self._printLineNumbers):
                    printInline(str(i).zfill(numberLength) + '  ')
                printInline(line)

    def dumpModule(self, moduleName):
        assertType(moduleName, 'moduleName', str)

        module = self._findModule(moduleName);
        if module is not None:
            numberLength = len(str(module.getLastLineNumber()));
            for i, line in module.getLines():
                if (self._printLineNumbers):
                    printInline(str(i).zfill(numberLength) + '  ')
                printInline(line)

    def dumpSourceFile(self, fileName):
        assertType(fileName, 'fileName', str)

        sourceFile = self._findSourceFile(fileName);
        if sourceFile is not None:
            lines = sourceFile.getLines();
            numberLength = len(str(len(lines)));
            for i, line in lines:
                if (self._printLineNumbers):
                    printInline(str(i).zfill(numberLength) + '  ')
                printInline(line)

class SourceStatementDumper(SourceDumper):

    def dumpSubroutine(self, subroutineName):
        assertType(subroutineName, 'subroutineName', SubroutineFullName)

        subroutine = self._findSubroutine(subroutineName);
        if subroutine is not None:
            numberLength = len(str(subroutine.getLastLineNumber()));
            for i, statement, _ in subroutine.getStatements():
                if (self._printLineNumbers):
                    printInline(str(i).zfill(numberLength) + '  ')
                printLine(statement)

    def dumpModule(self, moduleName):
        assertType(moduleName, 'moduleName', str)

        module = self._findModule(moduleName);
        if module is not None:
            numberLength = len(str(module.getLastLineNumber()));
            for i, statement, _ in module.getStatements():
                if (self._printLineNumbers):
                    printInline(str(i).zfill(numberLength) + '  ')
                printLine(statement)

    def dumpSourceFile(self, fileName):
        assertType(fileName, 'fileName', str)

        sourceFile = self._findSourceFile(fileName);
        if sourceFile is not None:
            statements = sourceFile.getStatements();
            numberLength = len(str(len(statements)));
            for i, statement, _ in statements:
                if (self._printLineNumbers):
                    printInline(str(i).zfill(numberLength) + '  ')
                printLine(statement)
