# coding=utf8

import os.path;
import re
from source import SubroutineFullName, InnerSubroutineName
from callgraph import CallGraph
from assertions import assertType, assertTypeAll
from supertypes import CallGraphBuilder
from printout import printWarning

class GNUx86AssemblerCallGraphBuilder(CallGraphBuilder):
    
    FILE_SUFFIX = '.s'

    def __init__(self, baseDirs, specialModuleFiles = {}):
        assertType(specialModuleFiles, 'specialModuleFiles', dict)
       
        if isinstance(baseDirs, str):
            baseDirs = [baseDirs]
        assertTypeAll(baseDirs, 'baseDirs', str)
        for baseDir in baseDirs:    
            if not os.path.isdir(baseDir):
                raise IOError("Not a directory: " + baseDir);
        
        self.__baseDirs = baseDirs;
        self.setSpecialModuleFiles(specialModuleFiles);
        
    def setSpecialModuleFiles(self, specialModuleFiles):
        assertType(specialModuleFiles, 'specialModuleFiles', dict)
        
        self.__specialModuleFiles = dict()
        for module, filE in specialModuleFiles.iteritems():
            self.__specialModuleFiles[module.lower()] = filE;
        
    def buildCallGraph(self, rootSubroutine, clear = False):  # @UnusedVariable
        assertType(rootSubroutine, 'rootSubroutine', SubroutineFullName)     
        ### clear only for compatibility ### 
        
        callGraph = CallGraph();
        self.__buildCallGraphRecursive(rootSubroutine, callGraph);
        return callGraph;
        
    def __buildCallGraphRecursive(self, rootSubroutine, callGraph, oldFilePath = None):
        if rootSubroutine in callGraph:
            return;
        else:      
            callGraph.addSubroutine(rootSubroutine);
            filePath = self.__getSubroutinesFilePath(rootSubroutine)
            if filePath is None:
                filePath = oldFilePath
            if filePath is None:
                printWarning('No Assembler file to start with for subroutine: ' + str(rootSubroutine), 'GNUx86AssemblerCallGraphBuilder')
                return;
            elif not os.path.isfile(filePath):
                filename = os.path.basename(filePath).lower()
                directory = os.path.dirname(filePath)
                foundFile = False
                for dirFile in os.listdir(directory):
                    if dirFile.lower() == filename:
                        filePath = os.path.join(directory, dirFile)
                        foundFile = True
                        break
                if not foundFile:
                    printWarning('Assembler file not found for subroutine: ' + str(rootSubroutine) + '. Expected: ' + filePath, 'GNUx86AssemblerCallGraphBuilder')
                return;
            else:  
                for callTriple in self.__findCalledSubroutines(rootSubroutine, filePath):
                    calledSubroutine = callTriple[0]
                    lineNumber = callTriple[1]
                    discriminator = callTriple[2]
                    callGraph.addCall(rootSubroutine, calledSubroutine, lineNumber, discriminator);
                    self.__buildCallGraphRecursive(calledSubroutine, callGraph, filePath);
        
    def __getSubroutinesFilePath(self, subroutine):
        moduleName = subroutine.getModuleName()
        if moduleName is None:
            return None 
        
        return self.getModuleFilePath(moduleName);
    
    def getModuleFilePath(self, moduleName):
        assertType(moduleName, 'moduleName', str)
            
        if moduleName in self.__specialModuleFiles:
            fileName = self.__specialModuleFiles[moduleName]
            fileName = fileName[:fileName.rfind('.')] + GNUx86AssemblerCallGraphBuilder.FILE_SUFFIX
        else:
            fileName = moduleName + GNUx86AssemblerCallGraphBuilder.FILE_SUFFIX;
        
        fileName = fileName.lower()
        for baseDir in self.__baseDirs:
            for root, _, files in os.walk(baseDir):
                for name in files:
                    if name.lower() == fileName:
                        return os.path.join(root, name)
            
        return None
    
    def __findCalledSubroutines(self, subroutine, filePath):
        openFile = open(filePath);
        calls = []
        inFunction = False 
        if isinstance(subroutine, InnerSubroutineName):
            hostForInnerSubroutines = subroutine.getHostName()
        else:
            hostForInnerSubroutines = subroutine
        lines = openFile.readlines()
        for i, line in enumerate(lines):
            line = line.strip();
             
            if line == str(subroutine) + ':':
                inFunction = True
            elif line == '.cfi_endproc':
                inFunction = False
             
            if inFunction:
                if line.startswith('call\t'):
                    callee = line.replace('call\t', '', 1)
                    atPos = callee.find('@')
                    if atPos >= 0:
                        callee = callee[:atPos] 
                    hashtagPos = callee.find('#')
                    if hashtagPos >= 0:
                        callee = callee[:(hashtagPos - 1)] 
                    callee = callee.strip()
                    
                    if SubroutineFullName.validFullName(callee):
                        calls.append((SubroutineFullName(callee),) + self.__findLineNumberAndDiscriminator(lines, i))
                    elif InnerSubroutineName.validInnerSubroutineName(callee):
                        calls.append((InnerSubroutineName(callee, hostForInnerSubroutines),) + self.__findLineNumberAndDiscriminator(lines, i))
                elif line.startswith('leaq\t') and line.find('._omp_fn.') >= 0:
                    ompRegion = line.replace('leaq\t', '', 1)
                    parathPos = ompRegion.find('(')
                    if parathPos >= 0:
                        ompRegion = ompRegion[:parathPos] 
                    calls += self.__findCalledSubroutines(ompRegion, filePath)
        openFile.close();            
        return calls             

    def __findLineNumberAndDiscriminator(self, lines, startLine):
        lineNumber = -1;
        discriminator = 0;
        regEx = re.compile(r'^\s*\.loc\s*\d+\s*(?P<linenumber>\d+)\s*\d+\s*(((basic_block)|(prologue_end)|(epilogue_begin)|(is_stmt)|(isa))\s*\d*\s*)*(discriminator\s*(?P<discriminator>\d+))?.*$');
        i = startLine
        for i in xrange(startLine, 0, -1):  # @UndefinedVariable
            regExMatch = regEx.match(lines[i]);
            if regExMatch is not None:
                lineNumber = (int) (regExMatch.group('linenumber'))
                if (regExMatch.group('discriminator') is not None):
                    discriminator = (int) (regExMatch.group('discriminator'))
                break
        
        return (lineNumber, discriminator)
    
class FromAssemblerCallGraphBuilder(GNUx86AssemblerCallGraphBuilder):
    '''DEPRECATED: exists only for compatiblity with older version'''
