# coding=utf8

import sys;
import os.path;
import re
from source import SubroutineFullName, InnerSubroutineName
from callgraph import CallGraph
from utils import assertType
from supertypes import CallGraphBuilder

class FromAssemblerCallGraphBuilder(CallGraphBuilder):
    
    FILE_SUFFIX = '.s'

    def __init__(self, baseFolder, specialModuleFiles = {}):
        assertType(specialModuleFiles, 'specialModuleFiles', dict)
        if not os.path.isdir(baseFolder):
            raise IOError("Not a directory: " + baseFolder);
        
        self.__baseFolder = baseFolder;
        self.setSpecialModuleFiles(specialModuleFiles);
        
    def setSpecialModuleFiles(self, specialModuleFiles):
        assertType(specialModuleFiles, 'specialModuleFiles', dict)
        
        self.__specialModuleFiles = dict()
        for module, file in specialModuleFiles.iteritems():
            self.__specialModuleFiles[module.lower()] = file;
        
    def buildCallGraph(self, rootSubroutine, clear = False):
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
                print  >> sys.stderr, '*** WARNING [FromAssemblerCallGraphBuilder]: No Assembler file to start with for subroutine: ' + str(rootSubroutine) + ' ***';
                return;
            elif not os.path.isfile(filePath):
                print  >> sys.stderr, '*** WARNING [FromAssemblerCallGraphBuilder]: Assembler file not found for subroutine: ' + str(rootSubroutine) + '. Expected: ' + filePath + ' ***';
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
            path = self.__specialModuleFiles[moduleName]
            path = path[:path.rfind('.')] + FromAssemblerCallGraphBuilder.FILE_SUFFIX
        else:
            path = moduleName + FromAssemblerCallGraphBuilder.FILE_SUFFIX;
        
        return os.path.join(self.__baseFolder, path);
    
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
             
            if inFunction and line.startswith('call\t'):
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
