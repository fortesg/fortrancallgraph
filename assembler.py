# coding=utf8

import sys;
import os.path;
import re
from source import SubroutineFullName, InnerSubroutineName
from callgraph import CallGraph
from utils import assertType, assertTypeAll
from supertypes import CallGraphBuilder

class FromAssemblerCallGraphBuilder(CallGraphBuilder):
    
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
                filename = os.path.basename(filePath).lower()
                directory = os.path.dirname(filePath)
                foundFile = False
                for dirFile in os.listdir(directory):
                    if dirFile.lower() == filename:
                        filePath = os.path.join(directory, dirFile)
                        foundFile = True
                        break
                if not foundFile:
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
        
        for baseDir in self.__baseDirs:
            fullPath = os.path.join(baseDir, path);
            if os.path.isfile(fullPath):
                return fullPath
            
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
