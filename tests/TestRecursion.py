#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/recursion'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from assembler import FromAssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName, VariableReference
from trackvariable import TrackVariableCallGraphAnalysis
from usetraversal import UseTraversal

''' 
Tests whether recursive subroutines are tracked correctly
'''
class RecursionTest(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        callGraphBuilder = FromAssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        
        self.srcFile = SOURCE_DIR + '/recursion.f90'
        self.assFile = ASSEMBLER_DIR + '/recursion.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.direct = SubroutineFullName('__recursion_MOD_recurse')
        self.callGraphDirect = callGraphBuilder.buildCallGraph(self.direct)
        
        sys.tracebacklimit = 0
        
    def testAssemberFileExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)

    def testCallGraphs(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        simpleNames = set()
        for name in self.callGraphDirect.getAllSubroutineNames():
            simpleNames.add(name.getSimpleName())
        self.assertEqual({'recurse'}, simpleNames)
        
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        sourceFile = self.sourceFiles.findSourceFile('recursion.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('recursion')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))

        module = sourceFile.getModule('recursion')
        self.assertIsNotNone(module)
        
        simpleNames = set(module.getSubroutines().keys())
        self.assertEqual({'recurse'}, simpleNames)
                
    def testDirect(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.direct)
        tracker = TrackVariableCallGraphAnalysis(self.sourceFiles, [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
        
        expressions = set(map(VariableReference.getExpression, tracker.trackDerivedTypeArguments(self.callGraphDirect)))
        self.assertEqual({'var%counter'}, expressions)

    #TODO Functions
    #TODO indirekte Rekursion
    #TODO andere Argumentposition
        
if __name__ == "__main__":
    unittest.main()