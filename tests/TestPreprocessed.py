#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/preprocessed/build'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from assembler import GNUx86AssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName
from trackvariable import VariableTracker, VariableTrackerSettings
from usetraversal import UseTraversal

''' 
Tests whether type-bound procedures are handled correctly
'''
class TypeProcedureTest(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        callGraphBuilder = GNUx86AssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles, True);
        
        self.srcFile = SOURCE_DIR + '/preprocessed.f90'
        self.assFile = ASSEMBLER_DIR + '/preprocessed.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.caller = SubroutineFullName('__preprocessed_MOD_caller')
        self.callGraph = callGraphBuilder.buildCallGraph(self.caller)
        self.useTraversal = UseTraversal(self.sourceFiles)
        self.useTraversal.parseModules(self.caller)
        
        self.fileExist = os.path.exists(self.srcFile)
        
    def testAssemberFileExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)
                
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        sourceFile = self.sourceFiles.findSourceFile('preprocessed.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('preprocessed')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))

        module = sourceFile.getModule('preprocessed')
        self.assertIsNotNone(module)
        
        simpleNames = set(module.getSubroutines().keys())
        self.assertEqual({'caller', 'test_0d', 'test_1d', 'test_2d'}, simpleNames)

    def testCallGraphs(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        self.assertEqual({'caller', 'test_0d', 'test_1d', 'test_2d'}, set(subroutineFullName.getSimpleName() for subroutineFullName in self.callGraph.getAllSubroutineNames()))
        
    def testUseTraversal(self):
        if not self.fileExist:
            self.skipTest('Files not there')
        
        self.assertEqual(1, len(self.useTraversal.getInterfaces()))
        self.assertEqual(1, len(self.useTraversal.getTypes()))
    
    def testOffset(self):
        sourceFile = self.sourceFiles.findSourceFile('preprocessed.f90')
        self.assertEqual(11, sourceFile.getPreprocessorOffset(47))
    
    def testTracker(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        tracker = VariableTracker(self.sourceFiles, VariableTrackerSettings(), self.useTraversal.getInterfaces(), self.useTraversal.getTypes())
        
        expressions = set(ref.getExpression() for ref in tracker.trackDerivedTypeArguments(self.callGraph))
        self.assertEqual({'t0%i0', 't1%i1', 't2%i2'}, expressions)


if __name__ == "__main__":
    unittest.main()