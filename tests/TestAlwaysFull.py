#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/alwaysfull'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from assembler import GNUx86AssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName
from trackvariable import VariableTracker, VariableTrackerSettings
from usetraversal import UseTraversal

''' 
Tests whether always full types are handled correctly
'''
class TypeAlwaysFull(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        self.callGraphBuilder = GNUx86AssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles)
        self.trackerSettings = VariableTrackerSettings()
        self.trackerSettings.fullTypes = ['b']
        self.ignoreRegex = 'ignore'
        
        self.srcFile = SOURCE_DIR + '/alwaysfull.f90'
        self.assFile = ASSEMBLER_DIR + '/alwaysfull.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.test = SubroutineFullName('__alwaysfull_MOD_test')
        self.callGraphTest = self.callGraphBuilder.buildCallGraph(self.test)
        self.useTraversal = UseTraversal(self.sourceFiles)
        self.useTraversal.parseModules(self.test)
        
    def testAssemberfilesExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)
                 
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        sourceFile = self.sourceFiles.findSourceFile('alwaysfull.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('alwaysfull')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))
 
        module = sourceFile.getModule('alwaysfull')
        self.assertIsNotNone(module)
         
        simpleNames = set(module.getSubroutines().keys())
        self.assertEqual({'test', 'ignore'}, simpleNames)
 
    def testCallGraphs(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        self.assertEqual({'test', 'ignore'}, set(subroutineFullName.getSimpleName() for subroutineFullName in self.callGraphTest.getAllSubroutineNames()))
         
    def testUseTraversal(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        self.assertEqual(0, len(self.useTraversal.getInterfaces()))
        self.assertEqual(4, len(self.useTraversal.getTypes()))
         
    def testAlwaysFull(self):
        if not self.filesExist:
            self.skipTest('Files not there')
          
        tracker = VariableTracker(self.sourceFiles, self.trackerSettings, self.useTraversal.getInterfaces(), self.useTraversal.getTypes(), callGraphBuilder = self.callGraphBuilder)
        tracker.setIgnoreRegex(self.ignoreRegex)
          
        expressions = set(ref.getExpression() for ref in tracker.trackDerivedTypeArguments(self.callGraphTest))
        self.assertEqual({'argb%a%one', 'argb%a%two', 'argb%three', 'argd%b%a%one', 'argd%b%a%two', 'argd%b%three'}, expressions)
        
if __name__ == "__main__":
    unittest.main()