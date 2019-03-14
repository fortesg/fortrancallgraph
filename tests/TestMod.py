#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/mod'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from tree import TreeLikeCallGraphPrinter
from assembler import GNUx86AssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName
from globals import GlobalVariableTracker
from trackvariable import VariableTrackerSettings

''' 
Tests wether TestNested.pysubroutine names are correctly handled when module names end on "_mod" 
'''
class SampleTest(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        callGraphBuilder = GNUx86AssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        
        self.srcFile = SOURCE_DIR + '/RHS_mod.f90'
        self.assFile = ASSEMBLER_DIR + '/RHS_mod.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.func = SubroutineFullName('__RHS_mod_MOD_func')
        self.callGraph = callGraphBuilder.buildCallGraph(self.func)
        
        self.printer = TreeLikeCallGraphPrinter()
        self.globalsTracker = GlobalVariableTracker(self.sourceFiles, VariableTrackerSettings())

    def testModuleName(self):
        self.assertEqual('rhs_mod', self.func.getModuleName())
        self.assertEqual('func', self.func.getSimpleName())
        
    def testAssemberFileExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)

    def testCallGraphs(self):
        # TODO Auch mit serialisiertem CallGraph testen
        if not self.filesExist:
            self.skipTest('Files not there')
        
        simpleNames = []
        for name in self.callGraph.getAllSubroutineNames():
            simpleNames.append(name.getSimpleName())
        self.assertEqual(['func'], simpleNames)
                
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        sourceFile = self.sourceFiles.findSourceFile('RHS_mod.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('RHS_mod')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))

        module = sourceFile.getModule('RHS_mod')
        self.assertIsNotNone(module)
        self.assertEqual(1, len(module.getSubroutines()))
        
        simpleNames = set(module.getSubroutines().keys())
        self.assertEqual({'func'}, simpleNames)
        
if __name__ == "__main__":
    unittest.main()