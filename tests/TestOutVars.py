#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/outvars'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from assembler import FromAssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName, VariableReference
from usetraversal import UseTraversal
from globals import GlobalVariableTracker

''' 
Tests whether assignments of function results are tracked correctly
'''
class OutVarsTest(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        callGraphBuilder = FromAssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        
        self.srcFile = SOURCE_DIR + '/outvars.f90'
        self.assFile = ASSEMBLER_DIR + '/outvars.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.testfunc = SubroutineFullName('__outvars_MOD_testFunc')
        self.callGraphTestFunc = callGraphBuilder.buildCallGraph(self.testfunc)
        
    def testAssemberFileExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)

    def testCallGraphs(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        self.assertEqual({'testfunc', 'get'}, set(map(SubroutineFullName.getSimpleName, self.callGraphTestFunc.getAllSubroutineNames())))
                
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        sourceFile = self.sourceFiles.findSourceFile('outvars.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('outvars')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))

        module = sourceFile.getModule('outvars')
        self.assertIsNotNone(module)
        
        simpleNames = set(module.getSubroutines().keys())
        self.assertEqual({'get', 'testfunc'}, simpleNames)
                 
    def testDirect(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.testfunc)
        tracker = GlobalVariableTracker(self.sourceFiles, [], [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
         
        globalVars = set([ref.getExpression() for ref in tracker.trackGlobalVariables(self.callGraphTestFunc)])
        self.assertEqual({'t1%first'}, globalVars)
        
if __name__ == "__main__":
    unittest.main()