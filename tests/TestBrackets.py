#!/usr/bin/python

import unittest
import os
import sys
TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/brackets'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from tree import TreeLikeCallGraphPrinter
from assembler import FromAssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName, Type
from globals import GlobalVariablesCallGraphAnalysis
from trackvariable import TrackVariableCallGraphAnalysis
from usetraversal import UseTraversal


class TestBrackets(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        callGraphBuilder = FromAssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        
        self.srcFile = SOURCE_DIR + '/brackets.f90'
        self.assFile = ASSEMBLER_DIR + '/brackets.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.root = SubroutineFullName('__brackets_MOD_dummy')
        
        self.usetraversal = UseTraversal(self.sourceFiles)
        
    def testAssemberFileExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)

    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        sourceFile = self.sourceFiles.findSourceFile('brackets.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('brackets')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))
 
        module = sourceFile.getModule('brackets')
        self.assertIsNotNone(module)
        self.assertEqual(1, len(module.getSubroutines()))
        self.assertEqual(10, len(module.getVariables()))
            
    def testUseTraversal(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        self.usetraversal.parseModules(self.root)
        self.assertEqual(0, len(self.usetraversal.getInterfaces()))
        
        types = set(map(Type.getName, self.usetraversal.getTypes()))
        self.assertEqual({'t_cdiparam'}, types)
                
if __name__ == "__main__":
    unittest.main()