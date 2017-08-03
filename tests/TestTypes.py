#!/usr/bin/python

import unittest
import os
import sys
TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/types'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from source import SourceFiles, SubroutineFullName
from usetraversal import UseTraversal


class TestTypes(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        
        self.srcFile = SOURCE_DIR + '/types.f90'
        self.fileExist = os.path.exists(self.srcFile)
        self.root = SubroutineFullName('__types_MOD_test')
        self.usetraversal = UseTraversal(self.sourceFiles)
        
    def testFileExists(self):
        self.assertTrue(self.fileExist, 'Test will fail. Source file not found: ' + self.srcFile)

    def testSourceFiles(self):
        if not self.fileExist:
            self.skipTest('File not there')
         
        sourceFile = self.sourceFiles.findSourceFile('types.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('types')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))
 
        module = sourceFile.getModule('types')
        self.assertIsNotNone(module)
        self.assertEqual(1, len(module.getSubroutines()))
        self.assertEqual(4, len(module.getVariables()))
         
        simpleNames = set((module.getVariables().keys()))
        self.assertEqual({'var0', 'var1', 'var2', 'var3'}, simpleNames)
            
    def testUseTraversal(self):
        if not self.fileExist:
            self.skipTest('Files not there')
        
        self.usetraversal.parseModules(self.root)
        self.assertEqual(0, len(self.usetraversal.getInterfaces()))
        self.assertEqual(10, len(self.usetraversal.getTypes()))
    
        
if __name__ == "__main__":
    unittest.main()