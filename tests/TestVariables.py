#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/variables'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from source import SourceFiles, SubroutineFullName

''' 
Tests for strange variable declarations
'''
class FunctionsTest(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        
        self.srcFile = SOURCE_DIR + '/variables.f90'
        self.assFile = ASSEMBLER_DIR + '/variables.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.grb2 = SubroutineFullName('__variables_MOD_grb2')
        
        self.fileExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile) 
        
        
    def testFilesExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)

                
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        sourceFile = self.sourceFiles.findSourceFile('variables.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('variables')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))

        module = sourceFile.getModule('variables')
        self.assertIsNotNone(module)
        
        simpleNames = set(module.getSubroutines().keys())
        self.assertEqual({'grb2'}, simpleNames)

    def testGrb2Variables(self):
        
        subroutine = self.sourceFiles.findSubroutine(self.grb2)
        variables = subroutine.getVariables()
        self.assertEqual(2, len(variables))

if __name__ == "__main__":
    unittest.main()