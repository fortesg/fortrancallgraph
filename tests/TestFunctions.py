#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/functions'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from assembler import FromAssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName, VariableReference
from trackvariable import VariableTracker
from usetraversal import UseTraversal

''' 
Tests whether type-bound procedures are handled correctly
'''
class FunctionsTest(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        
        self.srcFile = SOURCE_DIR + '/functions.f90'
        self.assFile = ASSEMBLER_DIR + '/functions.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.func1 = SubroutineFullName('__functions_MOD_func1')
        self.func2 = SubroutineFullName('__functions_MOD_func2')
        self.func3 = SubroutineFullName('__functions_MOD_func3')
        self.func4 = SubroutineFullName('__functions_MOD_func4')
        self.subr5 = SubroutineFullName('__functions_MOD_subr5')
        
        self.fileExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile) 
        
        
    def testFilesExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)

                
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        sourceFile = self.sourceFiles.findSourceFile('functions.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('functions')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))

        module = sourceFile.getModule('functions')
        self.assertIsNotNone(module)
        
        simpleNames = set(module.getSubroutines().keys())
        self.assertEqual({'func1', 'func2', 'func3', 'func4', 'subr5'}, simpleNames)


    def testFindFunctions(self):
        self.assertIsNotNone(self.sourceFiles.findSubroutine(self.func1))
        self.assertIsNotNone(self.sourceFiles.findSubroutine(self.func2))
        self.assertIsNotNone(self.sourceFiles.findSubroutine(self.func3))
        self.assertIsNotNone(self.sourceFiles.findSubroutine(self.func4))
        self.assertIsNotNone(self.sourceFiles.findSubroutine(self.subr5))


    def testIsFunction(self):
        self.assertTrue(self.sourceFiles.findSubroutine(self.func1).isFunction())
        self.assertTrue(self.sourceFiles.findSubroutine(self.func2).isFunction())
        self.assertTrue(self.sourceFiles.findSubroutine(self.func3).isFunction())
        self.assertTrue(self.sourceFiles.findSubroutine(self.func4).isFunction())
        self.assertFalse(self.sourceFiles.findSubroutine(self.subr5).isFunction())


    def testGetResultVar(self):
        
        self.assertIsNone(self.sourceFiles.findSubroutine(self.subr5).getResultVariable())

        func3 = self.sourceFiles.findSubroutine(self.func3)
        self.assertIsNotNone(func3.getResultVariable())
        self.assertEqual('func3', func3.getResultVariable().getName())
        self.assertEqual('INTEGER', func3.getResultVariable().getTypeName())

        func4 = self.sourceFiles.findSubroutine(self.func4)
        self.assertIsNotNone(func4.getResultVariable())
        self.assertEqual('r4', func4.getResultVariable().getName())
        self.assertEqual('INTEGER', func4.getResultVariable().getTypeName())
        
        func1 = self.sourceFiles.findSubroutine(self.func1)
        self.assertIsNotNone(func1.getResultVariable())
        self.assertEqual('func1', func1.getResultVariable().getName())
        self.assertEqual('INTEGER', func1.getResultVariable().getTypeName())

        func2 = self.sourceFiles.findSubroutine(self.func2)
        self.assertIsNotNone(func2.getResultVariable())
        self.assertEqual('r2', func2.getResultVariable().getName())
        self.assertEqual('INTEGER', func2.getResultVariable().getTypeName())

        
if __name__ == "__main__":
    unittest.main()