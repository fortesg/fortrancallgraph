#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/linenumbers'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from source import SourceFiles, SubroutineFullName, InnerSubroutineName

class TestLineNumbers(unittest.TestCase):
    def setUp(self):
        self.sourceFiles = SourceFiles(SOURCE_DIR, {}, True)
        
        self.srcFile = SOURCE_DIR + '/dyn_comp.f90'
        self.filesExist = os.path.exists(self.srcFile)
        
        self.dynRun = SubroutineFullName('__dyn_comp_MOD_dyn_run')
        self.dynReadNl = SubroutineFullName('__dyn_comp_MOD_dyn_readnl')
        self.initNsplit = InnerSubroutineName('init_nsplit', self.dynReadNl)
        self.dynRegister = SubroutineFullName('__dyn_comp_MOD_dyn_register')
        
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        sourceFile = self.sourceFiles.findSourceFile('dyn_comp.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('dyn_comp')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))
 
        module = sourceFile.getModule('dyn_comp')
        self.assertIsNotNone(module)
         
        dynReadNl = sourceFile.getSubroutine(self.dynReadNl)
        self.assertIsNotNone(dynReadNl)
        initNsplit = dynReadNl.getSubroutine(self.initNsplit)
        self.assertIsNotNone(initNsplit)
        dynRegister = sourceFile.getSubroutine(self.dynRegister)
        self.assertIsNotNone(dynRegister)
        dynRun = sourceFile.getSubroutine(self.dynRun)
        self.assertIsNotNone(dynRun)
        
    def testModuleLineNumbers(self):
        module = self.sourceFiles.findModule('dyn_comp')
        self.assertEqual(5, module.getDeclarationLineNumber())
        self.assertEqual(77, module.getLastUseLineNumber())
        self.assertEqual(151, module.getContainsLineNumber())
        self.assertEqual(148, module.getLastSpecificationLineNumber())
        self.assertEqual(3671, module.getLastLineNumber())
        
    def testDynReadNlLineNumbers(self):
        dynReadNl = self.sourceFiles.findSubroutine(self.dynReadNl)
        self.assertEqual(154, dynReadNl.getDeclarationLineNumber())
        self.assertEqual(162, dynReadNl.getLastUseLineNumber())
        self.assertEqual(370, dynReadNl.getContainsLineNumber())
        self.assertEqual(204, dynReadNl.getLastSpecificationLineNumber())
        self.assertEqual(415, dynReadNl.getLastLineNumber())
        
    def testInitSplitLineNumbers(self):
        initNsplit = self.sourceFiles.findSubroutine(self.initNsplit)
        self.assertEqual(373, initNsplit.getDeclarationLineNumber())
        self.assertEqual(373, initNsplit.getLastUseLineNumber())
        self.assertEqual(-1, initNsplit.getContainsLineNumber())
        self.assertEqual(402, initNsplit.getLastSpecificationLineNumber())
        self.assertEqual(412, initNsplit.getLastLineNumber())
        
    def testDynRegisterLineNumbers(self):
        dynRegister = self.sourceFiles.findSubroutine(self.dynRegister)
        self.assertEqual(419, dynRegister.getDeclarationLineNumber())
        self.assertEqual(424, dynRegister.getLastUseLineNumber())
        self.assertEqual(-1, dynRegister.getContainsLineNumber())
        self.assertEqual(424, dynRegister.getLastSpecificationLineNumber())
        self.assertEqual(441, dynRegister.getLastLineNumber())
        
    def testDynRunLineNumbers(self):
        dynRun = self.sourceFiles.findSubroutine(self.dynRun)
        self.assertEqual(676, dynRun.getDeclarationLineNumber())
        self.assertEqual(806, dynRun.getLastUseLineNumber())
        self.assertEqual(-1, dynRun.getContainsLineNumber())
        self.assertEqual(1642, dynRun.getLastSpecificationLineNumber())
        self.assertEqual(3225, dynRun.getLastLineNumber())
        

        
if __name__ == "__main__":
    unittest.main()