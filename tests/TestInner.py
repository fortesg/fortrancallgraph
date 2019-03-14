#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/inner'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from tree import TreeLikeCallGraphPrinter
from assembler import GNUx86AssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName, InnerSubroutineName
from trackvariable import VariableTrackerSettings
from globals import GlobalVariableTracker

class TestInner(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        callGraphBuilder = GNUx86AssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles)
        self.trackerSettings = VariableTrackerSettings()
        
        self.srcFile = SOURCE_DIR + '/inner.f90'
        self.assFile = ASSEMBLER_DIR + '/inner.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.s0 = SubroutineFullName('__inner_MOD_s0')
        self.s0CallGraph = callGraphBuilder.buildCallGraph(self.s0)
        
        self.sa = SubroutineFullName('__inner_MOD_sa')
        self.saCallGraph = callGraphBuilder.buildCallGraph(self.sa)

        self.sb = SubroutineFullName('__inner_MOD_sb')
        self.sbCallGraph = callGraphBuilder.buildCallGraph(self.sb)

        self.i1 = SubroutineFullName('__inner_MOD_i1')
        self.i1CallGraph = callGraphBuilder.buildCallGraph(self.i1)
        
        self.i1a = InnerSubroutineName('i1.3528', self.sa)
        self.i1b = InnerSubroutineName('i1.3518', self.sb)
        self.i2a = InnerSubroutineName('i2.3526', self.sa)
        self.i2b = InnerSubroutineName('i2.3516', self.sb)
        self.ia = InnerSubroutineName('ia.3526', self.sa)
        self.ib = InnerSubroutineName('ib.3514', self.sb)
        
        self.printer = TreeLikeCallGraphPrinter()
        self.globalsTracker = GlobalVariableTracker(self.sourceFiles, VariableTrackerSettings())
        
    def testAssemberFileExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)

    def testCallGraphs(self):
        # TODO Auch mit serialisiertem CallGraph testen
        if not self.filesExist:
            self.skipTest('Files not there')
        
        saSimpleNames = []
        for name in self.saCallGraph.getAllSubroutineNames():
            saSimpleNames.append(name.getSimpleName())
        saSimpleNames.sort()
        self.assertEqual(['i1', 'i2', 'ia', 'sa'], saSimpleNames)
        
        sbSimpleNames = []
        for name in self.sbCallGraph.getAllSubroutineNames():
            sbSimpleNames.append(name.getSimpleName())
        sbSimpleNames.sort()
        self.assertEqual(['i1', 'i2', 'ib', 'sb'], sbSimpleNames)
        for nameA in self.saCallGraph.getAllSubroutineNames():
            for nameB in self.sbCallGraph.getAllSubroutineNames():
                self.assertNotEqual(nameA, nameB)
                self.assertFalse(nameA == nameB)
                self.assertFalse(nameB == nameA)
                self.assertTrue(nameA != nameB)
                self.assertTrue(nameB != nameA)
                
        s0SimpleNames = []
        for name in self.s0CallGraph.getAllSubroutineNames():
            s0SimpleNames.append(name.getSimpleName())
        s0SimpleNames.sort()
        self.assertEqual(['i1', 'i1', 'i1', 'i2', 'i2', 'ia', 'ib', 's0', 'sa', 'sb'], s0SimpleNames)
            
        self.assertEqual(self.i1, self.s0CallGraph.findCalleeBySimpleName('i1', self.s0))
        self.assertEqual(self.i1a, self.s0CallGraph.findCalleeBySimpleName('i1', self.sa))
        self.assertEqual(self.i1b, self.s0CallGraph.findCalleeBySimpleName('i1', self.sb))
        self.assertEqual(self.i2a, self.s0CallGraph.findCalleeBySimpleName('i2', self.i1a))
        self.assertEqual(self.i2b, self.s0CallGraph.findCalleeBySimpleName('i2', self.i1b))
                
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        sourceFile = self.sourceFiles.findSourceFile('inner.f90')
        moduleFile = self.sourceFiles.findModuleFile('inner')
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))

        module = sourceFile.getModule('inner')
        self.assertIsNotNone(module)
        self.assertEqual(4, len(module.getSubroutines()))
        
        self.assertEqual({'i1', 's0', 'sa', 'sb'}, set(module.getSubroutines().keys()))
        
        self.assertEqual(11, self.sourceFiles.findSubroutine(self.s0).getDeclarationLineNumber())
        self.assertEqual(20, self.sourceFiles.findSubroutine(self.sa).getDeclarationLineNumber())
        self.assertEqual(29, self.sourceFiles.findSubroutine(self.i1a).getDeclarationLineNumber())
        self.assertEqual(37, self.sourceFiles.findSubroutine(self.i2a).getDeclarationLineNumber())
        self.assertEqual(45, self.sourceFiles.findSubroutine(self.ia).getDeclarationLineNumber())
        self.assertEqual(55, self.sourceFiles.findSubroutine(self.sb).getDeclarationLineNumber())
        self.assertEqual(64, self.sourceFiles.findSubroutine(self.i1b).getDeclarationLineNumber())
        self.assertEqual(72, self.sourceFiles.findSubroutine(self.i2b).getDeclarationLineNumber())
        self.assertEqual(80, self.sourceFiles.findSubroutine(self.ib).getDeclarationLineNumber())
        self.assertEqual(89, self.sourceFiles.findSubroutine(self.i1).getDeclarationLineNumber())
        
    def testGlobals(self):
        self.globalsTracker.trackGlobalVariables(self.s0CallGraph)
        
if __name__ == "__main__":
    unittest.main()