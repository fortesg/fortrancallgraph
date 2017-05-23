#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/use'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from tree import TreeLikeCallGraphPrinter
from assembler import FromAssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName
from globals import GlobalVariablesCallGraphAnalysis
from usetraversal import UseTraversal
from typefinder import TypeFinder
from useprinter import UseCollector

class SampleTest(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {'next' : 'middle.f90'}
        callGraphBuilder = FromAssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        
        self.srcFile = SOURCE_DIR + '/top.f90'
        self.assFile = ASSEMBLER_DIR + '/top.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.root = SubroutineFullName('__top_MOD_tiptop')
        self.callGraph = callGraphBuilder.buildCallGraph(self.root)
        
        self.printer = TreeLikeCallGraphPrinter()
        self.globalsTracker = GlobalVariablesCallGraphAnalysis(self.sourceFiles)
        self.useTraversal = UseTraversal(self.sourceFiles)
        
    def testAssemberFileExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)

    def testCallGraphs(self):
        # TODO Auch mit serialisiertem CallGraph testen
        if not self.filesExist:
            self.skipTest('Files not there')
        
        simpleNames = set()
        for name in self.callGraph.getAllSubroutineNames():
            simpleNames.add(name.getSimpleName())
        self.assertEqual({'tiptop', 'medium', 'butt_x'}, simpleNames)
                
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        sourceFile = self.sourceFiles.findSourceFile('middle.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('middle')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(2, len(sourceFile.getModules()))

        module1 = sourceFile.getModule('middle')
        self.assertIsNotNone(module1)
        self.assertEqual(4, len(module1.getSubroutines()))
        
        simpleNames = set(module1.getSubroutines().keys())
        self.assertEqual({'medium', 'average_i', 'average_r', 'average_f'}, simpleNames)

        module2 = sourceFile.getModule('next')
        self.assertIsNotNone(module2)
        self.assertEqual(0, len(module2.getSubroutines()))
                
    def testUseCollector(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        useCollector = UseCollector()
        self.useTraversal.addPassenger(useCollector)
        self.useTraversal.parseModules(self.root)
        
        result = useCollector.getResult()
        self.assertIsNotNone(result)
        self.assertTrue(result)
        self.assertEqual({'top', 'middle', 'next', 'bottom'}, result)
                
    def testTypeFinder(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        typeFinder = TypeFinder()
        self.useTraversal.addPassenger(typeFinder)
        self.useTraversal.parseModules(self.root)
        
        result = typeFinder.getResult()
        self.assertIsNotNone(result)
        self.assertTrue(result)
        self.assertEqual(2, len(result))
        
        self.assertIn('kangaroo', result)
        kangaroo = result['kangaroo']
        self.assertIsNotNone(kangaroo)
        self.assertEqual('kangaroo', kangaroo.getName())
        self.assertIsNone(kangaroo.getExtends())
        self.assertEqual('next', kangaroo.getDeclaredIn())
        self.assertEqual(3, len(kangaroo.getMembers()))
        
        self.assertIn('foot', result)
        foot = result['foot']
        self.assertIsNotNone(foot)
        self.assertEqual('foot', foot.getName())
        self.assertIsNone(foot.getExtends())
        self.assertEqual('bottom', foot.getDeclaredIn())
        self.assertEqual(4, len(foot.getMembers()))
        
if __name__ == "__main__":
    unittest.main()