#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/openmp'
ASSEMBLER_DIR = SOURCE_DIR
ASSEMBLER_DIR_MOD = TEST_DIR + '/samples/openmp-mod-s'

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from assembler import GNUx86AssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName
from usetraversal import UseTraversal
from trackvariable import VariableTracker, VariableTrackerSettings
from globals import GlobalVariableTracker

''' 
Tests whether assignments of function results are tracked correctly
'''
class OpenMPTest(unittest.TestCase):
    def setUp(self):
        self.specialModuleFiles = {}
        callGraphBuilder = GNUx86AssemblerCallGraphBuilder(ASSEMBLER_DIR, self.specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, self.specialModuleFiles)
        self.trackerSettings = VariableTrackerSettings()
        self.trackerSettings.excludeModules = ['omp_lib']
        
        self.srcFile = SOURCE_DIR + '/openmp.f90'
        self.assFile = ASSEMBLER_DIR + '/openmp.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.start = SubroutineFullName('__openmp_MOD_start')
        self.callGraph = callGraphBuilder.buildCallGraph(self.start)
        
        useTraversal = UseTraversal(self.sourceFiles, ['omp_lib'])
        useTraversal.parseModules(self.start)
        self.types = useTraversal.getTypes()
        self.interfaces = useTraversal.getInterfaces()
        
        
    def testAssemberFileExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)
                
                
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        sourceFile = self.sourceFiles.findSourceFile('openmp.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('openmp')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))

        module = sourceFile.getModule('openmp')
        self.assertIsNotNone(module)
        
        self.assertEqual({'start', 'random'}, set(module.getSubroutines().keys()))
        self.assertEqual({'tester'}, set(module.getVariables().keys()))
                
                
    def testTypes(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        self.assertEqual({'test'}, set([t.getName() for t in self.types]))


    def testCallGraphs(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        self.assertEqual({'start', 'random'}, set([name.getSimpleName() for name in self.callGraph.getAllSubroutineNames()]))

    def testArgument(self):
        if not self.filesExist:
            self.skipTest('Files not there')
           
        tracker = VariableTracker(self.sourceFiles, self.trackerSettings, self.interfaces, self.types)
           
        refs = tracker.trackDerivedTypeArguments(self.callGraph)
        expressions = set([ref.getExpression() for ref in refs])
        self.assertEqual({'arg%var1'}, expressions)

    def testGlobal(self):
        if not self.filesExist:
            self.skipTest('Files not there')
           
        tracker = GlobalVariableTracker(self.sourceFiles, self.trackerSettings, self.interfaces, self.types)
           
        refs = tracker.trackGlobalVariables(self.callGraph)
        expressions = set([ref.getExpression() for ref in refs])
        self.assertEqual({'tester%var2'}, expressions)
        
    def testModifiedAssember(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        callGraphBuilderMod = GNUx86AssemblerCallGraphBuilder(ASSEMBLER_DIR_MOD, self.specialModuleFiles)
        callGraphMod = callGraphBuilderMod.buildCallGraph(self.start)
        self.assertEqual({'start', 'random'}, set([name.getSimpleName() for name in callGraphMod.getAllSubroutineNames()]))
         
if __name__ == "__main__":
    unittest.main()