#!/usr/bin/python

import unittest
import os
import sys
TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/nested'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from tree import TreeLikeCallGraphPrinter
from assembler import FromAssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName
from globals import GlobalVariablesCallGraphAnalysis
from trackvariable import TrackVariableCallGraphAnalysis
from usetraversal import UseTraversal


class TestNested(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        callGraphBuilder = FromAssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        
        self.srcFile = SOURCE_DIR + '/nested.f90'
        self.assFile = ASSEMBLER_DIR + '/nested.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.root = SubroutineFullName('__nested_MOD_test')
        self.callGraph = callGraphBuilder.buildCallGraph(self.root)
        
        self.printer = TreeLikeCallGraphPrinter()
        self.globalsTracker = GlobalVariablesCallGraphAnalysis(self.sourceFiles)
        
        self.usetraversal = UseTraversal(self.sourceFiles)
        
        #self.tracker = TrackVariableCallGraphAnalysis(self.sourceFiles, [], [], interfaces, types)
        
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
        self.assertEqual({'test', 'assertion', 'is_contained_in_e_nd'}, simpleNames)
                
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        sourceFile = self.sourceFiles.findSourceFile('nested.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('nested')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))
 
        module = sourceFile.getModule('nested')
        self.assertIsNotNone(module)
        self.assertEqual(4, len(module.getSubroutines()))
        self.assertEqual(1, len(module.getVariables()))
         
        simpleNames = set((module.getSubroutines().keys()))
        self.assertEqual({'test', 'assertion', 'is_contained_in_e', 'is_contained_in_e_nd'}, simpleNames)
            
    def testUseTraversal(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        self.usetraversal.parseModules(self.root)
        self.assertEqual(1, len(self.usetraversal.getInterfaces()))
        self.assertEqual(3, len(self.usetraversal.getTypes()))
                
    def testTrackVariables(self):
        if not self.filesExist:
            self.skipTest('Files not there')
            
        self.usetraversal.parseModules(self.root)
        
        tracker = TrackVariableCallGraphAnalysis(self.sourceFiles, [], [], self.usetraversal.getInterfaces(), self.usetraversal.getTypes())
        expressions = set()
        for varRef in tracker.trackDerivedTypeArguments(self.callGraph):
            expressions.add(varRef.getExpression())
        self.assertEqual({'dm_array%sub_arrays_global_desc%rect%first', 'dm_array%sub_arrays_global_desc%rect%size'}, expressions)    
        
if __name__ == "__main__":
    unittest.main()