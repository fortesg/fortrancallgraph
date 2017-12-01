#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/typeprocedure'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from assembler import FromAssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName, VariableReference
from trackvariable import TrackVariableCallGraphAnalysis
from usetraversal import UseTraversal

''' 
Tests whether assignment are tracked correctly
'''
class TypeProcedureTest(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        callGraphBuilder = FromAssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        
        self.srcFile = SOURCE_DIR + '/typeprocedure.f90'
        self.assFile = ASSEMBLER_DIR + '/typeprocedure.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.test = SubroutineFullName('__typeprocedure_MOD_test')
        self.callGraph = callGraphBuilder.buildCallGraph(self.test)
        
    def testAssemberFileExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)

    def testCallGraphs(self):
        # TODO Auch mit serialisiertem CallGraph testen
        if not self.filesExist:
            self.skipTest('Files not there')
        
        self.assertEqual({'test'}, set(map(SubroutineFullName.getSimpleName, self.callGraph.getAllSubroutineNames())))
                
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        sourceFile = self.sourceFiles.findSourceFile('typeprocedure.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('typeprocedure')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))

        module = sourceFile.getModule('typeprocedure')
        self.assertIsNotNone(module)
        
        simpleNames = set(module.getSubroutines().keys())
        self.assertEqual({'test', 'dump'}, simpleNames)
                
    def testVariables(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.test)
        tracker = TrackVariableCallGraphAnalysis(self.sourceFiles, [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
        
        members = set()
        for ref in tracker.trackDerivedTypeArguments(self.callGraph):
            members.add(ref.getLevelNVariable().getName())
        self.assertEqual({'first', 'second'}, members)

        
if __name__ == "__main__":
    unittest.main()