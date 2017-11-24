#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/assignment'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from assembler import FromAssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName
from trackvariable import TrackVariableCallGraphAnalysis
from usetraversal import UseTraversal

''' 
Tests wether assignment are tracked correctly
'''
class SampleTest(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        callGraphBuilder = FromAssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        
        self.srcFile = SOURCE_DIR + '/assignment.f90'
        self.assFile = ASSEMBLER_DIR + '/assignment.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.direct = SubroutineFullName('__assignment_MOD_testDirect')
        self.callGraphDirect = callGraphBuilder.buildCallGraph(self.direct)

        self.indirect = SubroutineFullName('__assignment_MOD_testIndirect')
        self.callGraphIndirect = callGraphBuilder.buildCallGraph(self.indirect)

        self.member = SubroutineFullName('__assignment_MOD_testMember')
        self.callGraphMember = callGraphBuilder.buildCallGraph(self.member)
        
    def testAssemberFileExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)

    def testCallGraphs(self):
        # TODO Auch mit serialisiertem CallGraph testen
        if not self.filesExist:
            self.skipTest('Files not there')
        
        simpleNames = set()
        for name in self.callGraphDirect.getAllSubroutineNames():
            simpleNames.add(name.getSimpleName())
        self.assertEqual({'testdirect'}, simpleNames)
        
        simpleNames = set()
        for name in self.callGraphIndirect.getAllSubroutineNames():
            simpleNames.add(name.getSimpleName())
        self.assertEqual({'testindirect'}, simpleNames)
        
        simpleNames = set()
        for name in self.callGraphMember.getAllSubroutineNames():
            simpleNames.add(name.getSimpleName())
        self.assertEqual({'testmember'}, simpleNames)
                
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        sourceFile = self.sourceFiles.findSourceFile('assignment.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('assignment')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))

        module = sourceFile.getModule('assignment')
        self.assertIsNotNone(module)
        
        simpleNames = set(module.getSubroutines().keys())
        self.assertEqual({'testdirect', 'testindirect', 'testmember'}, simpleNames)
                
    def testDirect(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.direct)
        tracker = TrackVariableCallGraphAnalysis(self.sourceFiles, [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
        
        members = set()
        for ref in tracker.trackDerivedTypeArguments(self.callGraphDirect):
            members.add(ref.getLevelNVariable().getName())
        self.assertEqual({'first', 'second'}, members)
                
    def testIndirect(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.indirect)
        tracker = TrackVariableCallGraphAnalysis(self.sourceFiles, [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
        
        members = set()
        for ref in tracker.trackDerivedTypeArguments(self.callGraphIndirect):
            members.add(ref.getLevelNVariable().getName())
        self.assertEqual({'first', 'second'}, members)
                
# TODO: Doesn't work yet            
#     def testMember(self):
#         if not self.filesExist:
#             self.skipTest('Files not there')
#         
#         useTraversal = UseTraversal(self.sourceFiles, [])
#         useTraversal.parseModules(self.member)
#         tracker = TrackVariableCallGraphAnalysis(self.sourceFiles, [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
#         
#         members = set()
#         for ref in tracker.trackDerivedTypeArguments(self.callGraphMember):
#             members.add(ref.getLevelNVariable().getName())
#         self.assertEqual({'first', 'second'}, members)
                
        
if __name__ == "__main__":
    unittest.main()