#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/typeprocedure'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from assembler import GNUx86AssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName
from trackvariable import VariableTracker
from usetraversal import UseTraversal

''' 
Tests whether type-bound procedures are handled correctly
'''
class TypeProcedureTest(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        abstractTypes = {'atest':'ttest'}
        callGraphBuilder = GNUx86AssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles)
        
        self.srcFile = SOURCE_DIR + '/typeprocedure.f90'
        self.assFile = ASSEMBLER_DIR + '/typeprocedure.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.test = SubroutineFullName('__typeprocedure_MOD_test')
        self.callGraphTest = callGraphBuilder.buildCallGraph(self.test)
        self.useTraversal = UseTraversal(self.sourceFiles, abstractTypes = abstractTypes)
        self.useTraversal.parseModules(self.test)
        
        self.testIndirect = SubroutineFullName('__typeprocedure_MOD_testIndirect')
        self.callGraphIndirect = callGraphBuilder.buildCallGraph(self.testIndirect)
        
        self.testAdd = SubroutineFullName('__typeprocedure_MOD_testAdd')
        self.callGraphAddInt = callGraphBuilder.buildCallGraph(self.testAdd)
        
        self.testAnother = SubroutineFullName('__typeprocedure_MOD_testAnother')
        self.callGraphAnother = callGraphBuilder.buildCallGraph(self.testAnother)
        
        self.testGeneric = SubroutineFullName('__typeprocedure_MOD_testGeneric')
        self.callGraphGeneric = callGraphBuilder.buildCallGraph(self.testGeneric)
        
        self.testChild = SubroutineFullName('__typeprocedure_MOD_testChild')
        self.callGraphChild = callGraphBuilder.buildCallGraph(self.testChild)
        
        
        self.testDeferred = SubroutineFullName('__typeprocedure_MOD_testDeferred')
        self.callGraphDeferred = callGraphBuilder.buildCallGraph(self.testDeferred)
        
        self.fileExist = os.path.exists(self.srcFile)
        
    def testAssemberFileExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)
                
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
        self.assertEqual({'test', 'third_interface', 'dump', 'testadd', 'addint', 'testanother', 'addanother', 'testindirect', 'testgeneric', 'testchild', 'testdeferred', 'new_dump'}, simpleNames)

    def testCallGraphs(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        self.assertEqual({'test', 'dump'}, set(subroutineFullName.getSimpleName() for subroutineFullName in self.callGraphTest.getAllSubroutineNames()))
        self.assertEqual({'testindirect', 'test', 'dump'}, set(subroutineFullName.getSimpleName() for subroutineFullName in self.callGraphIndirect.getAllSubroutineNames()))
        self.assertEqual({'testadd', 'addint'}, set(subroutineFullName.getSimpleName() for subroutineFullName in self.callGraphAddInt.getAllSubroutineNames()))
        self.assertEqual({'testanother', 'addanother'}, set(subroutineFullName.getSimpleName() for subroutineFullName in self.callGraphAnother.getAllSubroutineNames()))
        self.assertEqual({'testgeneric', 'addanother'}, set(subroutineFullName.getSimpleName() for subroutineFullName in self.callGraphGeneric.getAllSubroutineNames()))
        self.assertEqual({'testchild', 'new_dump', 'addint'}, set(subroutineFullName.getSimpleName() for subroutineFullName in self.callGraphChild.getAllSubroutineNames()))
        #self.assertEqual({'testdeferred', 'dump'}, set(subroutineFullName.getSimpleName() for subroutineFullName in self.callGraphDeferred.getAllSubroutineNames()))
        
    def testUseTraversal(self):
        if not self.fileExist:
            self.skipTest('Files not there')
        
        self.assertEqual(0, len(self.useTraversal.getInterfaces()))
        self.assertEqual(3, len(self.useTraversal.getTypes()))
        
    def testChildType(self):
        if not self.fileExist:
            self.skipTest('Files not there')
        
        types = self.useTraversal.getTypes()
        child = types['child']
        self.assertIsNotNone(child)
        self.assertTrue(child.hasMember('ttest'))
        self.assertTrue
                
    def testTypeProcedureReference(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        tracker = VariableTracker(self.sourceFiles, [], [], self.useTraversal.getInterfaces(), self.useTraversal.getTypes())
        
        expressions = set(ref.getExpression() for ref in tracker.trackDerivedTypeArguments(self.callGraphTest))
        self.assertEqual({'t%first', 't%second'}, expressions)
                
    def testTypeProcedureReferenceIndirect(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        tracker = VariableTracker(self.sourceFiles, [], [], self.useTraversal.getInterfaces(), self.useTraversal.getTypes())
        
        expressions = set(ref.getExpression() for ref in tracker.trackDerivedTypeArguments(self.callGraphIndirect))
        self.assertEqual({'t%first', 't%second'}, expressions)
                
    def testTypeProcedureCall(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        tracker = VariableTracker(self.sourceFiles, [], [], self.useTraversal.getInterfaces(), self.useTraversal.getTypes())
        
        expressions = set(ref.getExpression() for ref in tracker.trackDerivedTypeArguments(self.callGraphAddInt))
        self.assertEqual({'t%first', 't%second'}, expressions)
                
    def testTypeProcedureArgument(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        tracker = VariableTracker(self.sourceFiles, [], [], self.useTraversal.getInterfaces(), self.useTraversal.getTypes())
         
        expressions = set(ref.getExpression() for ref in tracker.trackDerivedTypeArguments(self.callGraphAnother))
        self.assertEqual({'t1%first', 't2%first', 't1%second', 't2%second'}, expressions)
                
    def testGeneric(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        tracker = VariableTracker(self.sourceFiles, [], [], self.useTraversal.getInterfaces(), self.useTraversal.getTypes())
         
        expressions = set(ref.getExpression() for ref in tracker.trackDerivedTypeArguments(self.callGraphGeneric))
        self.assertEqual({'t1%first', 't2%first', 't1%second', 't2%second'}, expressions)
                
    def testChild(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        tracker = VariableTracker(self.sourceFiles, [], [], self.useTraversal.getInterfaces(), self.useTraversal.getTypes())
         
        expressions = set(ref.getExpression() for ref in tracker.trackDerivedTypeArguments(self.callGraphChild))
        self.assertEqual({'t%ttest%second'}, expressions)
                
    def testDeferred(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        tracker = VariableTracker(self.sourceFiles, [], [], self.useTraversal.getInterfaces(), self.useTraversal.getTypes())
         
        expressions = set(ref.getExpression() for ref in tracker.trackDerivedTypeArguments(self.callGraphDeferred))
        self.assertEqual({'a%ttest%second'}, expressions)

        
if __name__ == "__main__":
    unittest.main()