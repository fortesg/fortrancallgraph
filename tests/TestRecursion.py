#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/recursion'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from assembler import GNUx86AssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName, VariableReference
from trackvariable import VariableTracker
from usetraversal import UseTraversal

''' 
Tests whether recursive subroutines are tracked correctly
'''
class RecursionTest(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        callGraphBuilder = GNUx86AssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        
        self.srcFile = SOURCE_DIR + '/recursion.f90'
        self.assFile = ASSEMBLER_DIR + '/recursion.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.direct = SubroutineFullName('__recursion_MOD_recurse')
        self.callGraphDirect = callGraphBuilder.buildCallGraph(self.direct)
        
        self.indirect = SubroutineFullName('__recursion_MOD_indirect1')
        self.callGraphIndirect = callGraphBuilder.buildCallGraph(self.indirect)
        
        self.func = SubroutineFullName('__recursion_MOD_refunc')
        self.callGraphFunc = callGraphBuilder.buildCallGraph(self.func)
        
        self.position = SubroutineFullName('__recursion_MOD_position')
        self.callGraphPosition = callGraphBuilder.buildCallGraph(self.position)
        
        self.recdata = SubroutineFullName('__recursion_MOD_recdata')
        self.callGraphRecdata = callGraphBuilder.buildCallGraph(self.recdata)
        
        #sys.tracebacklimit = 0
        
    def testAssemberFileExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)
        
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        sourceFile = self.sourceFiles.findSourceFile('recursion.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('recursion')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))

        module = sourceFile.getModule('recursion')
        self.assertIsNotNone(module)
        
        simpleNames = set(module.getSubroutines().keys())
        self.assertEqual({'recurse', 'indirect1', 'indirect2', 'refunc', 'position', 'recdata'}, simpleNames)

    def testCallGraphs(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        self.assertEqual({'recurse'}, set(subroutineFullName.getSimpleName() for subroutineFullName in self.callGraphDirect.getAllSubroutineNames()))
        self.assertEqual({'indirect1', 'indirect2'}, set(subroutineFullName.getSimpleName() for subroutineFullName in self.callGraphIndirect.getAllSubroutineNames()))
        self.assertEqual({'refunc'}, set(subroutineFullName.getSimpleName() for subroutineFullName in self.callGraphFunc.getAllSubroutineNames()))
        self.assertEqual({'position'}, set(subroutineFullName.getSimpleName() for subroutineFullName in self.callGraphPosition.getAllSubroutineNames()))
        self.assertEqual({'recdata'}, set(subroutineFullName.getSimpleName() for subroutineFullName in self.callGraphRecdata.getAllSubroutineNames()))
        
    def testDirect(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.direct)
        tracker = VariableTracker(self.sourceFiles, [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
        
        self.assertEqual({'var%first'}, set(ref.getExpression() for ref in tracker.trackDerivedTypeArguments(self.callGraphDirect)))
                
    def testIndirect(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.indirect)
        tracker = VariableTracker(self.sourceFiles, [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
        
        self.assertEqual({'var%first', 'var%second'}, set(ref.getExpression() for ref in tracker.trackDerivedTypeArguments(self.callGraphIndirect)))
        
        
    def testFunc(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.func)
        tracker = VariableTracker(self.sourceFiles, [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
        
        self.assertEqual({'var%first'}, set(ref.getExpression() for ref in tracker.trackDerivedTypeArguments(self.callGraphFunc)))

    def testPosition(self):
        if not self.filesExist:
            self.skipTest('Files not there')

        subroutine = self.sourceFiles.findSubroutine(self.position)
        self.assertIsNotNone(subroutine)
        self.assertEqual(['var1', 'var2', 'i'], subroutine.getArgumentNames())
        self.assertEqual(['TYPE(test), INTENT(in) :: var1', 'TYPE(test), INTENT(in) :: var2', 'INTEGER, INTENT(in) :: i'], [str(a) for a in subroutine.getArguments()])
        
        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.position)
        tracker = VariableTracker(self.sourceFiles, [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
        
        self.assertEqual({'var1%first', 'var1%second', 'var2%first', 'var2%second'}, set(ref.getExpression() for ref in tracker.trackDerivedTypeArguments(self.callGraphPosition)))
        

    def testRecdata(self):
        if not self.filesExist:
            self.skipTest('Files not there')

        subroutine = self.sourceFiles.findSubroutine(self.recdata)
        self.assertIsNotNone(subroutine)
        self.assertEqual(['r'], subroutine.getArgumentNames())
        
        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.position)
        tracker = VariableTracker(self.sourceFiles, [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
        
        self.assertEqual({'r%second'}, set(ref.getExpression() for ref in tracker.trackDerivedTypeArguments(self.callGraphRecdata)))
        

    #TODO Functions
    #TODO andere Argumentposition
        
if __name__ == "__main__":
    unittest.main()