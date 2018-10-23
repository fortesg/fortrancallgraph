#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/outvars'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from assembler import FromAssemblerCallGraphBuilder
from source import SourceFiles, SubroutineFullName
from usetraversal import UseTraversal
from trackvariable import VariableTracker
from globals import GlobalVariableTracker

''' 
Tests whether assignments of function results are tracked correctly
'''
class OutVarsTest(unittest.TestCase):
    def setUp(self):
        specialModuleFiles = {}
        callGraphBuilder = FromAssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        
        self.srcFile = SOURCE_DIR + '/outvars.f90'
        self.assFile = ASSEMBLER_DIR + '/outvars.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.primitive = SubroutineFullName('__outvars_MOD_primitive')
        self.callGraphPrimitive = callGraphBuilder.buildCallGraph(self.primitive)
        
        self.get = SubroutineFullName('__outvars_MOD_get')
        self.callGraphGet = callGraphBuilder.buildCallGraph(self.get)
        
        self.testFunc1 = SubroutineFullName('__outvars_MOD_testFunc1')
        self.callGraphTestFunc1 = callGraphBuilder.buildCallGraph(self.testFunc1)
        
        self.testFunc2 = SubroutineFullName('__outvars_MOD_testFunc2')
        self.callGraphTestFunc2 = callGraphBuilder.buildCallGraph(self.testFunc2)
        
        self.testFunc3 = SubroutineFullName('__outvars_MOD_testFunc3')
        self.callGraphTestFunc3 = callGraphBuilder.buildCallGraph(self.testFunc3)
        
    def testAssemberFileExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)

    def testCallGraphs(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        self.assertEqual({'primitive'}, set([name.getSimpleName() for name in self.callGraphPrimitive.getAllSubroutineNames()]))
        self.assertEqual({'get'}, set([name.getSimpleName() for name in self.callGraphGet.getAllSubroutineNames()]))
        self.assertEqual({'testfunc1', 'get'}, set([name.getSimpleName() for name in self.callGraphTestFunc1.getAllSubroutineNames()]))
        self.assertEqual({'testfunc1'}, set(map(SubroutineFullName.getSimpleName, self.callGraphTestFunc1.getCallers(self.get))))
        self.assertEqual({'testfunc2', 'part'}, set([name.getSimpleName() for name in self.callGraphTestFunc2.getAllSubroutineNames()]))
        self.assertEqual({'testfunc3', 'part'}, set([name.getSimpleName() for name in self.callGraphTestFunc3.getAllSubroutineNames()]))
        
                
    def testSourceFiles(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        sourceFile = self.sourceFiles.findSourceFile('outvars.f90')
        self.assertIsNotNone(sourceFile)
        moduleFile = self.sourceFiles.findModuleFile('outvars')
        self.assertIsNotNone(moduleFile)
        self.assertEqual(sourceFile, moduleFile)
        self.assertEqual(1, len(sourceFile.getModules()))

        module = sourceFile.getModule('outvars')
        self.assertIsNotNone(module)
        
        simpleNames = set(module.getSubroutines().keys())
        self.assertEqual({'primitive', 'get', 'part', 'testfunc1', 'testfunc2', 'testfunc3'}, simpleNames)
        
        
    def testOutArguments(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        module = self.sourceFiles.findModule('outvars')
        self.assertIsNotNone(module)
        t1 = module.getVariable('t1')
        self.assertIsNotNone(t1)
        t2 = module.getVariable('t2')
        self.assertIsNotNone(t2)

        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.testFunc1)
        
        trackerPrimitive = VariableTracker(self.sourceFiles, [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
        self.assertEqual(0, len(trackerPrimitive.getOutAssignments()))
        trackerPrimitive.trackVariables([t1, t2], self.callGraphPrimitive)
        self.assertEqual(0, len(trackerPrimitive.getOutAssignments()))
        
        trackerGet = VariableTracker(self.sourceFiles, [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
        trackerGet.trackVariables([t1, t2], self.callGraphGet)
        self.assertEqual(2, len(trackerGet.getOutAssignments()))
        
        
        trackerTestFunc1 = VariableTracker(self.sourceFiles, [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
        trackerTestFunc1.trackVariables([t1, t2], self.callGraphTestFunc1)
        self.assertEqual(0, len(trackerTestFunc1.getOutAssignments()))
                 
    def testArgumentAsFunctionResult(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.testFunc2)
        tracker = VariableTracker(self.sourceFiles, [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
         
        refs = tracker.trackDerivedTypeArguments(self.callGraphTestFunc2)
        globalVars = set([ref.getExpression() for ref in refs])
        self.assertEqual({'mother%child%second'}, globalVars)
         
        refs = tracker.trackDerivedTypeArguments(self.callGraphTestFunc3)
        globalVars = set([ref.getExpression() for ref in refs])
        self.assertEqual({'grandpa%child%child%third'}, globalVars)
                 
    def testGlobalAsFunctionResult(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.testFunc1)
        tracker = GlobalVariableTracker(self.sourceFiles, [], [], [], useTraversal.getInterfaces(), useTraversal.getTypes())
         
        refs = tracker.trackGlobalVariables(self.callGraphPrimitive)
        globalVars = set([ref.getExpression() for ref in refs])
        self.assertEqual({'t1%second'}, globalVars)
         
        refs = tracker.trackGlobalVariables(self.callGraphGet)
        globalVars = set([ref.getExpression() for ref in refs])
        self.assertFalse(globalVars)
         
        refs = tracker.trackGlobalVariables(self.callGraphTestFunc1)
        globalVars = set([ref.getExpression() for ref in refs])
        self.assertEqual({'t1%first', 't2%first'}, globalVars)
        
if __name__ == "__main__":
    unittest.main()