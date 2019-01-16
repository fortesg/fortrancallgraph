#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/outvars'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from assembler import GNUx86AssemblerCallGraphBuilder
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
        callGraphBuilder = GNUx86AssemblerCallGraphBuilder(ASSEMBLER_DIR, specialModuleFiles)
        self.sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        
        self.srcFile = SOURCE_DIR + '/outvars.f90'
        self.assFile = ASSEMBLER_DIR + '/outvars.s'
        self.filesExist = os.path.exists(self.srcFile) and os.path.exists(self.assFile)
        
        self.primitive = SubroutineFullName('__outvars_MOD_primitive')
        self.callGraphPrimitive = callGraphBuilder.buildCallGraph(self.primitive)
        
        self.get = SubroutineFullName('__outvars_MOD_get')
        self.callGraphGet = callGraphBuilder.buildCallGraph(self.get)
        
        self.withouta = SubroutineFullName('__outvars_MOD_withouta')
        self.callGraphWithOutA = callGraphBuilder.buildCallGraph(self.withouta)
        
        self.withoutg = SubroutineFullName('__outvars_MOD_withoutg')
        self.callGraphWithOutG = callGraphBuilder.buildCallGraph(self.withoutg)
        
        self.testFunc1 = SubroutineFullName('__outvars_MOD_testFunc1')
        self.callGraphTestFunc1 = callGraphBuilder.buildCallGraph(self.testFunc1)
        
        self.testFunc2 = SubroutineFullName('__outvars_MOD_testFunc2')
        self.callGraphTestFunc2 = callGraphBuilder.buildCallGraph(self.testFunc2)
        
        self.testFunc3 = SubroutineFullName('__outvars_MOD_testFunc3')
        self.callGraphTestFunc3 = callGraphBuilder.buildCallGraph(self.testFunc3)
        
        self.testFunc4 = SubroutineFullName('__outvars_MOD_testFunc4')
        self.callGraphTestFunc4 = callGraphBuilder.buildCallGraph(self.testFunc4)
        
        self.testFunc5 = SubroutineFullName('__outvars_MOD_testFunc5')
        self.callGraphTestFunc5 = callGraphBuilder.buildCallGraph(self.testFunc5)
        
        self.testFunc6 = SubroutineFullName('__outvars_MOD_testFunc6')
        self.callGraphTestFunc6 = callGraphBuilder.buildCallGraph(self.testFunc6)
        
        self.testFunc7 = SubroutineFullName('__outvars_MOD_testFunc7')
        self.callGraphTestFunc7 = callGraphBuilder.buildCallGraph(self.testFunc7)
        
        self.testFunc8 = SubroutineFullName('__outvars_MOD_testFunc8')
        self.callGraphTestFunc8 = callGraphBuilder.buildCallGraph(self.testFunc8)
        
        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.testFunc1)
        self.types = useTraversal.getTypes()
        self.interfaces = useTraversal.getInterfaces()
        
        
    def testAssemberFileExists(self):
        self.assertTrue(os.path.exists(self.srcFile), 'Test will fail. Source file not found: ' + self.srcFile)
        self.assertTrue(os.path.exists(self.assFile), 'Test will fail. Assembler file not found: ' + self.assFile)
                
                
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
        self.assertEqual({'primitive', 'get', 'get2', 'part', 'part2', 'part3', 'withouta', 'withoutg', 'witha', 'withg', 
                          'testfunc1', 'testfunc2', 'testfunc3', 'testfunc4', 'testfunc5', 'testfunc6', 'testfunc7', 'testfunc8'}, simpleNames)
                
                
    def testTypes(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        self.assertEqual({'ttest', 'parent', 'grand', 'proxy'}, set([t.getName() for t in self.types]))
                
                
    def testInterfaces(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        self.assertEqual({'g', 'p', 'without'}, set(self.interfaces.keys()))


    def testCallGraphs(self):
        if not self.filesExist:
            self.skipTest('Files not there')
        
        self.assertEqual({'primitive'}, set([name.getSimpleName() for name in self.callGraphPrimitive.getAllSubroutineNames()]))
        self.assertEqual({'get'}, set([name.getSimpleName() for name in self.callGraphGet.getAllSubroutineNames()]))
        self.assertEqual({'testfunc1', 'get'}, set([name.getSimpleName() for name in self.callGraphTestFunc1.getAllSubroutineNames()]))
        self.assertEqual({'testfunc1'}, set(map(SubroutineFullName.getSimpleName, self.callGraphTestFunc1.getCallers(self.get))))
        self.assertEqual({'testfunc2', 'part'}, set([name.getSimpleName() for name in self.callGraphTestFunc2.getAllSubroutineNames()]))
        self.assertEqual({'testfunc3', 'part'}, set([name.getSimpleName() for name in self.callGraphTestFunc3.getAllSubroutineNames()]))
        self.assertEqual({'testfunc4', 'withouta', 'withoutg'}, set([name.getSimpleName() for name in self.callGraphTestFunc4.getAllSubroutineNames()]))
        self.assertEqual({'testfunc5', 'part', 'get2'}, set([name.getSimpleName() for name in self.callGraphTestFunc5.getAllSubroutineNames()]))
        self.assertEqual({'testfunc6', 'witha', 'withg', 'withouta', 'withoutg'}, set([name.getSimpleName() for name in self.callGraphTestFunc6.getAllSubroutineNames()]))
        self.assertEqual({'testfunc7', 'part2'}, set([name.getSimpleName() for name in self.callGraphTestFunc7.getAllSubroutineNames()]))
        self.assertEqual({'testfunc8', 'part3'}, set([name.getSimpleName() for name in self.callGraphTestFunc8.getAllSubroutineNames()]))
        
         
    def testOutArguments(self):
        if not self.filesExist:
            self.skipTest('Files not there')
          
        module = self.sourceFiles.findModule('outvars')
        self.assertIsNotNone(module)
        t1 = module.getVariable('t1')
        self.assertIsNotNone(t1)
        t2 = module.getVariable('t2')
        self.assertIsNotNone(t2)
 
        trackerPrimitive = VariableTracker(self.sourceFiles, [], [], self.interfaces, self.types)
        self.assertEqual(0, len(trackerPrimitive.getOutAssignments()))
        trackerPrimitive.trackVariables([t1, t2], self.callGraphPrimitive)
        self.assertEqual(0, len(trackerPrimitive.getOutAssignments()))
         
        trackerGet = VariableTracker(self.sourceFiles, [], [], self.interfaces, self.types)
        trackerGet.trackVariables([t1, t2], self.callGraphGet)
        self.assertEqual(2, len(trackerGet.getOutAssignments()))
         
        trackerWithOutA = VariableTracker(self.sourceFiles, [], [], self.interfaces, self.types)
        trackerWithOutA.trackDerivedTypeArguments(self.callGraphWithOutA)
        self.assertEqual(1, len(trackerWithOutA.getOutAssignments()))
         
        trackerTestFunc1 = VariableTracker(self.sourceFiles, [], [], self.interfaces, self.types)
        trackerTestFunc1.trackVariables([t1, t2], self.callGraphTestFunc1)
        self.assertEqual(0, len(trackerTestFunc1.getOutAssignments()))
         
        trackerTestFunc4 = VariableTracker(self.sourceFiles, [], [], self.interfaces, self.types)
        trackerTestFunc4.trackDerivedTypeArguments(self.callGraphTestFunc4)
        self.assertEqual(0, len(trackerTestFunc4.getOutAssignments()))
         
        trackerTestFunc5 = VariableTracker(self.sourceFiles, [], [], self.interfaces, self.types)
        trackerTestFunc5.trackDerivedTypeArguments(self.callGraphTestFunc5)
        self.assertEqual(0, len(trackerTestFunc5.getOutAssignments()))
         
        trackerTestFunc6 = VariableTracker(self.sourceFiles, [], [], self.interfaces, self.types)
        trackerTestFunc6.trackDerivedTypeArguments(self.callGraphTestFunc6)
        self.assertEqual(0, len(trackerTestFunc6.getOutAssignments()))
                  
    def testArgumentAsFunctionResult(self):
        if not self.filesExist:
            self.skipTest('Files not there')
          
        tracker = VariableTracker(self.sourceFiles, [], [], self.interfaces, self.types)
          
        refs = tracker.trackDerivedTypeArguments(self.callGraphTestFunc2)
        self.assertEqual({'mother%child%second', 'mother%child%third'}, set([ref.getExpression() for ref in refs]))
          
        refs = tracker.trackDerivedTypeArguments(self.callGraphTestFunc3)
        expressions = set([ref.getExpression() for ref in refs])
        self.assertEqual({'grandpa%child%child%third'}, expressions)
                  
    def testGlobalAsFunctionResult(self):
        if not self.filesExist:
            self.skipTest('Files not there')
          
        tracker = GlobalVariableTracker(self.sourceFiles, [], [], [], self.interfaces, self.types)
          
        refs = tracker.trackGlobalVariables(self.callGraphPrimitive)
        expressions = set([ref.getExpression() for ref in refs])
        self.assertEqual({'t1%second'}, expressions)
          
        refs = tracker.trackGlobalVariables(self.callGraphGet)
        expressions = set([ref.getExpression() for ref in refs])
        self.assertFalse(expressions)
          
        refs = tracker.trackGlobalVariables(self.callGraphTestFunc1)
        expressions = set([ref.getExpression() for ref in refs])
        self.assertEqual({'t1%first', 't2%first', 'g3%child%child%first', 't1%second', 't2%second', 'g3%child%child%second'}, expressions)
                          
    def testArgumentAsSubroutineOutVar(self):
        if not self.filesExist:
            self.skipTest('Files not there')
          
        tracker = VariableTracker(self.sourceFiles, [], [], self.interfaces, self.types)
        refs = tracker.trackDerivedTypeArguments(self.callGraphTestFunc4)
        expressions = set([ref.getExpression() for ref in refs])
        self.assertEqual({'father%child%first', 'father%child%second'}, expressions)
                         
    def testGlobalAsSubroutineOutVar(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        tracker = GlobalVariableTracker(self.sourceFiles, [], [], [], self.interfaces, self.types)
        refs = tracker.trackGlobalVariables(self.callGraphTestFunc4)
        expressions = set([ref.getExpression() for ref in refs])
        self.assertEqual({'t1%second'}, expressions)
        
    def testArgumentAsTypeBoundFunctionResultOnThis(self):
        if not self.filesExist:
            self.skipTest('Files not there')
          
        useTraversal = UseTraversal(self.sourceFiles, [])
        useTraversal.parseModules(self.testFunc5)
          
        tracker = VariableTracker(self.sourceFiles, [], [], self.interfaces, self.types)
        refs = tracker.trackDerivedTypeArguments(self.callGraphTestFunc5)
        expressions = set([ref.getExpression() for ref in refs])
        self.assertEqual({'m%child%first', 'm%child%second'}, expressions)
        
    def testArgumentAsTypeBoundFunctionResultOnOther(self):
        if not self.filesExist:
            self.skipTest('Files not there')
          
        tracker = VariableTracker(self.sourceFiles, [], [], self.interfaces, self.types)
        refs = tracker.trackDerivedTypeArguments(self.callGraphTestFunc7)
        expressions = set([ref.getExpression() for ref in refs])
        self.assertEqual({'stepmother%child%first'}, expressions)
        
    def testGlobalAsTypeBoundFunctionResultOnOther(self):
        if not self.filesExist:
            self.skipTest('Files not there')
          
        tracker = GlobalVariableTracker(self.sourceFiles, [], [], [], self.interfaces, self.types)
        refs = tracker.trackGlobalVariables(self.callGraphTestFunc5)
        expressions = set([ref.getExpression() for ref in refs])
        self.assertEqual({'t1%second', 't2%second'}, expressions)
                          
    def testArgumentAsTypeBoundSubroutineOutVarOnThis(self):
        if not self.filesExist:
            self.skipTest('Files not there')
          
        tracker = VariableTracker(self.sourceFiles, [], [], self.interfaces, self.types)
        refs = tracker.trackDerivedTypeArguments(self.callGraphTestFunc8)
        expressions = set([ref.getExpression() for ref in refs])
        self.assertEqual({'stepfather%child%third'}, expressions)
                          
    def testArgumentAsTypeBoundSubroutineOutVarOnOther(self):
        if not self.filesExist:
            self.skipTest('Files not there')
          
        tracker = VariableTracker(self.sourceFiles, [], [], self.interfaces, self.types)
        refs = tracker.trackDerivedTypeArguments(self.callGraphTestFunc6)
        expressions = set([ref.getExpression() for ref in refs])
        self.assertEqual({'f%child%first', 'f%child%third'}, expressions)
                         
    def testGlobalAsTypeBoundSubroutineOutVarOnOther(self):
        if not self.filesExist:
            self.skipTest('Files not there')
         
        tracker = GlobalVariableTracker(self.sourceFiles, [], [], [], self.interfaces, self.types)
        refs = tracker.trackGlobalVariables(self.callGraphTestFunc6)
        expressions = set([ref.getExpression() for ref in refs])
        self.assertEqual({'t1%third'}, expressions)
        
if __name__ == "__main__":
    unittest.main()