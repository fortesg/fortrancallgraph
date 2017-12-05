#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from source import SubroutineFullName, VariableReference, Variable, Module, SourceFile, Type

''' 
Tests whether assignment are tracked correctly
'''
class VariableReferenceTest(unittest.TestCase):
    
    def setUp(self):
        file = SourceFile('test', True)
        module = Module('test', [], file, 0)
        ttest = Type('ttest', module, None)
        subroutineName = SubroutineFullName('__test_MOD_sub')
        
        self.var1 = Variable('first', 'INTEGER')
        ttest.addMember(self.var1)
        varRec  = Variable('rec', 'TYPE(ttest)')
        varRec.setType(ttest)
        ttest.addMember(varRec)
        ttest.addProcedure('proc2', 'procedure')
        ttest.addProcedure('proc3', 'gnasl')
        ttest.addProcedure('generic', ['proc2', 'proc3'])
        self.var0 = Variable('t', 'TYPE(ttest)')
        self.var0.setType(ttest)
        self.var3 = Variable('unbekanntes', 'TYPE(u)')
        self.ref1 = VariableReference('t%first', subroutineName, 23, self.var0)

        self.ref3 = VariableReference('unbekanntes%pferd(1)%lauf%heim', subroutineName, 42, self.var3)
        self.proc2 = VariableReference('t%proc2(1)%test', subroutineName, 109, self.var0)
        self.proc1 = VariableReference('t%proc2', subroutineName, 109, self.var0)
        self.generic = VariableReference('t%generic', subroutineName, 109, self.var0)
        self.recursive = VariableReference('t%rec%first', subroutineName, 109, self.var0)
        
    def testLevel(self):
        self.assertEqual(1, self.ref1.getLevel())
        self.assertEqual(3, self.ref3.getLevel())
        self.assertEqual(2, self.proc2.getLevel())
        self.assertEqual(1, self.proc1.getLevel())
    
    def testExpression(self):
        self.assertEqual('unbekanntes%pferd%lauf%heim', self.ref3.getExpression())
        self.assertEqual('unbekanntes', self.ref3.getExpression(0))
        self.assertEqual('unbekanntes%pferd', self.ref3.getExpression(1))
        self.assertEqual('unbekanntes%pferd%lauf', self.ref3.getExpression(2))
        self.assertEqual('unbekanntes%pferd%lauf%heim', self.ref3.getExpression(3))

        self.assertEqual('t%first', self.ref1.getExpression())
        self.assertEqual('t', self.ref1.getExpression(0))
        self.assertEqual('t%first', self.ref1.getExpression(1))

        self.assertEqual('t%proc2%test', self.proc2.getExpression())
        self.assertEqual('t', self.proc2.getExpression(0))
        self.assertEqual('t%proc2', self.proc2.getExpression(1))
        self.assertEqual('t%proc2%test', self.proc2.getExpression(2))
        
    def testVariable(self):
        self.assertEqual(self.var3, self.ref3.getVariable(0))
        self.assertIsNone(self.ref3.getVariable(1))
        self.assertEqual(self.var3, self.ref3.getLevel0Variable())
        self.assertIsNone(self.ref3.getLevelNVariable())
        
        self.assertEqual(self.var0, self.ref1.getVariable(0))
        self.assertEqual(self.var1, self.ref1.getVariable(1))
        
    def testProcedure(self):
        self.assertTrue(self.proc2.containsProcedure())
        self.assertEqual('procedure', self.proc2.findFirstProcedure())
        self.assertEqual('t', self.proc2.getSubReferenceBeforeFirstProcedure().getExpression())
        
        self.assertFalse(self.ref1.lastIsProcedure())
        self.assertFalse(self.ref3.lastIsProcedure())
        self.assertFalse(self.proc2.lastIsProcedure())
        self.assertTrue(self.proc1.lastIsProcedure())
        
    def testGeneric(self):
        self.assertTrue(self.generic.containsProcedure())
        self.assertTrue(self.generic.lastIsProcedure())
        self.assertEqual(['procedure', 'gnasl'], self.generic.findFirstProcedure())
        self.assertEqual('t', self.generic.getSubReferenceBeforeFirstProcedure().getExpression())
        
    def testRecursive(self):
        self.assertFalse(self.proc1.isRecursive())
        self.assertFalse(self.proc2.isRecursive())
        self.assertFalse(self.generic.isRecursive())
        self.assertTrue(self.recursive.isRecursive())
        
if __name__ == "__main__":
    unittest.main()