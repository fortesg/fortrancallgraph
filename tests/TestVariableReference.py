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
        ttest.addProcedure('proc', 'procedure')
        self.var2 = Variable('t', 'TYPE(ttest)')
        self.var2.setType(ttest)
        self.var4 = Variable('unbekanntes', 'TYPE(u)')
        self.ref2 = VariableReference('t%first', subroutineName, 23, self.var2)

        self.ref4 = VariableReference('unbekanntes%pferd(1)%lauf%heim', subroutineName, 42, self.var4)
        self.proc = VariableReference('t%proc(1)%test', subroutineName, 109, self.var2)
        
    def testExpression(self):
        self.assertEqual('unbekanntes%pferd%lauf%heim', self.ref4.getExpression())
        self.assertEqual('unbekanntes', self.ref4.getExpression(0))
        self.assertEqual('unbekanntes%pferd', self.ref4.getExpression(1))
        self.assertEqual('unbekanntes%pferd%lauf', self.ref4.getExpression(2))
        self.assertEqual('unbekanntes%pferd%lauf%heim', self.ref4.getExpression(3))

        self.assertEqual('t%first', self.ref2.getExpression())
        self.assertEqual('t', self.ref2.getExpression(0))
        self.assertEqual('t%first', self.ref2.getExpression(1))

        self.assertEqual('t%proc%test', self.proc.getExpression())
        self.assertEqual('t', self.proc.getExpression(0))
        self.assertEqual('t%proc', self.proc.getExpression(1))
        self.assertEqual('t%proc%test', self.proc.getExpression(2))
        
    def testVariable(self):
        self.assertEqual(self.var4, self.ref4.getVariable(0))
        self.assertIsNone(self.ref4.getVariable(1))
        self.assertEqual(self.var4, self.ref4.getLevel0Variable())
        self.assertIsNone(self.ref4.getLevelNVariable())
        
        self.assertEqual(self.var2, self.ref2.getVariable(0))
        self.assertEqual(self.var1, self.ref2.getVariable(1))
        
    def testProcedure(self):
        self.assertTrue(self.proc.containsProcedure())
        
if __name__ == "__main__":
    unittest.main()