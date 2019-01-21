#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from source import Variable

''' 
Tests for strange variable declarations
'''
class VariableTest(unittest.TestCase):

    def testGrb2(self):
        decl = "CHARACTER(LEN=LEN(grb2_grid_info)),PARAMETER::grb2_grid_info_lc=''"
        variables = Variable.fromDeclarationStatement(decl, 'VariableTest')
        self.assertTrue(variables)
        grb2_grid_info_lc = variables[0]
        self.assertIsNotNone(grb2_grid_info_lc)
        self.assertEqual('grb2_grid_info_lc', grb2_grid_info_lc.getName())
        self.assertEqual('CHARACTER(LEN=LEN(grb2_grid_info))', grb2_grid_info_lc.getTypeName())

if __name__ == "__main__":
    unittest.main()