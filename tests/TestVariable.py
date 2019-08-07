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

    def testNormal(self):
        decl = "REAL(8)::glob1(109,1000),glob2(109,1000)"
        self.assertTrue(Variable.validVariableDeclaration(decl))
        variables = Variable.fromDeclarationStatement(decl, 'VariableTest')
        self.assertTrue(variables)
        glob1 = variables[0]
        self.assertIsNotNone(glob1)
        self.assertEqual('glob1', glob1.getName())
        self.assertEqual('REAL(8)', glob1.getTypeName())
        glob2 = variables[1]
        self.assertIsNotNone(glob2)
        self.assertEqual('glob2', glob2.getName())
        self.assertEqual('REAL(8)', glob2.getTypeName())

    def testNoColons(self):
        decl = "REAL(8)glob1(109,1000),glob2(109,1000)"
        self.assertTrue(Variable.validVariableDeclaration(decl))
        variables = Variable.fromDeclarationStatement(decl, 'VariableTest')
        self.assertTrue(variables)
        glob1 = variables[0]
        self.assertIsNotNone(glob1)
        self.assertEqual('glob1', glob1.getName())
        self.assertEqual('REAL(8)', glob1.getTypeName())
        glob2 = variables[1]
        self.assertIsNotNone(glob2)
        self.assertEqual('glob2', glob2.getName())
        self.assertEqual('REAL(8)', glob2.getTypeName())

    def testCommas(self):
        decl = "CHARACTER(LEN=sizes(1,1))commas"
        self.assertTrue(Variable.validVariableDeclaration(decl))
        variables = Variable.fromDeclarationStatement(decl, 'VariableTest')
        self.assertTrue(variables)
        commas = variables[0]
        self.assertIsNotNone(commas)
        self.assertEqual('commas', commas.getName())
        self.assertEqual('CHARACTER(LEN=sizes(1,1))', commas.getTypeName())

    def testGrb2(self):
        decl = "CHARACTER(LEN=LEN(grb2_grid_info)),PARAMETER::grb2_grid_info_lc=''"
        self.assertTrue(Variable.validVariableDeclaration(decl))
        variables = Variable.fromDeclarationStatement(decl, 'VariableTest')
        self.assertTrue(variables)
        grb2_grid_info_lc = variables[0]
        self.assertIsNotNone(grb2_grid_info_lc)
        self.assertEqual('grb2_grid_info_lc', grb2_grid_info_lc.getName())
        self.assertEqual('CHARACTER(LEN=LEN(grb2_grid_info))', grb2_grid_info_lc.getTypeName())

    def testGrb2NoColon(self):
        decl = "CHARACTER(LEN=LEN(grb2_grid_info))grb2_grid_info_lc"
        self.assertTrue(Variable.validVariableDeclaration(decl))
        variables = Variable.fromDeclarationStatement(decl, 'VariableTest')
        self.assertTrue(variables)
        grb2_grid_info_lc = variables[0]
        self.assertIsNotNone(grb2_grid_info_lc)
        self.assertEqual('grb2_grid_info_lc', grb2_grid_info_lc.getName())
        self.assertEqual('CHARACTER(LEN=LEN(grb2_grid_info))', grb2_grid_info_lc.getTypeName())

    def testIflag(self):
        decl = "logical::iflag=.true."
        self.assertTrue(Variable.validVariableDeclaration(decl))
        variables = Variable.fromDeclarationStatement(decl, 'VariableTest')
        self.assertTrue(variables)
        iflag = variables[0]
        self.assertIsNotNone(iflag)
        self.assertEqual('iflag', iflag.getName())
        self.assertEqual('logical', iflag.getTypeName())

    def testDimensionConstant(self):
        decl1 = "integer(LONG_KIND)::addrs_s(MAX_DOMAIN_FIELDS)"
        self.assertTrue(Variable.validVariableDeclaration(decl1))
        variables = Variable.fromDeclarationStatement(decl1, 'VariableTest')
        self.assertTrue(variables)
        addrs_s = variables[0]
        self.assertIsNotNone(addrs_s)
        self.assertEqual('addrs_s', addrs_s.getName())
        self.assertEqual('integer(LONG_KIND)', addrs_s.getTypeName())
        self.assertTrue(addrs_s.isArray())
        self.assertEqual(1, addrs_s.getDimension())

        decl2 = "integer,dimension(MAX_REQUEST)::buffer_pos_recv"
        self.assertTrue(Variable.validVariableDeclaration(decl2))
        variables = Variable.fromDeclarationStatement(decl2, 'VariableTest')
        self.assertTrue(variables)
        buffer_pos_recv = variables[0]
        self.assertIsNotNone(buffer_pos_recv)
        self.assertEqual('buffer_pos_recv', buffer_pos_recv.getName())
        self.assertEqual('integer', buffer_pos_recv.getTypeName())
        self.assertTrue(buffer_pos_recv.isArray())
        self.assertEqual(1, buffer_pos_recv.getDimension())

if __name__ == "__main__":
    unittest.main()