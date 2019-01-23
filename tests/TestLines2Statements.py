#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from source import SourceFile

''' 
Tests for strange variable declarations
'''
class Lines2StatementsTest(unittest.TestCase):

    def testComments(self):
        line = '! Comment on line start'
        statement = ''
        self.assertStatement(line, statement)

        line = '   ! Comment with blanks in front'
        statement = ''
        self.assertStatement(line, statement)

        line = 'CALL sub(a) ! Comment with statement in front'
        statement = 'CALL sub(a)'
        self.assertStatement(line, statement)
        
        
    def assertStatement(self, line, expStatement):    
        statements = SourceFile.linesToStatements([(0,line)])
        if not expStatement:
            self.assertEqual(0, len(statements))
        else:
            self.assertEqual(1, len(statements))
            self.assertEqual(3, len(statements[0]))
            statement = statements[0][1]
            self.assertEqual(expStatement, statement)

if __name__ == "__main__":
    unittest.main()