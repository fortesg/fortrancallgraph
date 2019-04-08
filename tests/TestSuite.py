#! /usr/bin/python

import unittest

# import your test modules
import TestInner
import TestMod
import TestUse
import TestFunctions
import TestNested
import TestTypes
import TestAssignment
import TestBrackets
import TestRecursion
import TestVariableReference
import TestTypeProcedure
import TestAlwaysFull
import TestOutVars
import TestPreprocessed
import TestOpenMP
import TestVariable
import TestLineNumbers
import TestLines2Statements
import TestConfigurator

# initialize the test suite
loader = unittest.TestLoader()
suite  = unittest.TestSuite()

# add tests to the test suite
suite.addTests(loader.loadTestsFromModule(TestInner))
suite.addTests(loader.loadTestsFromModule(TestMod))
suite.addTests(loader.loadTestsFromModule(TestUse))
suite.addTests(loader.loadTestsFromModule(TestFunctions))
suite.addTests(loader.loadTestsFromModule(TestNested))
suite.addTests(loader.loadTestsFromModule(TestTypes))
suite.addTests(loader.loadTestsFromModule(TestAssignment))
suite.addTests(loader.loadTestsFromModule(TestBrackets))
suite.addTests(loader.loadTestsFromModule(TestRecursion))
suite.addTests(loader.loadTestsFromModule(TestVariableReference))
suite.addTests(loader.loadTestsFromModule(TestTypeProcedure))
suite.addTests(loader.loadTestsFromModule(TestAlwaysFull))
suite.addTests(loader.loadTestsFromModule(TestOutVars))
suite.addTests(loader.loadTestsFromModule(TestPreprocessed))
suite.addTests(loader.loadTestsFromModule(TestOpenMP))
suite.addTests(loader.loadTestsFromModule(TestVariable))
suite.addTests(loader.loadTestsFromModule(TestLineNumbers))
suite.addTests(loader.loadTestsFromModule(TestLines2Statements))
suite.addTests(loader.loadTestsFromModule(TestConfigurator))

# initialize a runner, pass it your suite and run it
runner = unittest.TextTestRunner(verbosity=3)
result = runner.run(suite)