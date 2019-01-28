#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from fcgconfigurator import loadFortranCallGraphConfiguration, CFG_SOURCE_DIRS, CFG_ASSEMBLER_DIRS, CFG_SPECIAL_MODULE_FILES,\
    CFG_CACHE_DIR, CFG_SOURCE_FILES_PREPROCESSED, CFG_EXCLUDE_MODULES, CFG_IGNORE_GLOBALS_FROM_MODULES, CFG_IGNORE_DERIVED_TYPES

''' 
Tests for strange variable declarations
'''
class ConfiguratorTest(unittest.TestCase):

    def testConfig(self):
        config = loadFortranCallGraphConfiguration(TEST_DIR + '/samples/configurator/config_fortrancallgraph.py')
        self.assertIsNotNone(config)
        self.assertEqual([TEST_DIR + '/samples/configurator/ass'], config[CFG_ASSEMBLER_DIRS])
        self.assertEqual([TEST_DIR + '/samples/configurator/src'], config[CFG_SOURCE_DIRS])

if __name__ == "__main__":
    unittest.main()