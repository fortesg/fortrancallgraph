#!/usr/bin/python

import unittest
import os
import sys
TEST_DIR = os.path.dirname(os.path.realpath(__file__))
SOURCE_DIR = TEST_DIR + '/samples/types'
ASSEMBLER_DIR = SOURCE_DIR

FCG_DIR = TEST_DIR + '/..'
sys.path.append(FCG_DIR)

from source import SourceFiles, SubroutineFullName
from usetraversal import UseTraversal


class TestTypes(unittest.TestCase):
    def setUp(self):
        
        self.srcFile = SOURCE_DIR + '/types.f90'
        self.fileExist = os.path.exists(self.srcFile)
        
        specialModuleFiles = {}
        sourceFiles = SourceFiles(SOURCE_DIR, specialModuleFiles);
        root = SubroutineFullName('__types_MOD_test')
        self.module = sourceFiles.findModule('types')
        
        self.usetraversal = UseTraversal(sourceFiles)
        self.usetraversal.parseModules(root)
        self.types = self.usetraversal.getTypes()
        
    def testFileExists(self):
        self.assertTrue(self.fileExist, 'Test will fail. Source file not found: ' + self.srcFile)

    def testSourceFiles(self):
        if not self.fileExist:
            self.skipTest('File not there')
         
        self.assertIsNotNone(self.module)
        self.assertEqual(1, len(self.module.getSubroutines()))
        self.assertEqual(4, len(self.module.getVariables()))
         
        simpleNames = set((self.module.getVariables().keys()))
        self.assertEqual({'var0', 'var1', 'var2', 'var3'}, simpleNames)
            
    def testUseTraversal(self):
        if not self.fileExist:
            self.skipTest('Files not there')
        
        self.assertEqual(0, len(self.usetraversal.getInterfaces()))
        self.assertEqual(10, len(self.usetraversal.getTypes()))
            
    def testType0FromModA(self):
        var0 = self.module.getVariable('var0')
        self.assertIsNotNone(var0)
        self.assertEqual('TYPE(type0)', var0.getTypeName())
        self.assertTrue(var0.hasDerivedType())
        self.assertEqual('type0', var0.getDerivedTypeName())
        type0 = self.types.getTypeOfVariable(var0)
        self.assertIsNotNone(type0)
        self.assertEqual('INTEGER', type0.getMember('member').getTypeName())
        self.assertEqual('moda', type0.getModule().getName())
            
    def testType1FromModB(self):
        var1 = self.module.getVariable('var1')
        self.assertIsNotNone(var1)
        self.assertEqual('TYPE(type1)', var1.getTypeName())
        self.assertTrue(var1.hasDerivedType())
        self.assertEqual('type1', var1.getDerivedTypeName())
        type1 = self.types.getTypeOfVariable(var1)
        self.assertIsNotNone(type1)
        self.assertEqual('type1', type1.getName())
        self.assertEqual('REAL', type1.getMember('member').getTypeName())
        self.assertEqual('modb', type1.getModule().getName())
            
    def testAliasTypeBFromModB(self):
        var2 = self.module.getVariable('var2')
        self.assertIsNotNone(var2)
        self.assertEqual('TYPE(typeB)', var2.getTypeName())
        self.assertTrue(var2.hasDerivedType())
        self.assertEqual('typeb', var2.getDerivedTypeName())
        type2 = self.types.getTypeOfVariable(var2)
        self.assertIsNotNone(type2)
        self.assertEqual('type2', type2.getName())
        self.assertEqual('REAL', type2.getMember('member').getTypeName())
        self.assertEqual('modb', type2.getModule().getName())
            
    def testWildcardType3FromModC(self):
        var3 = self.module.getVariable('var3')
        self.assertIsNotNone(var3)
        self.assertEqual('TYPE(type3)', var3.getTypeName())
        self.assertTrue(var3.hasDerivedType())
        self.assertEqual('type3', var3.getDerivedTypeName())
        type3 = self.types.getTypeOfVariable(var3)
        self.assertIsNotNone(type3)
        self.assertEqual('type3', type3.getName())
        self.assertEqual('TYPE(typeA)', type3.getMember('member1').getTypeName())
        self.assertEqual('modc', type3.getModule().getName())

        type2 = type3.getExtends()
        self.assertIsNotNone(type2)
        self.assertEqual('type2', type2.getName())
        self.assertEqual('modc', type2.getModule().getName())
        
if __name__ == "__main__":
    unittest.main()