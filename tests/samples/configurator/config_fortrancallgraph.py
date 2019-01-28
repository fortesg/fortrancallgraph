import os

FCG_DIR = os.path.dirname(os.path.realpath(__file__))

TEST_DIR = FCG_DIR + '/tests'

CACHE_DIR = TEST_DIR + '/samples/configurator/cache'

ASSEMBLER_DIRS = [TEST_DIR + '/samples/configurator/ass'] 
SOURCE_DIRS = [TEST_DIR + '/samples/configurator/src'] 

SPECIAL_MODULE_FILES = {'test': 'mo_test.f90',
                        'example': 'beispiel.f90' }
EXCLUDE_MODULES = [ 'iso_fortran_env', 'iso_c_binding', 'ifcore', 'mpi']

IGNORE_GLOBALS_FROM_MODULES = EXCLUDE_MODULES 
IGNORE_DERIVED_TYPES = []
