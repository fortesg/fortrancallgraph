from source import SourceFiles
from treecache import CachedAssemblerCallGraphBuilder
from assembler import FromAssemblerCallGraphBuilder
import os


### EDIT HERE ###

FCG_DIR = os.path.dirname(os.path.realpath(__file__))
CACHE_DIR = FCG_DIR + '/cache'

ASSEMBLER_DIR = ''
SOURCE_DIR = ''

SPECIAL_MODULE_FILES = {}
EXCLUDE_MODULES = []
IGNORE_GLOBALS_FROM_MODULES = [] 
IGNORE_DERIVED_TYPES = []

#################

GRAPH_BUILDER = CachedAssemblerCallGraphBuilder(CACHE_DIR, FromAssemblerCallGraphBuilder(ASSEMBLER_DIR, SPECIAL_MODULE_FILES))
SOURCE_FILES = SourceFiles(SOURCE_DIR, SPECIAL_MODULE_FILES)
