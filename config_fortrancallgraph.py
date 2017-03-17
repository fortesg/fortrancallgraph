from source import SourceFiles
from treecache import CachedAssemblerCallGraphBuilder
from assembler import FromAssemblerCallGraphBuilder
import os


### EDIT HERE ###

FCG_FOLDER = os.path.dirname(os.path.realpath(__file__))
CACHE_FOLDER = FCG_FOLDER + '/cache'

ASSEMBLER_FOLDER = ''
SOURCE_FOLDER = ''

SPECIAL_MODULE_FILES = {}
EXCLUDE_MODULES = []
IGNORE_GLOBALS_FROM_MODULES = [] 
IGNORE_DERIVED_TYPES = []

#################

GRAPH_BUILDER = CachedAssemblerCallGraphBuilder(CACHE_FOLDER, FromAssemblerCallGraphBuilder(ASSEMBLER_FOLDER, SPECIAL_MODULE_FILES))
SOURCE_FILES = SourceFiles(SOURCE_FOLDER, SPECIAL_MODULE_FILES)