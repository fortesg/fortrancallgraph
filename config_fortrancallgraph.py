import os
from config.config_fortrancallgraph import ABSTRACT_TYPE_IMPLEMENTATIONS

# Config file for FortranCallGraph
# Can be placed in FortranCallGraph's root directory or in a subdirectory called config or at any place and picked with -cf

# Directory where serialized call trees are stored for quicker analysis
# OPTIONAL: If omitted call trees won't be cached
CACHE_DIR = os.path.dirname(os.path.realpath(__file__)) + '/cache'

# Locations of the assembler files 
# Directories are searched in the order of this list
# Subdirectories are included automatically 
# REQUIRED
ASSEMBLER_DIRS = [] 

# Locations of the original source files
# same as above
# REQUIRED
SOURCE_DIRS = [] 

# Are the files in SOURCE_DIRS already preprocessed?
# OPTIONAL, default: False
SOURCE_FILES_PREPROCESSED = False 

# dict of all modules that are not defined in a filename with correspondig name (module_name.f90)
# Format 'module_name':'actual_file_name'
# OPTIONAL
SPECIAL_MODULE_FILES = {}

# Modules to be excluded completely from the analysis
# OPTIONAL
EXCLUDE_MODULES = []

# Modules from which global variables shall not be listed when running -a globals or -a all
# OPTIONAL
IGNORE_GLOBALS_FROM_MODULES = EXCLUDE_MODULES + [] 

# Types whose components shall not be listed when running -a ... 
# OPTIONAL
IGNORE_DERIVED_TYPES = []

# dict of subtypes that are chosen as the one and only implementation of an abstract type. 
# FCG handles variables of a given abstract type as if the type were the given subtype.
# Format: 'abstract_type':('subtype_module','subtype')
# If the module with analyzed subroutine has a dependency to subtype_module anyway, you can leave it away:
# 'abstract_type':'subtype'
# OPTIONAL
ABSTRACT_TYPE_IMPLEMENTATIONS = {}
