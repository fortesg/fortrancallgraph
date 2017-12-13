import os

FCG_DIR = os.path.dirname(os.path.realpath(__file__))

# Directory where serialized call trees are stored for quicker analysis
CACHE_DIR = FCG_DIR + '/cache'

# Locations of the assembler files 
# Directories are searched in the order of this list
# Subdirectories are included automatically 
ASSEMBLER_DIRS = [] 

# Locations of the original source files
# same as above
SOURCE_DIRS = [] 

# Are the files in SOURCE_DIRS already preprocessed?
SOURCE_FILES_PREPROCESSED = False 

# dict of all modules that are not defined in a filename with correspondig name (module_name.f90)
# Format 'module_name': 'actual_file_name'
SPECIAL_MODULE_FILES = {}

# Modules to be excluded completely from the analysis
EXCLUDE_MODULES = []


# Modules from which global variables shall not be listed when running -a globals or -a all
IGNORE_GLOBALS_FROM_MODULES = EXCLUDE_MODULES + [] 

# Types whose components shall not be listed when running -a ... 
IGNORE_DERIVED_TYPES = []
