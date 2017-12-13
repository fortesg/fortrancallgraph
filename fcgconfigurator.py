import os
import sys
from assertions import assertType

CFG_SOURCE_DIRS = 'SOURCE_DIRS'
CFG_SOURCE_DIRS_LEGACY = 'SOURCE_DIR'
CFG_ASSEMBLER_DIRS = 'ASSEMBLER_DIRS'
CFG_ASSEMBLER_DIRS_LEGACY = 'ASSEMBLER_DIR'
CFG_SPECIAL_MODULE_FILES = 'SPECIAL_MODULE_FILES'
CFG_SOURCE_FILES_PREPROCESSED = 'SOURCE_FILES_PREPROCESSED'
CFG_CACHE_DIR = 'CACHE_DIR'
CFG_EXCLUDE_MODULES = 'EXCLUDE_MODULES'
CFG_IGNORE_GLOBALS_FROM_MODULES = 'IGNORE_GLOBALS_FROM_MODULES'
CFG_IGNORE_DERIVED_TYPES = 'IGNORE_DERIVED_TYPES'
        
def loadFortranCallGraphConfiguration(configFile, incomplete = False, baseConfig = {}):
    assertType(configFile, 'configFile', str, True)

    if configFile:
        configFile = configFile.strip('"\'')
    else:
        configFile = 'config_fortrancallgraph.py'
    originalConfigFile = configFile
    if not os.path.isfile(configFile):
        configFile = os.path.join(os.path.dirname(os.path.realpath(__file__)), originalConfigFile)
    if not os.path.isfile(configFile):
        configFile = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'config', originalConfigFile)
    if not os.path.isfile(configFile):
        print >> sys.stderr, 'Config file not found: ' + originalConfigFile
        return None
        
    config = baseConfig
    execfile(configFile, globals(), config)
    
    configError = False
    if CFG_SOURCE_DIRS not in config and CFG_SOURCE_DIRS_LEGACY in config:
        config[CFG_SOURCE_DIRS] = config[CFG_SOURCE_DIRS_LEGACY]
    if CFG_SOURCE_DIRS not in config or not config[CFG_SOURCE_DIRS]:
        if not incomplete:
            print >> sys.stderr, 'Missing config variable: ' + CFG_SOURCE_DIRS
            configError = True
    elif isinstance(config[CFG_SOURCE_DIRS], str):
        config[CFG_SOURCE_DIRS] = [config[CFG_SOURCE_DIRS]]

    if CFG_ASSEMBLER_DIRS not in config and CFG_ASSEMBLER_DIRS_LEGACY in config:
        config[CFG_ASSEMBLER_DIRS] = config[CFG_ASSEMBLER_DIRS_LEGACY]
    if CFG_ASSEMBLER_DIRS not in config or not config[CFG_ASSEMBLER_DIRS]:
        if not incomplete:
            print >> sys.stderr, 'Missing config variable: ' + CFG_ASSEMBLER_DIRS
            configError = True
    elif isinstance(config[CFG_ASSEMBLER_DIRS], str):
        config[CFG_ASSEMBLER_DIRS] = [config[CFG_ASSEMBLER_DIRS]]
        
    if CFG_SPECIAL_MODULE_FILES not in config or not config[CFG_SPECIAL_MODULE_FILES]:
        config[CFG_SPECIAL_MODULE_FILES] = {}
        
    if CFG_SOURCE_FILES_PREPROCESSED not in config or not config[CFG_SOURCE_FILES_PREPROCESSED]:
        config[CFG_SOURCE_FILES_PREPROCESSED] = False

    if CFG_CACHE_DIR not in config or not config[CFG_CACHE_DIR]:
        config[CFG_CACHE_DIR] = None

    if CFG_EXCLUDE_MODULES not in config or not config[CFG_EXCLUDE_MODULES]:
        config[CFG_EXCLUDE_MODULES] = []

    if CFG_IGNORE_GLOBALS_FROM_MODULES not in config or not config[CFG_IGNORE_GLOBALS_FROM_MODULES]:
        config[CFG_IGNORE_GLOBALS_FROM_MODULES] = []

    if CFG_IGNORE_DERIVED_TYPES not in config or not config[CFG_IGNORE_DERIVED_TYPES]:
        config[CFG_IGNORE_DERIVED_TYPES] = []
    
    if configError:
        return None
    
    return config