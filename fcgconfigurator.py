import os
import sys
from utils import assertType
        
def loadFortranCallGraphConfiguration(configFile):
    assertType(configFile, 'configFile', str, True)

    if configFile:
        configFile = configFile.strip('"\'')
    else:
        configFile = 'config_fortrancallgraph.py'
    originalConfigFile = configFile
    if not os.path.isfile(configFile):
        configFile = os.path.dirname(os.path.realpath(__file__)) + '/' + originalConfigFile
    if not os.path.isfile(configFile):
        configFile = os.path.dirname(os.path.realpath(__file__)) + '/config/' + originalConfigFile
    if not os.path.isfile(configFile):
        print >> sys.stderr, 'Config file not found: ' + originalConfigFile
        return None
        
    config = {}
    execfile(configFile, globals(), config)
    
    #TODO Constants for keys
    
    configError = False
    if 'SOURCE_DIR' not in config or not config['SOURCE_DIR']:
        print >> sys.stderr, 'Missing config variable: SOURCE_DIR'
        configError = True
    elif isinstance(config['SOURCE_DIR'], str):
        config['SOURCE_DIR'] = [config['SOURCE_DIR']]

    if 'ASSEMBLER_DIR' not in config or not config['ASSEMBLER_DIR']:
        print >> sys.stderr, 'Missing config variable: ASSEMBLER_DIR'
        configError = True
    elif isinstance(config['ASSEMBLER_DIR'], str):
        config['ASSEMBLER_DIR'] = [config['ASSEMBLER_DIR']]
        
    if 'SPECIAL_MODULE_FILES' not in config or not config['SPECIAL_MODULE_FILES']:
        config['SPECIAL_MODULE_FILES'] = {}

    if 'EXCLUDE_MODULES' not in config or not config['CACHE_DIR']:
        config['EXCLUDE_MODULES'] = None

    if 'EXCLUDE_MODULES' not in config or not config['EXCLUDE_MODULES']:
        config['EXCLUDE_MODULES'] = []

    if 'IGNORE_GLOBALS_FROM_MODULES' not in config or not config['IGNORE_GLOBALS_FROM_MODULES']:
        config['IGNORE_GLOBALS_FROM_MODULES'] = []

    if 'IGNORE_DERIVED_TYPES' not in config or not config['IGNORE_DERIVED_TYPES']:
        config['IGNORE_DERIVED_TYPES'] = []
        
    if 'SOURCE_FILES_PREPROCESSED' not in config or not config['SOURCE_FILES_PREPROCESSED']:
        config['SOURCE_FILES_PREPROCESSED'] = False
    
    if configError:
        return None
    
    return config