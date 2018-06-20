import re

REGEX_TYPE = type(re.compile('x'))

def assertType(var, varName, expectedType, noneAccepted = False):
    assert isinstance(varName, str)
    assert isinstance(expectedType, type) or ( isinstance(expectedType, list) and all(isinstance(t, type) for t in expectedType)) 
    assert isinstance(noneAccepted, bool)

    if var is None and noneAccepted:
        return
    
    if isinstance(expectedType, list):
        try:
            assert any(isinstance(var, t) for t in expectedType)
        except AssertionError:
            raise TypeError('Variable ' + varName + ' has type ' + str(type(var)) + ', expected one of: ' + str(map(str,expectedType)))
    else:
        try:
            assert isinstance(var, expectedType)
        except AssertionError:
            raise TypeError('Variable ' + varName + ' has type ' + str(type(var)) + ', expected: ' + str(expectedType))
        
def assertTypeAll(variables, varName, expectedType, noneAccepted = False):
    assertType(variables, 'vars', [list, set, dict, tuple], True)
    assert isinstance(varName, str)
    assert isinstance(expectedType, type) or ( isinstance(expectedType, list) and all(isinstance(t, type) for t in expectedType)) 
    assert isinstance(noneAccepted, bool)

    if variables is None and noneAccepted:
        return
    
    for i, var in enumerate(variables):
        assertType(var, varName + '[' + str(i) + ']', expectedType)  
        
