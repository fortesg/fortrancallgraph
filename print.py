from __future__ import print_function
import sys

def printLine(line):
    print(line)

def printLines(lines):
    print('\n'.join(lines))
    
def printError(line, location = '', exitCode = 0):
    msg = 'ERROR'
    if location:
        msg += ' [' + location + ']'
    msg += ': ' + line
    print(msg, file=sys.stderr)
    if exit != 0:
        sys.exit(exitCode)
    
def printWarning(line, location = ''):
    msg = '*** WARNING'
    if location:
        msg += ' [' + location + ']'
    msg += ': ' + line + ' ***'
    print(msg, file=sys.stderr)