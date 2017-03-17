#!/usr/bin/python

import re
from source import Variable

print
print 'Type definition'
print '==============='

typeRegEx = re.compile(r'^TYPE\s*(,\s[A-Z]+(\([a-z0-9_]+\))?\s*)*((\:\:)|\s)\s*([a-z0-9_]+)\s*$', re.IGNORECASE);

statements = ['TYPE ATMOSPHERE', 
              'TYPE particle_nonsphere', 
              'TYPE, EXTENDS(particle_nonsphere) :: particle_rain']

for statement in statements:
    if typeRegEx.match(statement):
        print statement

print
print 'Variable declaration'
print '===================='        
        
varRegEx = re.compile(r'^(?P<typespecifier>((LOGICAL)|(INTEGER)|(DOUBLE PRECISION)|(REAL\s*\(\s*[a-z0-9_]{1,63}\s*\)\s*)|(COMPLEX)|(CHARACTER)|(TYPE\s*\(\s*[a-z0-9_]{1,63}\s*\)\s*)).*)\:\:(?P<varlist>.+)$', re.IGNORECASE)

statements = ['TYPE(particle_rain), PARAMETER :: rainSBBcoeffs = particle_rain( 9.292000, 9.623000, 6.222d+2, 6.000000, 3.000d+1, 1.000d+3, 1.100d-3, 1.000000, 2 )', 
              'REAL(wp), SAVE :: ice_sc_delta_n, ice_sc_delta_q, ice_sc_theta_n, ice_sc_theta_q',
              'DOUBLE PRECISION, INTENT(in) :: a',
              'TYPE(gamlookuptable), INTENT(inout) :: ltable',
              'INTEGER, INTENT(in) :: nl, nlhr',
              'INTEGER :: i, err',
              'DOUBLE PRECISION, PARAMETER :: c1 =  36.629433904824623d0, c2 = -0.119475603955226d0, c3 =  0.339332937820052d0, c4 =  1.156369000458310d0']

print 'RegEx'
print '-----'

for statement in statements:
    if varRegEx.match(statement):
        print statement

print 'Variable.validVariableDeclaration'
print '---------------------------------'

for statement in statements:
    if Variable.validVariableDeclaration(statement):
        print statement
        

print
print 'Brackets'
print '========'        
        
varRegEx = re.compile(r'.*\(.*\).*')

strings = ['rainSBBcoeffs = particle_rain( 9.292000, 9.623000, 6.222d+2, 6.000000, 3.000d+1, 1.000d+3, 1.100d-3, 1.000000, 2 )']

for text in strings:
    if varRegEx.match(text):
        print text
        
print        
print 'Function Calls'
print '=============='        
        
funcCallRegEx = re.compile(r'^.*(?P<routine>[a-z0-9_]+)\s*\((?P<before>(.*[^a-z0-9_])?)ltabdminwgg(?P<after>([^a-z0-9_].*)?)\)\s*$')

strings = ['d_trenn = dmin_wg_gr_ltab_equi(p_a,T_a,qw_a,qi_a,ltabdminwgg)']

for text in strings:
    if funcCallRegEx.match(text):
        print text
        
print        
print 'Function Definition'
print '==================='        
        
functionRegEx = re.compile(r'\s*(ELEMENTAL\s+)?(PURE\s+)?(RECURSIVE\s+)?((INTEGER\s+)|(LOGICAL\s+)|(DOUBLE(\s+PRECISION)?\s+)|(REAL(\(.*\))?s+)|(CHARACTER(\(.*\))?s+)|(TYPE(\(.*\))?s+))*FUNCTION\s+(?P<name>[a-z0-9_]{1,63})', re.IGNORECASE);
functionRegEx = re.compile(r'\s*(((ELEMENTAL)|(PURE)|(RECURSIVE)|(INTEGER)|(LOGICAL)|(DOUBLE(\s+PRECISION)?)|(REAL(\(.*\))?)|(CHARACTER(\(.*\))?)|(TYPE(\(.*\))?))\s+)*FUNCTION\s+(?P<name>[a-z0-9_]{1,63})', re.IGNORECASE);

strings = ['ELEMENTAL REAL(wp) FUNCTION Dv_Rasmussen(Ta,pa)']

for text in strings:
    if functionRegEx.match(text):
        print text
        
print        
print 'Access Definition'
print '==================='        

variableName = 'cloud'        
#accessRegEx = re.compile(r'(.*?[^a-z0-9_]+)?(?P<reference>' + variableName + r'%[a-z0-9_% ]+)(?P<suffix>.*)', re.IGNORECASE);
accessRegEx = re.compile(r'^(.*[^a-z0-9_])?' + variableName + r'%', re.IGNORECASE);

strings = ["WRITE(txt,'(A,D10.3)') \"   b_cloud    = \",cloud%b_geo ; CALL message(routine,TRIM(txt))"]

for text in strings:
    if accessRegEx.match(text):
        print text
        
print        
print 'Dimensions'
print '=========='        
statements = ['REAL A, B',
              'REAL, DIMENSION(10) :: A, B', 
              'INTEGER, DIMENSION(0:9) :: C',
              'REAL, DIMENSION(:), ALLOCATABLE :: A',
              'REAL, DIMENSION(2,3), INTENT(iN), OPTIONAL :: A',
              'REAL, DIMENSION(0:1,0:2) :: B',
              'INTEGER, DIMENSION(10,20,3) :: I',
              'REAL, DIMENSION(:, :), ALLOCATABLE :: A',
              'REAL, DIMENSION(:, f(a, b)), ALLOCATABLE :: A',
              'REAL A(:), B(10,n,f(a,b))',
              'REAL, DIMENSION(:,:) :: A(:), B(10,n,f(a,b)), C',]

for statement in statements:
    print '> ' + statement
    if Variable.validVariableDeclaration(statement):
        for var in Variable.fromDeclarationStatement(statement):
            print str(var)
    else:
        print 'INVALID!' 

print        
print 'Type Definition'
print '==============='        
        
typeRegEx = re.compile(r'^TYPE\s*(,\s*[A-Z]+(\([a-z0-9_]+\))?\s*)*((\:\:)|\s)\s*([a-z0-9_]+)$', re.IGNORECASE);

strings = ['TYPE PARTICLE', 'TYPE,EXTENDS(particle_frozen)::particle_lwf']

for text in strings:
    if typeRegEx.match(text):
        print text