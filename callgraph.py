from assertions import assertType
from source import SubroutineFullName, SubroutineName, InnerSubroutineName
import sys

class _CallGraphCall(object):
    
    ATTR_CALLEE_NAME = 'calleeName'
    ATTR_CALLEE_HOST = 'calleeHost'
    ATTR_LINE_NUMBER = 'lineNumber'
    ATTR_DISCRIMINATOR = 'discriminator'
    
    def __init__(self, calleeName, lineNumber, discriminator):
        self.__calleeName = calleeName
        self.__lineNumber = lineNumber
        self.__discriminator = discriminator
        
    def __str__(self, *args, **kwargs):
        return 'call ' + str(self.__calleeName) + ' ' + str(self.__lineNumber) + ' ' + str(self.__discriminator) 
    
    def __cmp__(self, other):
        if other.__lineNumber == self.__lineNumber:
            return self.__discriminator - other.__discriminator
        else:
            return self.__lineNumber - other.__lineNumber
        
    def serialize(self):
        ser = dict()
        ser[_CallGraphCall.ATTR_CALLEE_NAME] = str(self.__calleeName)
        if isinstance(self.__calleeName, InnerSubroutineName):
            ser[_CallGraphCall.ATTR_CALLEE_HOST] = str(self.__calleeName.getHostName())
        ser[_CallGraphCall.ATTR_LINE_NUMBER] = self.__lineNumber
        ser[_CallGraphCall.ATTR_DISCRIMINATOR] = self.__discriminator
        
        return ser
    
    @staticmethod
    def deserialize(ser):
        if _CallGraphCall.ATTR_CALLEE_NAME not in ser or _CallGraphCall.ATTR_LINE_NUMBER not in ser or _CallGraphCall.ATTR_DISCRIMINATOR not in ser:
            raise ValueError('No valid _CallGraphCall serialization')
        
        name = str(ser[_CallGraphCall.ATTR_CALLEE_NAME])
        if SubroutineFullName.validFullName(name):
            subroutineName = SubroutineFullName(name)
        else:
            subroutineName = InnerSubroutineName(name, SubroutineFullName(str(ser[_CallGraphCall.ATTR_CALLEE_HOST])))
            
        return _CallGraphCall(subroutineName, ser[_CallGraphCall.ATTR_LINE_NUMBER], ser[_CallGraphCall.ATTR_DISCRIMINATOR])
    
    def getCalleeName(self):
        return self.__calleeName;
    
    def getLineNumber(self):
        return self.__lineNumber
        
class _CallGraphSubroutine(object):
    
    ATTR_NAME = 'name'
    ATTR_HOST = 'host'
    ATTR_CALLS = 'calls'
    
    def __init__(self, subroutineName):
        self.__name = subroutineName
        self.__calls = []
        
    def serialize(self):
        ser = dict()
        ser[_CallGraphSubroutine.ATTR_NAME] = str(self.__name)
        if isinstance(self.__name, InnerSubroutineName):
            ser[_CallGraphSubroutine.ATTR_HOST] = str(self.__name.getHostName())
        ser[_CallGraphSubroutine.ATTR_CALLS] = []
        for call in self.__calls:
            ser[_CallGraphSubroutine.ATTR_CALLS].append(call.serialize())
        
        return ser
    
    @staticmethod
    def deserialize(ser):
        if _CallGraphSubroutine.ATTR_NAME not in ser or _CallGraphSubroutine.ATTR_CALLS not in ser:
            raise ValueError('No valid _CallGraphSubroutine serialization')
        
        name = str(ser[_CallGraphSubroutine.ATTR_NAME])
        if SubroutineFullName.validFullName(name):
            subroutineName = SubroutineFullName(name)
        else:
            subroutineName = InnerSubroutineName(name, SubroutineFullName(str(ser[_CallGraphSubroutine.ATTR_HOST])))
        
        cgs = _CallGraphSubroutine(subroutineName)
        for call in ser[_CallGraphSubroutine.ATTR_CALLS]:
            cgs.__calls.append(_CallGraphCall.deserialize(call))
            
        return cgs
    
    def getName(self):
        return self.__name;
    
    def addCall(self, calleeName, lineNumber, discriminator):
        
        if isinstance(calleeName, InnerSubroutineName) and calleeName.getModuleName() is None:
            calleeName.setModuleName(self.getName().getModuleName())
        
        self.__calls.append(_CallGraphCall(calleeName, lineNumber, discriminator))
        
    def getCallees(self):
        callees = set()
        for call in self.__calls:
            callees.add(call.getCalleeName())
        return callees
    
    def getSortedCallees(self):
        callees = []
        for call in sorted(self.__calls):
            callees.append(call.getCalleeName())
        return callees
        
    def getCalls(self):
        calls = set();
        for call in self.__calls:
            calls.add((self.__name, call.getCalleeName()))
        return calls
    
    def findCalleeBySimpleName(self, calleeSimpleName):
        for call in self.__calls:
            calleeName = call.getCalleeName()
            if calleeName.getSimpleName() == calleeSimpleName:
                return calleeName
            
        return None
    
    def findNextCalleesFromLine(self, lineNumber):
        nextCallees = []
        smallestLineNumberDelta = sys.maxsize
        for call in self.__calls:
            lineNumberDelta = abs(lineNumber - call.getLineNumber()) 
            if lineNumberDelta < smallestLineNumberDelta:
                nextCallees = [call.getCalleeName()]
                smallestLineNumberDelta = lineNumberDelta
            elif lineNumberDelta == smallestLineNumberDelta:
                nextCallees.append(call.getCalleeName())
        
        return nextCallees

class CallGraph(object):
    
    ATTR_ROOT_SUBROUTINE = 'rootSubroutine'
    ATTR_SUBROUTINES = 'subroutines'
    
    def __init__(self):
        self.__rootSubroutine = None;
        self.__subroutines = dict();
        
    def __contains__(self, subroutineName):
        assertType(subroutineName, 'subroutineName', SubroutineName)

        return subroutineName in self.__subroutines
    
    def __iter__(self):
        return iter(self.__subroutines);
    
    def serialize(self):
        ser = dict()
        ser[CallGraph.ATTR_ROOT_SUBROUTINE] = str(self.__rootSubroutine)
        ser[CallGraph.ATTR_SUBROUTINES] = dict()
        for name, subroutine in self.__subroutines.iteritems():
            ser[CallGraph.ATTR_SUBROUTINES][str(name)] = subroutine.serialize()
        
        return ser
    
    @staticmethod
    def deserialize(ser):
        if CallGraph.ATTR_ROOT_SUBROUTINE not in ser or CallGraph.ATTR_SUBROUTINES not in ser:
            raise ValueError('No valid CallGraph serialization')
        
        callgraph = CallGraph()
        callgraph.__rootSubroutine = SubroutineFullName(str(ser[CallGraph.ATTR_ROOT_SUBROUTINE]))
        for subroutine in ser[CallGraph.ATTR_SUBROUTINES].values():
            subroutine = _CallGraphSubroutine.deserialize(subroutine)
            callgraph.__subroutines[subroutine.getName()] = subroutine
            
        return callgraph
    
    def getRoot(self):
        return self.__rootSubroutine;
        
    def addSubroutine(self, subroutineName):
        assertType(subroutineName, 'subroutineName', SubroutineName)

        self.__subroutines[subroutineName] = _CallGraphSubroutine(subroutineName);
        if self.__rootSubroutine is None:
            self.__rootSubroutine = subroutineName;
            
    def getAllSubroutineNames(self):
        return self.__subroutines.keys(); 
            
    def getAllModuleNames(self):
        modules = set()
        for subroutineName in self.__subroutines:
            moduleName = subroutineName.getModuleName()
            if moduleName is not None:
                modules.add(moduleName)
        return modules
        
    def addCall(self, callerName, calleeName, lineNumber, discriminator):
        assertType(callerName, 'callerName', SubroutineName)        
        assertType(calleeName, 'calleeName', SubroutineName)        
        assertType(lineNumber, 'lineNumber', int)
        if not callerName in self:
            raise ValueError("Caller subroutine not found in CallGraph: " + callerName);
        
        self.__subroutines[callerName].addCall(calleeName, lineNumber, discriminator)
        
        
    def getCallees(self, callerName):
        assertType(callerName, 'callerName', SubroutineName)     
        if not callerName in self:
            raise ValueError("Caller subroutine not found in CallGraph: " + str(callerName));
        return self.__subroutines[callerName].getCallees()
        
    def findCalleeBySimpleName(self, calleeSimpleName, callerFullName):
        assertType(calleeSimpleName, 'calleeSimpleName', str)   
        assertType(callerFullName, 'callerFullName', SubroutineName)     
        if not callerFullName in self:
            raise ValueError("Caller subroutine not found in CallGraph: " + str(callerFullName))
        
        subroutine = self.__subroutines[callerFullName]
        if subroutine is not None:
            return subroutine.findCalleeBySimpleName(calleeSimpleName)
        
        return None;    
    
    def findNextCalleesFromLine(self, callerName, lineNumber):
        assertType(callerName, 'callerName', SubroutineName)        
        assertType(lineNumber, 'lineNumber', int)
        
        subroutine = self.__subroutines[callerName]
        if subroutine is not None:
            return subroutine.findNextCalleesFromLine(lineNumber)
        
        return []
    
    def getSortedCallees(self, callerName):
        assertType(callerName, 'callerName', SubroutineName)     
        if not callerName in self:
            raise ValueError("Caller subroutine not found in CallGraph: " + str(callerName));
        return self.__subroutines[callerName].getSortedCallees()
        
    def getAllCalls(self):
        calls = set()
        for subroutine in self.__subroutines.values():
            calls.update(subroutine.getCalls())
        return calls
 
    def extractSubgraph(self, subrootName):
        assertType(subrootName, 'subrootName', SubroutineName)
        if not subrootName in self:
            raise ValueError("Subroutine not found in CallGraph: " + str(subrootName));
        
        subgraph = CallGraph();
        subgraph.__rootSubroutine = subrootName;
        self.__fillSubgraphRecursive(subgraph, subrootName)
            
        return subgraph;
    
    def __fillSubgraphRecursive(self, subgraph, subroutineName):
        subroutine = self.__subroutines[subroutineName]
        subgraph.__subroutines[subroutineName] = subroutine
        for callee in subroutine.getCallees():
            if callee not in subgraph:
                self.__fillSubgraphRecursive(subgraph, callee) 
