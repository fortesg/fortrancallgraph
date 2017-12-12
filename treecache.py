from assertions import assertType
import os
from supertypes import CallGraphBuilder
from source import SubroutineFullName
import json
from callgraph import CallGraph
from json.encoder import JSONEncoder
from json.decoder import JSONDecoder

class CachedAssemblerCallGraphBuilder(CallGraphBuilder):
    
    FILE_SUFFIX = '.tree'
    
    def __init__(self, cacheDir, graphBuilder):
        assertType(graphBuilder, 'graphBuilder', CallGraphBuilder)
        
        self.__cacheDir = cacheDir;
        self.__graphBuilder = graphBuilder

    def buildCallGraph(self, rootSubroutine, clear = False):
        assertType(rootSubroutine, 'rootSubroutine', SubroutineFullName)
        assertType(clear, 'clear', bool)
        
        callgraph = None
        if clear:
            self.__clearGraph(rootSubroutine)
        else:
            callgraph = self.__loadGraph(rootSubroutine)
            
        if callgraph is None:
            callgraph = self.__graphBuilder.buildCallGraph(rootSubroutine);
            self.__saveGraph(callgraph)
        
        return callgraph
    
    def getModuleFilePath(self, moduleName):
        assertType(moduleName, 'moduleName', str)
            
        return self.__graphBuilder.getModuleFilePath(moduleName)
    
    def __getCacheFilePath(self, subroutineName):
        return os.path.join(self.__cacheDir, str(subroutineName) + CachedAssemblerCallGraphBuilder.FILE_SUFFIX)
    
    def __clearGraph(self, subroutineName):
        cacheFilePath = self.__getCacheFilePath(subroutineName)
        if os.path.isfile(cacheFilePath):
            os.remove(cacheFilePath) 
    
    def __loadGraph(self, subroutineName):
        cacheFilePath = self.__getCacheFilePath(subroutineName)
        if not os.path.isfile(cacheFilePath):
            return None 
        
        cacheFile = open(cacheFilePath)
        callgraph = json.load(cacheFile, cls=CallGraphJSONDecoder)
        
        cacheTime = os.path.getmtime(cacheFilePath)
        for module in callgraph.getAllModuleNames():
            moduleFilePath = self.getModuleFilePath(module)
            if moduleFilePath is not None and os.path.getmtime(moduleFilePath) > cacheTime:
                return None
        
        return callgraph 
    
    def __saveGraph(self, callgraph):
        subroutineName = callgraph.getRoot()

        if not os.path.exists(self.__cacheDir):
            os.makedirs(self.__cacheDir) 

        cacheFilePath = self.__getCacheFilePath(subroutineName)
        cacheFile = open(cacheFilePath, 'w')
        json.dump(callgraph, cacheFile, cls=CallGraphJSONEncoder)
        cacheFile.close()
 
class CallGraphJSONEncoder(JSONEncoder):
     
    def default(self, o):
         
        if isinstance(o, CallGraph):
            return o.serialize()
        else:
            return JSONEncoder.default(self, o) 
 
class CallGraphJSONDecoder(JSONDecoder):
     
    def decode(self, s):
        ser = super(CallGraphJSONDecoder, self).decode(s) 
        return CallGraph.deserialize(ser)
                