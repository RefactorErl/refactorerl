module RefErl.BufferServer.Data where

import RefErl.BufferServer.Data.Rope 
import Data.IntMap
import Data.Map
import System.IO

data Buffer = Buffer 
	{ fileName :: String
	, content  :: !Rope
	, undos    :: !URList
	}
	deriving (Show) 

data State = State
	{ refactorings  :: Refactorings
	, buffers       :: Buffers
	, autoIncrement :: !Int
	, refacType		:: !String
	, renamings		:: !(IntMap (String, String))
	--, configData	:: ConfigData
	}
	deriving (Show) 

data Buffers = Buffers
	{ openedBufs :: ![Buffer]
	, closedBufs :: !(Map String Buffer)
	}
	deriving (Show)

data ClosedBuffer = ClosedBuffer 
	{ baseFileName :: String
	, changes	   :: !URList
	}
	deriving (Show)


data BufferState = Opened | Closed
	deriving (Eq, Show)

data Refactorings = Refactorings
	{ openedRefacs :: !(IntMap [String])
	}
	deriving (Show)

data Change = AtomicChange  
	{ changeId   :: Int
	--, changeType :: String 
	, updates    :: ![Update]
	, changeType :: String 
	}
	deriving (Show) 

data URList = URList 
	{ getUndos :: ![Change]
	, getRedos :: ![Change]
	}
	deriving (Show) 

data Update = Insert {updatePoint :: !Point, insertUpdateString :: !Rope} 
            | Delete {updatePoint :: !Point, deleteUpdateString :: !Rope}
              deriving (Show)

data UpdateType = InsertUpdate | DeleteUpdate
	deriving (Eq)

data UndoType = UndoOp | RedoOp
{-data ConfigData = ConfigData
	{ encoding :: TextEncoding
	}
	deriving (Show)
--}
--encoding :: TextEncoding
--encoding = utf8
