module Main (main) where

import RefErl.BufferServer.Data
import RefErl.BufferServer.Data.Buffer 
import RefErl.BufferServer.Undo
import RefErl.BufferServer.Interpreter

import qualified Data.IntMap as IM
import qualified Data.Map as M
import System.IO

initState :: State
initState = State
	{ refactorings  = Refactorings IM.empty 
	, buffers       = Buffers [] M.empty
	, autoIncrement = 1
	, refacType		= []
	, renamings		= IM.empty
	}

loop :: State -> IO ()
loop state = do
	--appendFile "./log.txt" $ "before: " ++ show state ++ "\n"
	cmd <- getLine
	--appendFile "./log.txt" $ "command: " ++ cmd ++ "\n"
	case interpret cmd state of
		Left err -> do 
			print err
			hFlush stdout
			loop state
		Right s -> case s of
			Left _   -> return () 
--print "buffer-server stopped"
			Right s' -> do 
				state' <- s'
				--appendFile "./log.txt" $ "after: " ++ show state' ++ "\n"
				loop state'

--encoding = utf8

main :: IO ()
main = do
	--writeFile "./log.txt" ""
	loop initState
	--config <- getConfigData
	--initState' = initState { configData = config }
	--hSetEncoding stdin Data.encoding
	--hSetEncoding stdout Data.encoding
	--loop initState'
	
