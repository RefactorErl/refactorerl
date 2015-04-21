module RefErl.BufferServer.Interpreter 
	(Command(..), interpret) where

import qualified Data.List as L
import Control.Monad
import Data.Sequence
import Control.Applicative ((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import System.IO
import Text.Parsec.Language
import qualified Data.Map as M

import RefErl.BufferServer.Data
import RefErl.BufferServer.Data.Buffer
import RefErl.BufferServer.Undo

data Command = Load BufferState String 
			 | LoadEdited String String
			 | Redo String Int
			 | Undo String Int
			 | Save String 
			 | BeginUpdate 
			 | EndUpdate 
			 | List 
			 | Open String
			 | Print String
			 | Drop String
			 | Close String
			 | RefacType String
			 | Rename BufferState String String
			 | Quit
		deriving Show

data Paths = Paths [String] deriving Show

def = emptyDef { reservedNames = ["load-closed", "load-edited", "load-opened", "redo", "undo", 
								  "save", "list", "print", "open", "drop", 
								  "quit", "close","begin-update", "refac-type",
								  "end-update", "rename-opened",  "rename-closed"]
			   }
               

TokenParser { parens = m_parens
            , identifier = m_identifier
            , reserved = m_reserved
            , semiSep1 = m_semiSep1
            , whiteSpace = m_whiteSpace } = makeTokenParser def

cmdParser :: Parser Command
cmdParser =  do { m_reserved "load-opened"
 				; p <- pathParser
				; return (Load Opened p)
 				}
		 <|> do { m_reserved "load-closed"
 				; p <- pathParser
				; return (Load Closed p)
 				}
		 <|> do { m_reserved "load-edited"
 				; bp <- pathParser
				; char ' '
 				; op <- pathParser
				; return (LoadEdited bp op)
 				}
		 <|> do { m_reserved "undo"
		 		; p <- pathParser
				; char ' '
				; n <- many digit
				; return (Undo p (read n))
		 		}
		 <|> do { m_reserved "redo"
		 		; p <- pathParser
				; char ' '
				; n <- many digit
				; return (Redo p (read n))
		 		}
		<|> do { m_reserved "refac-type"
		 		; p <- pathParser
				; return (RefacType p)
		 		}
		 <|> do { m_reserved "save"
		 		; p <- pathParser
				; return (Save p)
		 		}
		 <|> do { m_reserved "drop"
		 		; p <- pathParser
				; return (Drop p)
		 		}
		<|> do { m_reserved "open"
		 		; p <- pathParser
				; return (Open p)
		 		}
		 <|> do { m_reserved "close"
		 		; p <- pathParser
				; return (Close p)
		 		}
		 <|> do { m_reserved "rename-opened"
		 		; o <- pathParser
				; char ' '
		 		; n <- pathParser
				; return (Rename Opened o n)
		 		}
		 <|> do { m_reserved "rename-closed"
		 		; o <- pathParser
				; char ' '
		 		; n <- pathParser
				; return (Rename Closed o n)
		 		}
		 <|> do { m_reserved "list"
				; return List
		 		}
		 <|> do { m_reserved "begin-update"
				; return BeginUpdate
		 		}
		 <|> do { m_reserved "end-update"
				; return EndUpdate
		 		}
		 <|> do { m_reserved "print"
		 		; p <- pathParser
				; return (Print p)
		 		}
		 <|> do { m_reserved "quit"
				; return Quit
		 		}

pathParser = do 
	char '"'
	x <- many (noneOf "\"")
	char '"'
	return x

pathsParser = sepBy pathParser (string " ")

-- | értelmezi a szöveges parancsot
interpret cmd st = case parse cmdParser "" cmd of
	Left err -> Left err 
	Right ans -> Right $ doCmd ans st

doCmd cmd st = case cmd of
	Load bst p      -> Right $ do
		readBuffer st p bst 
	LoadEdited bp op      -> Right $ do
		(st', rs) <- readEditedBuffer st bp op 
		let ufs = lastUndoNamesFor (buffers st') [op]
		let rfs = lastRedoNamesFor (buffers st') rs
		putStrLn $ show ([]::[String]) ++ ";" ++ ufs ++ ";" ++ rfs
		hFlush stdout
		return $ st' 	
	BeginUpdate -> Right $ return $ beginUpdate st 	
	EndUpdate   -> Right $ do
		let (st', us, rs) = endUpdate st
		if not $ (L.null rs && L.null us) then do
			let ufs = lastUndoNamesFor (buffers st') us
			let rfs = lastRedoNamesFor (buffers st') rs
		 	putStrLn $ show ([]::[String]) ++ ";" ++ ufs ++ ";" ++ rfs
			hFlush stdout
		 else do
			putStrLn $ show ([]::[String]) ++ ";" ++  ";"
			hFlush stdout
		return $ st' 	
	Undo p n	    ->  Right $ do
		(st', sfs, ofs, cfs, orns, crns) <- undo st p n
		--appendFile "./log.txt"  $ "renames: " ++ show orns ++ show crns ++ "\n"
		--appendFile "./log.txt"  $ "renames: " ++ showRenamings (renamings st) orns ++ showRenamings (renamings st) crns ++ "\n"
		let st'' = renameBuffers st' orns crns
		--appendFile "./log.txt"  $ "renames': " ++ showRenamings (renamings st'') orns ++ showRenamings (renamings st'') crns ++ "\n"
		--mapM_ (renameBufferFile (renamings st)) orns
		mapM_ (saveBuffer st') $ sfs ++ ofs
		mapM_ (renameBufferFile (renamings st)) crns
		if not $ (L.null sfs && L.null ofs && L.null cfs)
		 then do
		 	let ufs = lastUndoNamesFor (buffers st') $ sfs ++ ofs ++ cfs
			let rfs = lastRedoNamesFor (buffers st') $ sfs ++ ofs ++ cfs
		 	putStrLn $ show sfs ++ ";" ++ ufs ++ ";" ++ rfs ++ ";" ++ (showRenamings (renamings st) orns) ++ ";" ++ (showRenamings (renamings st) crns)
			hFlush stdout
		 else do
			putStrLn $ show ([]::[String]) ++ ";" ++  ";"  
			hFlush stdout
		--print fs
		return st''
	Redo p n	    -> Right $ do
		(st', sfs, ofs, cfs, orns, crns) <- redo st p n
		--appendFile "./log.txt"  $ "renames: " ++ showRenamings (renamings st) orns ++ showRenamings (renamings st) crns ++ "\n"
		let st'' = renameBuffers st' orns crns
		mapM_ (saveBuffer st') $ sfs ++ ofs
		--mapM_ (renameBufferFile (renamings st)) orns
		mapM_ (renameBufferFile (renamings st)) crns
		if not $ (L.null sfs && L.null ofs && L.null cfs)
		 then do
		 	let ufs = lastUndoNamesFor (buffers st') $ sfs ++ ofs ++ cfs
			let rfs = lastRedoNamesFor (buffers st') $ sfs ++ ofs ++ cfs
		 	putStrLn $ show sfs ++ ";" ++ ufs ++ ";" ++ rfs ++ ";" ++ (showRenamings (renamings st) orns) ++ ";" ++ (showRenamings (renamings st) crns)
			hFlush stdout
		 else do
			putStrLn $ show ([]::[String]) ++ ";" ++  ";"  
			hFlush stdout
		--print fs
		return st''
	RefacType p -> Right $ do
		return $ readRefacType st p
	Print p     -> Right $ printBuffer st p >> hFlush stdout  >> return st
	List 	    -> Right $ do 
		print $ map fileName (openedBufs $ buffers st)
		--print $ M.keys (closedBufs $ buffers st)
		hFlush stdout
		return st
	Rename bst o n  -> 
		Right $ return $ renameBuffer st o n bst
	Drop p     -> 
		Right $ return $ dropBuffer st p 
	Open p     -> Right $ do 
		(st', us, rs) <- createBuffer st p 
		if not (L.null rs && L.null us)
		 then do
		 	let ufs = lastUndoNamesFor (buffers st') us
			let rfs = lastRedoNamesFor (buffers st') rs
			putStrLn $ show ([]::[String]) ++ ";" ++ ufs ++ ";" ++ rfs
			hFlush stdout
		 else do
			putStrLn $ show ([]::[String]) ++ ";" ++ ";"
			hFlush stdout
		return $ st' 	
	Close p     -> 
		Right $ return $ closeBuffer st p 
	Quit 	    -> Left "stop"

