module RefErl.BufferServer.Data.Buffer where

import Prelude hiding (readFile, writeFile)
import Data.Algorithm.Diff
import System.IO hiding (openFile, hClose, writeFile)
import qualified System.IO as SysIO(openFile, hClose)
import qualified System.IO.Strict as SIO (hGetContents)
import System.Directory (doesFileExist)
import System.Directory as SD (renameFile) 
import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe
import System.Directory
import Text.Regex.Posix
import Data.List.Split

import RefErl.BufferServer.Undo
import RefErl.BufferServer.Data.Rope
import RefErl.BufferServer.Data


readRefacType :: State -> String -> State
readRefacType s n = s { refacType = n }


-- | kiírja a megadott nevű buffer tartalmát a standard kimenetre 
printBuffer :: State -> String -> IO ()
printBuffer s fn = 
	case L.find ((== fn) . fileName) (openedBufs $ buffers s) of
		--Nothing -> case L.find ((== fn) . fileName) (closedBufs $ buffers s) of
			Nothing -> print $ "Can't find file: " ++ fn
			Just b  -> print $ toString $ content b
		--Just b  -> print $ toString $ content b

-- | elmenti a megadott nevű fájlt
saveBuffer :: State -> String -> IO ()
saveBuffer s fn = do 
	case L.find ((== fn) . fileName) (openedBufs $ buffers s) of
		Nothing -> return () 
		Just b  -> writeFile (fileName b) (content b)

closeBuffer :: State -> String -> State
closeBuffer s fn = s' where
	s' = s { buffers = buffers' }
	buffers' = bs { openedBufs = bs', closedBufs = (M.fromList $ map (\x -> (fileName x, x)) bs0') `M.union` (closedBufs $ bs) }
	
	noRefac = IM.null $ IM.filter (L.any (== fn)) $ openedRefacs rs
	bs0' = if noRefac
		then []
		else bs0
	rs = refactorings s
	bs = buffers s
	(bs0, bs') = L.partition ((== fn) . fileName) (openedBufs $ buffers s)

-- | eldobja a fájlt a bufferek listájából
dropBuffer :: State -> String -> State
dropBuffer s fn = s' where
	s' = s { buffers = buffers', refactorings = refactorings' }
	refactorings' = (refactorings s) { openedRefacs = ors' }
	buffers' = bs { openedBufs = bs' }

	ors' = IM.map (L.filter (/= fn)) rs
	rs = openedRefacs $ refactorings s
	bs = buffers s 
	(b0, bs') = L.partition ((== fn) . fileName) (openedBufs $ buffers s)


-- | átnevezi a megadott fájlt
renameBuffer :: State -> String -> String -> BufferState -> State
renameBuffer s ofn nfn bst = s' where
	s' = s { buffers = renameBuffer' (buffers s) (autoIncrement s) ofn nfn bst
		   , refactorings = renameRefacs (refactorings s) ofn nfn bst 
		   , renamings = IM.insert (autoIncrement s) (nfn, ofn) (renamings s)
		   }

renameBuffers :: State -> [Int] -> [Int] -> State
renameBuffers s orns crns = s' where
	s' = s { buffers = bs'
		   , refactorings = rs
		   , renamings = rns}
	orns' = zip orns $ mapMaybe (\x -> IM.lookup x (renamings s)) orns
	crns' = zip crns $ mapMaybe (\x -> IM.lookup x (renamings s)) crns
	bs = foldl (\y (x0, (x1, x2)) -> renameBuffer' y x0 x1 x2 Opened) (buffers s) orns' 
	bs' = foldl (\y (x0, (x1, x2)) -> renameBuffer' y x0 x1 x2 Closed) bs crns' 
	rs = foldl (\a (_, (x, y)) -> renameRefacs a x y Opened) (refactorings s) $ orns' ++ crns'
	rns = (IM.fromList (map (\(i, (x, y)) -> (i, (y, x))) $ orns' ++ crns')) `IM.union` renamings s

renameBufferFile :: IM.IntMap (String, String) -> Int -> IO ()
renameBufferFile rns n = case IM.lookup n rns of
	Just (ofn, nfn) -> SD.renameFile ofn nfn
	Nothing -> return ()

showRenamings :: IM.IntMap (String, String) -> [Int] -> String
showRenamings rns xs = if L.null frns
	then frns 
	else tail frns
  where 
	frns = foldr (\(x, y) str -> "," ++ str ++ "\"" ++ x ++ "\"->\"" ++ y ++ "\"") "" rns'
  	rns' = mapMaybe (\x -> IM.lookup x rns) xs

--renameBuffers :: Buffers -> Refactorings -> IM.IntMap (String, String) -> [Int]  -> (Buffers, Refactorings, IM.IntMap (String, String))
--renameBuffers bs rs rns xs = 

	
renameRefacs :: Refactorings -> String -> String -> BufferState -> Refactorings
renameRefacs rs@(Refactorings ors) ofn nfn Closed = rs' where
	rs' = rs
renameRefacs rs@(Refactorings ors) ofn nfn Opened = rs' where
	rs' = rs { openedRefacs = ors' }
	ors' = IM.map renameRefac' ors
	renameRefac' r = renameRefac r ofn nfn

renameRefac :: [String] -> String -> String -> [String]
renameRefac fs ofn nfn = fs' 
 where
 	fs' = if L.null fs0 
		then fs1
		else nfn : fs1
	(fs0,fs1) = L.partition (== ofn) fs

renameBuffer' :: Buffers -> Int -> String -> String -> BufferState -> Buffers
renameBuffer' bs@(Buffers xs _ys) n ofn nfn Opened = bs' where
	bs' = bs { openedBufs = renameBuffer'' {-n-} xs ofn nfn}
renameBuffer' bs@(Buffers _xs ys) n ofn nfn Closed = bs' where
	bs' = bs { closedBufs = renameBufferMap ys {-n-} ofn nfn}

renameBufferMap :: M.Map String Buffer -> String -> String -> M.Map String Buffer
renameBufferMap xs ofn nfn = xs' where
	xs' = M.insert nfn xs0' xs1 
	(xs0, xs1) = M.partition ((== ofn) . fileName) xs
	xs0' = xs0'' { fileName = nfn }
	Just xs0'' = M.lookup ofn xs0

renameBuffer'' :: [Buffer] -> String -> String -> [Buffer]
renameBuffer'' xs ofn nfn = xs' where
	xs' = xs0':xs1 
	([xs0], xs1) = L.partition ((== ofn) . fileName) xs
	xs0' = xs0 { fileName = nfn }


readEditedBuffer :: State -> String -> String -> IO (State, [String])
readEditedBuffer s bfn ofn = do
	h <- SysIO.openFile bfn ReadMode
	--hSetEncoding h utf8
	hSetEncoding h latin1
	str <- SIO.hGetContents h
	(bs, ids, p) <- readEditedBuffer' (buffers s) ofn str (autoIncrement s)
	let ai' = if p 
		then autoIncrement s + 1
		else autoIncrement s
	SysIO.hClose h
	case ids of
		[] -> return (s { buffers = bs , autoIncrement = ai'}, [])
		_  -> return (s { buffers = bs', refactorings = (refactorings s) { openedRefacs = ors' }, autoIncrement = ai' }, fs) where
		 bs' = clearRedos bs ids (openedRefacs $ refactorings s)
		 --bs'' = clearRedos bs' ids (openedRefacs $ refactorings s) Closed
		 ors' = IM.filterWithKey (\x _ -> not $ any (== x) ids) $ openedRefacs$ refactorings s
		 fs = L.map fileName $ L.filter (L.null . getRedos . undos) $ openedBufs bs'

readEditedBuffer' bs@(Buffers xs ys) fn str i = return (bs', ids, p) where
	bs' = bs { openedBufs = xs' }
	(xs', ids, p) = addEditedFile xs fn str i


createBuffer :: State -> String -> IO (State, [String], [String])
createBuffer s fn = do
	exists <- doesFileExist fn
	str <- if exists 
		then do
			h <- SysIO.openFile fn ReadMode
			--hSetEncoding h utf8			
			hSetEncoding h latin1
			str' <- SIO.hGetContents h
			SysIO.hClose h
			return str'
		else return ""
	let (ys0,ys1) = M.partition ((== fn) . fileName) $ closedBufs $ buffers s
	let bs
		| M.null ys0  = (buffers s) { openedBufs = obs } 
		| otherwise = (buffers s) { openedBufs = ( y { content = fromString str  }) : (openedBufs $ buffers s), closedBufs = ys1 } where
			obs = fst $ openFile (openedBufs $ buffers s) fn str
			Just y = M.lookup fn ys0
	let us = L.nub $ fn : (L.map fileName $ L.filter (L.null . getUndos . undos) $ openedBufs bs)
	let rs = L.nub $ fn : (L.map fileName $ L.filter (L.null . getRedos . undos) $ openedBufs bs)
	return (s { buffers = bs }, us, rs)


readBuffer :: State -> String -> BufferState ->  IO State
readBuffer s fn bst = do
	exists <- doesFileExist fn
	str <- if exists 
		then do
			h <- SysIO.openFile fn ReadMode
			hSetEncoding h latin1
			--hSetEncoding h utf8
			str' <- SIO.hGetContents h
			SysIO.hClose h
			return str'
		else return ""
	{-(bs, ids) <- if (bst == Closed) && (M.null $ M.filter (L.any (== fn)) $ openedRefacs $ refactorings s)
		then return (buffers s, [])
		else 
	-}
	let cn = refacType s
	(bs, ids) <- readBuffer' (buffers s) fn str (autoIncrement s) bst cn
	let rs' = if L.length fns > 0 
			then IM.insert (autoIncrement s) fns rs1  
			else rs1 where

		 fns = if not $ IM.null rs0
		 	then fn : (snd $ head $ IM.toList rs0)
			else []
		 (rs0, rs1) = IM.partitionWithKey (\x _ -> x == autoIncrement s) $ openedRefacs $ refactorings s

	case ids of
		[] -> return s { buffers = bs, refactorings = (refactorings s) { openedRefacs = rs'} }
		_  -> return s { buffers = bs', refactorings = (refactorings s) { openedRefacs = ors' } } where
		 bs' = clearRedos bs ids (openedRefacs $ refactorings s) 
		 ors' = IM.filterWithKey (\x _ -> not $ any (== x) ids) rs'

readBuffer' bs@(Buffers xs ys) fn str i Opened cn = return (bs', ids) where
	bs' = bs { openedBufs = xs' }
	(xs', ids) = addFile xs fn str i cn
readBuffer' bs@(Buffers xs ys) fn str i Closed cn
 | not $ L.null xs0 = do 
	let clearContent b = b { content = fromString "" }
	let xs0' = M.fromList $ map (\x -> (fileName x, clearContent x)) xs0
 	let bs1 = bs { openedBufs = xs1, closedBufs = xs0' `M.union` ys }
	return (bs1, []) 
 | otherwise = do 
	let fns = splitOn "/" fn
 	let path = concatMap (++ "/") $ init fns
 	fs <- getDirectoryContents path >>= return . filter (=~ (last fns ++ ".bak."))
	let y = maximum fs
	h <- SysIO.openFile (path ++ y) ReadMode
	--hSetEncoding h utf8
	hSetEncoding h latin1
	str2 <- SIO.hGetContents h
	SysIO.hClose h
	let (ys', ids) = addClosedFile (map snd $ M.toList ys) fn str str2 i cn
	let bs2 = bs { closedBufs = (M.fromList $ map (\x -> (fileName x, x)) ys') }
 	return (bs2, ids) where
	(xs0, xs1) = L.partition ((== fn) . fileName) xs

clearRedos bs@(Buffers xs ys) ids rs = bs' where
	bs' = bs { openedBufs = xs', closedBufs = ys' }
	ys' = ys0' `M.union` ys1
	ys0' = M.map (\x -> x { undos = clearRedo $ undos x }) ys0
	(ys0, ys1) = M.partition (\x -> any (== fileName x) fs) ys
	fs = L.nub $ L.concat $ mapMaybe (\x -> IM.lookup x rs) ids
--clearRedos bs@(Buffers xs _ys) ids rs Opened = bs' where
--	bs' = bs { openedBufs = xs' }
	xs' = xs0' ++ xs1
	xs0' = map (\x -> x { undos = clearRedo $ undos x }) xs0
	(xs0, xs1) = L.partition (\x -> any (== fileName x) fs) xs
--	fs = L.nub $ L.concat $ mapMaybe (\x -> M.lookup x rs) ids

clearRedo (URList us _) = URList us [] 

-- | jelzi egy refaktorálás kezdetét
-- előkészíti az aktuális azonosítónak a refaktorálások bejegyzését
beginUpdate :: State -> State
beginUpdate s = s { refactorings = refactorings', autoIncrement = autoIncrement s }
 where
 	refactorings' = (refactorings s) { openedRefacs = ors' } 
	ors' = IM.insert (autoIncrement s) rs0' rs1
	(rs0,rs1) = IM.partitionWithKey (\x _ -> x == autoIncrement s) (openedRefacs $ refactorings s)
	rs0' = if IM.null rs0 || L.null (snd $ head $ IM.toList rs0)
		then []
		else snd $ head $ IM.toList rs0 


-- | jelzi egy refaktorálás végét
-- csak akkor hagyja meg az aktuális refaktorálást a listában, ha az több fájlt érintett
endUpdate :: State -> (State, [String], [String])
endUpdate s = (s { refactorings = refactorings', autoIncrement = ai' }, us, rs)
 where
	refactorings' = (refactorings s) { openedRefacs = ors' }
	ors' = (IM.map L.nub rs0') `IM.union` rs1
	(rs0,rs1) = IM.partitionWithKey (\x _ -> x == autoIncrement s) (openedRefacs $ refactorings s)
	noRefac = IM.null rs0 || {-1 == L.length (snd $ head $ IM.toList rs0) ||-} L.null (snd $ head $ IM.toList rs0)
	rs0' = if noRefac
		then IM.empty
		else rs0
	ai' = if IM.null rs0 || L.null (snd $ head $ IM.toList rs0) 
	--ai' = if noRefac 
		then autoIncrement s
		else autoIncrement s + 1
	rs = if IM.null rs0 || L.null (snd $ head $ IM.toList rs0) 
		then []
		else L.map fileName $ L.filter (L.null . getRedos . undos) $ openedBufs $ buffers s
	us = if IM.null rs0
		then []
		else snd $ head $ IM.toList rs0


-- | hozzáadja a fájlt a bufferek listájához (első megnyitás a szerkesztőben)
openFile :: [Buffer] -> String -> String -> ([Buffer], [Int])
openFile [] fn cs   = ([Buffer {fileName = fn, content = fromString cs, undos = URList [] []}], [])
openFile (x:xs) fn cs 
 | fileName x == fn = (x : xs, [])
 | otherwise        = (x : xs', [])
  where 
	(xs', ids) = openFile xs fn cs


-- | frissíti a szerkesztett fájl tartalmát a bufferek listájában
addEditedFile :: [Buffer] -> String -> String -> Int -> ([Buffer], [Int], Bool)
addEditedFile [] fn cs _     = ([], [], False)
addEditedFile (x:xs) fn cs ai 
 | fileName x == fn = (x' : xs, ids, True)
 | otherwise        = (x : xs', ids', p)
  where 
 	x' = x { content = content', undos = undos' }
	content' = L.foldl apply (content x) us
	apply cs u = applyUpdate u cs
	undos'
	 | L.null updates' = undos x 
	 | otherwise       = addChange reversed (undos x)
	reversed = AtomicChange ai updates' "edit"
	updates' = L.reverse $ map reverseUpdate us 
	us = getUpdates (getGroupedDiff cs (toString $ content x))
	ids 
	 | L.null updates' =  []
	 | otherwise 	   = map changeId $ getRedos $ undos x 
	(xs', ids', p) = addEditedFile xs fn cs ai

-- | hozzáadja a fájlt, vagy frissíti a tartalmát a bufferek listájában
addFile :: [Buffer] -> String -> String -> Int -> String -> ([Buffer], [Int])
addFile [] fn cs _ _    = ([Buffer {fileName = fn, content = fromString cs, undos = URList [] []}], [])
addFile (x:xs) fn cs ai cn 
 | fileName x == fn = (x' : xs, ids)
 | otherwise        = (x : xs', ids')
  where 
 	x' = x { content = content', undos = undos' }
	content' = L.foldl apply (content x) us
	apply cs u = applyUpdate u cs
	undos'
	 | L.null updates' = undos x 
	 | otherwise       = addChange reversed (undos x)
	reversed = AtomicChange ai updates' cn
	updates' = L.reverse $ map reverseUpdate us 
	us = getUpdates' (filter (not . L.null . snd) $ map (\(x,y) -> (x, concat y)) $ getGroupedDiff (splitOnWs cs) (splitOnWs $ toString $ content x))
	--us = getUpdates (getGroupedDiff cs (toString $ content x))
	--us = getUpdates (getGroupedDiff cs (toString $ content x))
	ids 
	 | L.null updates' =  []
	 | otherwise 	   = map changeId $ getRedos $ undos x 
	(xs', ids') = addFile xs fn cs ai cn

-- | hozzáadja a meg nem nyitott fájlt a többi ilyen buffer listájához, kiszámítva az esetleges változásokat
addClosedFile :: [Buffer] -> String -> String -> String -> Int -> String -> ([Buffer], [Int])
addClosedFile [] fn str1 str2 ai cn = ([Buffer {fileName = fn, content = fromString "" , undos = URList [reversed] []}], [])
 where
  reversed = AtomicChange ai updates' cn
  updates' = L.reverse $ map reverseUpdate us 
  us = getUpdates (getGroupedDiff str1 str2)
addClosedFile (x:xs) fn str1 str2 ai cn
 | fileName x == fn = (x' : xs, ids)
 | otherwise        = (x : xs', ids')
  where 
 	x' = x { undos = undos' }
	undos'
	 | L.null updates' = undos x 
	 | otherwise       = addChange reversed (undos x)
	reversed = AtomicChange ai updates' cn
	updates' = L.reverse $ map reverseUpdate us 
	--us = getUpdates (getGroupedDiff str1 str2)
	us = getUpdates' (filter (not . L.null . snd) $ map (\(x,y) -> (x, concat y)) $ getGroupedDiff (splitOnWs str1) (splitOnWs str2))
	ids 
	 | L.null updates' =  []
	 | otherwise 	   = map changeId $ getRedos $ undos x 
	(xs', ids') = addClosedFile xs fn str1 str2 ai cn


extDiff :: [(DI, String)] -> [(DI, String)]
extDiff ((x@(d1,s1)):(y@(d2,s2)):ys)
 | d1 == S && d2 == F || d1 == F && d2 == S = x:y:(extDiff ys)
 | d1 == S && d2 == B = (d1, s1 ++ " "):y:(extDiff ys)
 | d1 == F && d2 == B = (d1, s1 ++ " "):y:(extDiff ys)
 | otherwise = x:(extDiff $ y:ys)
extDiff [x@(d,s)] 
 | d == S || d == F = [(d, " " ++ s)]
 | otherwise = [x] 
extDiff [] = []

getUpdates :: [(DI, String)] -> [Update]
getUpdates ds = getUpdates0 ds 0 0

getUpdates0 :: [(DI,String)] -> Int -> Int -> [Update]
getUpdates0 [] _ _ = []
getUpdates0 ((d,s):xs) p1 p2  
-- új állapot, régi állapot a sorrend
 | d == B = getUpdates0 xs (p1 + L.length s) (p2 + L.length s)
 | d == S = Delete (Point p2) (fromString s) : getUpdates0 xs p1 p2
 | d == F = Insert (Point p1) (fromString s) : getUpdates0 xs (p1 + L.length s) (p2 + L.length s)
 
getUpdates' :: [(DI, String)] -> [Update]
getUpdates' ds = getUpdates0' ds 0 0

getUpdates0' :: [(DI,String)] -> Int -> Int -> [Update]
getUpdates0' [] _ _ = []
getUpdates0' ((d,s):xs) p1 p2  
-- új állapot, régi állapot a sorrend
 | d == B = getUpdates0 xs (p1 + L.length s) (p2 + L.length s)
 | d == S = Delete (Point p2) (fromString s) : getUpdates0 xs p1 p2
 | d == F = Insert (Point p1) (fromString s) : getUpdates0 xs (p1 + L.length s) (p2 + L.length s)

splitOnWs = split (oneOf " \t\n\r\n")
