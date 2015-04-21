module RefErl.BufferServer.Undo 
--(
--	addChange,
--	applyUpdate,
--	reverseUpdate,
--	undo,
--	redo
--	)
	where

import RefErl.BufferServer.Data
import Prelude hiding (length, writeFile, readFile)
import RefErl.BufferServer.Data.Rope
import Data.Maybe
import Data.List.Utils (genericJoin, replace)
import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Foldable (foldrM)
import qualified Data.IntSet as S

import System.IO hiding (readFile, writeFile)
import qualified System.IO.Strict as SIO (readFile)
import qualified System.IO.UTF8 as SysIO (readFile, writeFile)

import Data.Algorithm.Diff


-- hozzáad egy változást az undo-redo listához
addChange :: Change -> URList -> URList
addChange c (URList xs ys) = URList (c:xs) []

-- | alkalmaz egy módosítást egy fingertree-n
applyUpdate :: Update -> Rope -> Rope
applyUpdate (Insert p s) r = insertChars r s p
applyUpdate (Delete p s) r = deleteChars r p (length s)

-- | invertálja a módosítást
reverseUpdate :: Update -> Update
reverseUpdate (Delete p cs) = Insert p cs
reverseUpdate (Insert p cs) = Delete p cs

groupByN _ _ [] = []
groupByN _ 0 _ = []
groupByN eq n (x:xs) = (x:ys) : groupByN eq (n - 1) zs
	where (ys, zs) = span (eq x) xs

-- | utolsó n módosítás undo-ból
lastUndos :: Buffer -> Int -> [[Change]]
lastUndos b@(Buffer { undos = URList us _ }) n = groupByN sameEditType n $ us 

-- | utolsó n módosítás redo-ból
lastRedos :: Buffer -> Int -> [[Change]]
lastRedos b@(Buffer { undos = URList _ rs }) n = groupByN sameEditType n $ rs 

lastUndoNames :: Buffer -> Int -> [String]
lastUndoNames b n = getUniqueNames (map (getUpdateName UndoOp) (lastUndos b n)) M.empty

lastRedoNames :: Buffer -> Int -> [String]
lastRedoNames b n = getUniqueNames (map (getUpdateName RedoOp) (lastRedos b n)) M.empty

sameEditType :: Change -> Change -> Bool
sameEditType x y = 
	changeType x == "edit" && changeType x == changeType y 
	&& L.length (updates x) == 1 && L.length (updates x) == L.length (updates y)
	&& (updateType $ head $ updates x) == (updateType $ head $ updates y)

updateType :: Update -> String
updateType (Insert _ _) = "insert"
updateType (Delete _ _) = "remove"

getUpdateName :: UndoType -> [Change] -> String
getUpdateName UndoOp [AtomicChange _ us name] 
 | name == "edit" = case us of
 	[u] -> updateType $ reverseUpdate u
 	otherwise -> name
 | otherwise = name
getUpdateName UndoOp (x:xs) = (updateType $ reverseUpdate $ head $ updates x) ++ "s"
getUpdateName RedoOp [AtomicChange _ us name]
 | name == "edit" = case us of
 	[u] -> updateType u
 	otherwise -> name
 | otherwise = name
getUpdateName RedoOp (x:xs) = (updateType $ head $ updates x) ++ "s"

getUniqueNames :: [String] -> (M.Map String Int) -> [String]
getUniqueNames (x:xs) m = case M.lookup x m of
	Just n ->  (x ++ "(" ++ (show n) ++ ")") : (getUniqueNames xs (M.insert x (n + 1) m))
	Nothing -> x : (getUniqueNames xs (M.insert x 1 m))
getUniqueNames [] _ = []

lastUndoNamesFor :: Buffers -> [String] -> String
lastUndoNamesFor (Buffers {openedBufs = xs}) fs = formatNames $ map (\x -> (fileName x, lastUndoNames x 5)) fxs
	where fxs = L.filter (\b -> any (== fileName b) fs) xs

lastRedoNamesFor :: Buffers -> [String] -> String
lastRedoNamesFor (Buffers {openedBufs = xs}) fs = formatNames $ map (\x -> (fileName x, lastRedoNames x 5)) fxs
	where fxs = L.filter (\b -> any (== fileName b) fs) xs

formatNames :: [(String, [String])] -> String
formatNames = filter (\x -> notElem x "()" ) . replace ",[" ":[" . genericJoin "|"

------------------------------------------------------------------
-- undo
------------------------------------------------------------------

-- | visszalép a megadott fájl állapotában, magával húzva a többi fájlban is az érintett változás(okat)t
undo :: State -> String -> Int -> IO (State, [String], [String], [String], [Int], [Int])
undo s fn n = do 
	(buffers', sfs, ofs, cfs, orns, crns) <- undo0 (buffers s) fn n (refactorings s) (renamings s)
 	let s' = s { buffers = buffers' }

	return (s', sfs, ofs, cfs, orns, crns)
	
renameFile ((x0, x1):xs) ys fn
  | x0 == fn = OpenedRenamed x0
  | otherwise = renameFile xs ys fn
renameFile [] ((y0, y1):ys) fn
  | y0 == fn = ClosedRenamed y0
  | otherwise = renameFile [] ys fn
renameFile [] [] fn = Simple fn
	
data FileType = OpenedRenamed String
			  | ClosedRenamed String
			  | Simple String

separateFilenames (x:xs) os cs ss = case x of
  OpenedRenamed f -> separateFilenames xs (f:os) cs ss
  ClosedRenamed f -> separateFilenames xs os (f:cs) ss
  Simple f 		  -> separateFilenames xs os cs (f:ss)
separateFilenames [] os cs ss = (os, cs, ss)

undo0 :: Buffers -> String -> Int -> Refactorings -> IM.IntMap (String, String) -> IO (Buffers, [String], [String], [String], [Int], [Int])
undo0 bs@(Buffers xs ys) fn 0 rs@(Refactorings ors) rns  = do
	let ([x], xs'') = L.partition (\b -> fileName b == fn) xs
	let cId = lastUId (undos x)
	--let (x', x''') = if IM.member cId rns 
	--	then renameBufferData
	--	else (x, xs'')
	if IM.member cId ors
		  then do 
		  	(bs0, ids0, ids1) <- undoUntilC fn (S.empty) [] [] [cId] bs rs
			--let ufs = map (\x -> (x, lastUndoNames x 5)) fs0
			--let ufs = L.map fileName $ L.filter (L.null . getUndos . undos) $ openedBufs bs0
			let orns = L.filter (\x -> IM.member x rns) ids0
			let crns = L.filter (\x -> IM.member x rns) ids1
		  	let fs0 = L.foldl L.union [] $ mapMaybe (`IM.lookup` ors) $ ids0 ++ ids1
			let orns' = mapMaybe (\x -> IM.lookup x rns) orns
			let crns' = mapMaybe (\x -> IM.lookup x rns) crns
			let fs' = map (renameFile orns' crns') fs0
			let (ofs, cfs, sfs) = separateFilenames fs' [] [] []
		  	return (bs0, sfs, ofs, cfs, orns, crns)
		  else do
		  	let bs' = Buffers (undoB x : xs'') ys
			--let ufs = L.map fileName $ L.filter (L.null . getUndos . undos) $ openedBufs bs'
			let orns = if IM.member cId rns
				then [cId]
				else []
			return (bs', [fileName x], [], [], [], [])
undo0 bs@(Buffers xs ys) fn n rs@(Refactorings ors) rns = do
	let ([x], xs'') = L.partition (\b -> fileName b == fn) xs
	let cId = lastUId (undos x)
	let cIds = L.map changeId $ last $ lastUndos x n
	--appendFile "./log.txt"  $ "rns: " ++ show rns ++ "\n"
	--appendFile "./log.txt"  $ "cids: " ++ show cIds ++ "\n"
	if not $ L.null cIds && (L.length cIds > 1 || IM.member (head cIds) ors || L.head cIds /= cId)
		  then do 
		  	(bs0, ids0, ids1) <- undoUntilC fn (S.empty) [] [] cIds bs rs
			--let cfs = map fileName xs
		  	let fs0 = L.foldl L.union [] $ mapMaybe (`IM.lookup` ors) $ ids0 ++ ids1
			--let ufs = map (\x -> (x, lastUndoNames x 5)) fs0
			--let ufs = L.map fileName $ L.filter (L.null . getUndos . undos) $ openedBufs bs0
			let fs = L.nub $ fn : fs0
			let orns = L.filter (\x -> IM.member x rns) ids0
			let crns = L.filter (\x -> IM.member x rns) ids1
			--appendFile "./log.txt"  $ "ids0: " ++ show ids0 ++ "\n"
			--appendFile "./log.txt"  $ "ids1: " ++ show ids1 ++ "\n"
			--appendFile "./log.txt"  $ "orns: " ++ show orns ++ "\n"
			--appendFile "./log.txt"  $ "crns: " ++ show crns ++ "\n"
			let orns' = mapMaybe (\x -> IM.lookup x rns) orns
			let crns' = mapMaybe (\x -> IM.lookup x rns) crns
			let fs' = map (renameFile orns' crns') fs
			let (ofs, cfs, sfs) = separateFilenames fs' [] [] []
		  	return (bs0, sfs, ofs, cfs, orns, crns)
		  else do
		  	let bs' = Buffers (undoB x : xs'') ys
			let orns = if IM.member cId rns
				then [cId]
				else []
			--let ufs = L.map fileName $ L.filter (L.null . getUndos . undos) $ openedBufs bs'
			return (bs', [fileName x], [], [], [], [])


-- | visszavonja az utolsó változtatást a bufferben
undoB :: Buffer -> Buffer
undoB b@(Buffer { undos = URList [] _ }) = b
undoB b = b { undos = undos', content = content' }
 where
 	content' = L.foldl apply (content b) u 
	apply cs u = applyUpdate u cs
	reversed = L.reverse $ map reverseUpdate u
	undos' = URList us ((AtomicChange i reversed name):rs)
 	(URList (AtomicChange i u name : us) rs) = undos b 

--undoClosedB b= 

-- | bejárja a konfliktus miatt visszavonandó változást
undoUntilC fn cs ocs ccs [] bs@(Buffers xs ys) rs@(Refactorings ors) = 
	return (bs, L.nub ocs, L.nub ccs)
undoUntilC fn cs ocs ccs (i:is) bs@(Buffers xs ys) rs@(Refactorings ors) = do
	--let (i, is') = S.deleteFindMin is
 	--appendFile "./log.txt" $ "undo: " ++ show i ++ "\n"
	let undoUntilB' buffer = undoUntilB buffer i
	let undoUntilCB' buffer = undoUntilClosedB buffer i
	bs' <- if i `S.member` cs
		then undoUntilC fn cs ocs ccs is bs rs
		else case IM.lookup i ors of
			Nothing -> undoUntilC fn (i `S.insert` cs) (i:ocs) ccs (is ++ is') bs' rs
			 where 
			 	bs' = bs { openedBufs = xs'}
				xs' = xs0' : xs1
				(xs0', is') = undoUntilB' xs0
				([xs0], xs1) = L.partition ((== fn) . (fileName)) xs
			Just fs -> do 
				--foldI-rM (\k m -> )
				ys0'' <- mapM undoUntilCB' (map snd $ M.toList ys0)
				case (unzip $ map undoUntilB' xs0, unzip ys0'') of 
					((xs0', is1), (ys0', is2)) -> undoUntilC fn (i `S.insert` cs) (ocs ++ (concat is1)) (ccs ++ (concat is2)) (is ++ (concat $ is1 ++ is2)) (Buffers (xs1 ++ xs0') (ys1 `M.union` (M.fromList $ map (\x -> (fileName x,x)) ys0'))) rs
	  		 where 
				  (xs0, xs1) = L.partition (\b -> any (\f -> f == fileName b) fs ) xs
				  (ys0, ys1) = M.partition (\b -> any (\f -> f == fileName b) fs ) ys
	return bs' 

-- | visszavonja a megadott azonosítójú változást a bufferben, lekezeli az esetleges konfliktusokat
undoUntilB :: Buffer -> Int -> (Buffer, [Int])
undoUntilB b@(Buffer { undos = URList [] _ }) _ = (b, [])
undoUntilB b n 
 | L.null us1 = (b, [])
 | otherwise = (b { undos = undos', content = content' }, ids)
 where
 	(us0, us1) = span ((/= n) . changeId) us'
	(hus1,tus1) = (head us1,tail us1)
 	content' = L.foldl applyUs (content b) us0'
	(mus, mcs) = mergeUntil3'' (updates hus1) (L.reverse us0) []
	--mcs = mergeUntil3 mus us0
	us0' = case mus of
		[] -> L.map (\(AtomicChange _ u _) -> u) (us0 ++ [hus1])
		_  ->  [mus]
	applyUs = L.foldl apply
	apply cs u = applyUpdate u cs
	reverseUs u = L.reverse $ map reverseUpdate u
	reversedUs = case mus of 
		[] -> L.reverse $ map (\(AtomicChange i u name) -> (AtomicChange i (reverseUs u) name)) (us0 ++ [hus1])
		_  -> [hus1 { updates = reverseUs mus }]
	us1' = case mus of 
		[] -> L.tail us1
		_  -> (L.reverse mcs) ++ L.tail us1
	undos' = URList us1' (reversedUs ++ rs)
 	(URList us'@(AtomicChange i u name:us) rs) = undos b 
	ids = case mus of
		[]  -> (map changeId us0) ++ [n]
		_   -> [n]

-- | visszavonja a megadott azonosítójú változást a meg nem nyitott fájlban, lekezeli az esetleges konfliktusokat
undoUntilClosedB :: Buffer -> Int -> IO (Buffer, [Int])
undoUntilClosedB b@(Buffer { undos = URList [] _ }) _ = return (b, [])
undoUntilClosedB b n = do
	str <- SIO.readFile $ fileName b 
 	let (URList us'@(AtomicChange i u name:us) rs) = undos b 
 	let (us0, us1) = span ((/= n) . changeId) us'
	if L.null us1
	 then return (b, [])
	 else do
		let (hus1,tus1) = (head us1,tail us1)
		let apply cs u = applyUpdate u cs
		let applyUs = L.foldl apply
		let (mus, mcs) = mergeUntil3'' (updates hus1) (L.reverse us0) []
		--let mcs = mergeUntil3 mus us0
		let us0' = case mus of
			[] -> L.map (\(AtomicChange _ u _) -> u) (us0 ++ [hus1])
			_  ->  [mus]
	 	let content' = L.foldl applyUs (fromString str) us0'
		let reverseUs u = L.reverse $ map reverseUpdate u
		let reversedUs = case mus of 
			[] -> L.reverse $ map (\(AtomicChange i u name) -> (AtomicChange i (reverseUs u) name)) (us0 ++ [hus1])
			_  -> [hus1 { updates = reverseUs mus }]
		let us1' = case mus of 
			[] -> L.tail us1
			_  -> (L.reverse mcs) ++ L.tail us1
		let undos' = URList us1' (reversedUs ++ rs)
		let ids = case mus of
			[]  -> (map changeId us0) ++ [n]
			_   -> [n]
	
		h' <- openFile (fileName b) WriteMode
		hPutStr h' $ toString content'
		hClose h'
		return (b { undos = undos' }, ids)


-- | visszaadja az utolsó változás azonosítóját
lastUId (URList (x:_) _) = changeId x
lastUId _ = 0

------------------------------------------------------------------
-- redo
------------------------------------------------------------------

-- | előrelép a megadott fájl állapotában, magával húzva a többi fájlban is az érintett változás(okat)t
redo :: State -> String -> Int -> IO (State, [String], [String], [String], [Int], [Int])
redo s fn n = do
	(buffers', sfs, ofs, cfs, orns, crns) <- redo0 (buffers s) fn n (refactorings s) (renamings s)
	let s' = s { buffers = buffers' }
	return (s', sfs, ofs, cfs, orns, crns)

-- | visszavonja az utolsó változtatást a bufferben
redoB :: Buffer -> Buffer
redoB b@(Buffer {undos = URList _ []}) = b
redoB b = b { undos = undos', content = content' }
 where
 	content' = L.foldl apply (content b) r
	apply cs r = applyUpdate r cs
	reversed = L.reverse $ map reverseUpdate r
	undos' = URList ((AtomicChange i reversed name):us) rs
 	(URList us (AtomicChange i r name : rs)) = undos b 


redo0 :: Buffers -> String -> Int -> Refactorings -> IM.IntMap (String, String) -> IO (Buffers, [String], [String], [String], [Int], [Int])
redo0 bs@(Buffers xs ys) fn 0 rs@(Refactorings ors) rns = do
	let ([x], xs'') = L.partition ((== fn) . fileName) xs
	let cId = lastRId $ undos x
 	if IM.member cId ors
	  then do 
	  	(bs0, ids0, ids1) <- redoUntilC fn (S.empty) [] [] [cId] bs rs
		let orns = L.filter (\x -> IM.member x rns) ids0
		let crns = L.filter (\x -> IM.member x rns) ids1
		let fs0 = L.foldl L.union [] $ mapMaybe (`IM.lookup` ors) ids0
		let orns' = mapMaybe (\x -> IM.lookup x rns) orns
		let crns' = mapMaybe (\x -> IM.lookup x rns) crns
		let fs' = map (renameFile orns' crns') fs0
		let (ofs, cfs, sfs) = separateFilenames fs' [] [] []
		--let rfs = L.map fileName $ L.filter (L.null . getRedos . undos) $ openedBufs bs0
		return (bs0, sfs, ofs, cfs, orns, crns)
	  else do
		let bs' = Buffers (redoB x : xs'') ys
		let rfs = L.map fileName $ L.filter (L.null . getRedos . undos) $ openedBufs bs'
	  	return (bs', [fileName x], [], [], [], [])
redo0 bs@(Buffers xs ys) fn n rs@(Refactorings ors) rns = do
	let ([x], xs'') = L.partition ((== fn) . fileName) xs
	let cId = lastRId (undos x)
	let cIds = L.map changeId $ last $ lastRedos x n
	--putStrLn $ show cIds
 	if not $ L.null cIds && (L.length cIds > 1 || IM.member (head cIds) ors || L.head cIds /= cId)
	  then do 
	  	(bs0, ids0, ids1) <- redoUntilC fn (S.empty) [] [] cIds bs rs
		--let cfs = map fileName xs
		let fs0 = L.foldl L.union [] $ mapMaybe (`IM.lookup` ors) ids0
		let fs = L.nub $ fn : fs0
		let orns = L.filter (\x -> IM.member x rns) ids0
		let crns = L.filter (\x -> IM.member x rns) ids1
		--let rfs = L.map fileName $ L.filter (L.null . getRedos . undos) $ openedBufs bs0
		let orns' = mapMaybe (\x -> IM.lookup x rns) orns
		let crns' = mapMaybe (\x -> IM.lookup x rns) crns
		let fs' = map (renameFile orns' crns') fs
		let (ofs, cfs, sfs) = separateFilenames fs' [] [] []
		return (bs0, sfs, ofs, cfs, orns, crns)
	  else do
		let bs' = Buffers (redoB x : xs'') ys
		--let rfs = L.map fileName $ L.filter (L.null . getRedos . undos) $ openedBufs bs'
	  	return (bs', [fileName x], [], [], [], [])



-- | bejárja a konfliktus miatt újra végrehajtandó változásokatt
redoUntilC fn cs ocs ccs [] bs@(Buffers xs ys) rs@(Refactorings ors) = do
  return (bs, L.nub ocs, L.nub ccs)
redoUntilC fn cs ocs ccs (i:is) bs@(Buffers xs ys) rs@(Refactorings ors) = do
	let redoUntilB' buffer = redoUntilB buffer i
	let redoUntilCB' buffer = redoUntilClosedB buffer i
	bs' <- if i `S.member` cs 
		then redoUntilC fn cs ocs ccs is bs rs
		else case IM.lookup i ors of
			Nothing -> redoUntilC fn (i `S.insert` cs) (i:ocs) ccs (is ++ is') bs' rs
			 where
			 	bs' = bs { openedBufs = xs'}
				xs' = xs0' : xs1
				(xs0', is') = redoUntilB' xs0
				([xs0], xs1) = L.partition ((== fn) . (fileName)) xs
			Just fs -> do
				ys0'' <- mapM redoUntilCB' (map snd $ M.toList ys0)
				case (unzip $ map redoUntilB' xs0, unzip ys0'') of 
					((xs0', is1),(ys0',is2)) -> redoUntilC fn (i `S.insert` cs) (ocs ++ (concat is1)) (ccs ++ (concat is2))  (is ++ (concat $ is1 ++ is2 )) (Buffers (xs1 ++ xs0') (ys1 `M.union` (M.fromList $ map (\x -> (fileName x,x)) ys0'))) rs
			 where
			  (xs0, xs1) = L.partition (\b -> any (\f -> f == fileName b) fs ) xs
			  (ys0, ys1) = M.partition (\b -> any (\f -> f == fileName b) fs ) ys
	return bs'
	

-- | megismétli a megadott azonosítójú változást a bufferben, lekezeli az esetleges konfliktusokat
redoUntilB :: Buffer -> Int -> (Buffer, [Int])
redoUntilB b@(Buffer { undos = URList _ [] }) _ = (b, [])
redoUntilB b n 
 | L.null rs1 = (b, [])
 | otherwise = (b { undos = undos', content = content' }, ids)
 where
 	content' = L.foldl applyUs (content b) rs0'
 	(rs0, rs1) = span ((/= n) . changeId) rs'
	(hrs1, trs1) = (head rs1, tail rs1)
	(mrs, mcs) = mergeUntil3'' (updates hrs1) (L.reverse rs0) []
	--mcs = mergeUntil3 mrs rs0
	rs0' = case mrs of
		[] -> L.map updates (rs0 ++ [hrs1])
		_  ->  [mrs]
	rs1' = case mrs of 
		[] -> L.tail rs1
		_  -> (L.reverse mcs) ++ L.tail rs1
	reversedUs = case mrs of 
		[] -> L.reverse $ map (\c -> c { updates = reverseUs (updates c) }) (rs0 ++ [hrs1])
		_  -> [hrs1 { updates = (reverseUs mrs) }]

	undos' = URList (reversedUs ++ us) rs1'
	URList us rs'@((AtomicChange i r name):rs) = undos b
	
	ids = case mrs of
		[]  -> (map changeId rs0) ++ [n]
		_   -> [n]

	applyUs = L.foldl apply
	apply cs u = applyUpdate u cs
	reverseUs u = L.reverse $ map reverseUpdate u

-- | megismétli a megadott azonosítójú változást a meg nem nyitott fájlban, lekezeli az esetleges konfliktusokat
redoUntilClosedB :: Buffer -> Int -> IO (Buffer, [Int])
redoUntilClosedB b@(Buffer { undos = URList _ [] }) _ = return (b, [])
redoUntilClosedB b n = do
	str <- SIO.readFile $ fileName b 
	let URList us rs'@((AtomicChange i r name):rs) = undos b
 	let (rs0, rs1) = span ((/= n) . changeId) rs'
	if L.null rs1
	 then return (b, [])
	 else do
		let (hrs1, trs1) = (head rs1, tail rs1)
		let apply cs u = applyUpdate u cs
		let applyUs = L.foldl apply
		let (mrs, mcs) = mergeUntil3'' (updates hrs1) (L.reverse rs0) []
		--let mcs = mergeUntil3 mrs rs0
		let rs0' = case mrs of
			[] -> L.map updates (rs0 ++ [hrs1])
			_  ->  [mrs]
		let rs1' = case mrs of 
			[] -> L.tail rs1
			_  -> L.reverse mcs ++ L.tail rs1
	 	let content' = L.foldl applyUs (fromString str) rs0'
		let reverseUs u = L.reverse $ map reverseUpdate u
		let reversedUs = case mrs of 
			[] -> L.reverse $ map (\c -> c { updates = reverseUs (updates c) }) (rs0 ++ [hrs1])
			_  -> [hrs1 { updates = (reverseUs mrs) }]
	
		let undos' = URList (reversedUs ++ us) rs1'
		let ids = case mrs of
			[]  -> (map changeId rs0) ++ [n]
			_   -> [n]
	
		writeFile (fileName b) content'
		return (b { undos = undos' }, ids)

-- | visszaadja az utolsó visszavont változás azonosítóját
lastRId (URList _ (AtomicChange i _ _ :_)) = i
lastRId _ = 0

------------------------------------------------------------------
-- merge
------------------------------------------------------------------
------------------------------------------------------------------
---- új change-k kiszámolásához 
------------------------------------------------------------------

mergeUntil1 :: Update -> [Update] -> [Update] -> [Update]
mergeUntil1 x acc [] = L.reverse acc
mergeUntil1 x acc (y:ys) = case merge x y of
	Just u -> mergeUntil1 x (u:acc) ys 
		where Just x' = merge'' x y
		-- ^ folyamatosan egy "korban" kell lenniük, hogy értelmes legyen a vizsgálat
		-- biztosan mergelhető a másik iránbyban is, mivel fordítva az
	Nothing -> []

mergeUntil2 :: Update -> [Change] -> [Change] -> [Change]
mergeUntil2 x acc [] = L.reverse acc
mergeUntil2 x acc (y:ys) = case mergeUntil1 x [] (updates y) of
	[] -> []
	xs -> mergeUntil2 x (AtomicChange (changeId y) xs (changeType y):acc) ys

mergeUntil3 :: [Update] -> [Change] -> [Change]
mergeUntil3 [] ys = ys
mergeUntil3 (x:xs) ys = case mergeUntil2 x [] ys of 
	[] -> []
	us -> mergeUntil3 xs us

-- visszavisszük az refac update-t a múltba
-- refac, change, refac
merge'' :: Update -> Update -> Maybe Update 
merge'' x@(Insert p s) y@(Insert p' s') 
 | fromPoint p' <= fromPoint p = Just $ Insert (Point (fromPoint p + length s')) s
 | fromPoint p {-+ length s-} <{-=-} fromPoint p' {-+ length s-}  = Just x
 | otherwise = Nothing
merge'' x@(Insert p s) y@(Delete p' s') 
 | fromPoint p {- + length s -} <= fromPoint p' {- + length s -} = Just x
 | fromPoint p' {-+ length s'-} <= fromPoint p - length s' = Just $ Insert (Point (fromPoint p - length s')) s
 | otherwise = Nothing
merge'' x@(Delete p s) y@(Insert p' s') 
 | fromPoint p' {- + length s' -} <= fromPoint p {- + length s' -} = Just $ Delete (Point (fromPoint p + length s')) s
 | fromPoint p + length s <= fromPoint p' = Just x 
 | otherwise = Nothing
merge'' x@(Delete p s) y@(Delete p' s') 
 | fromPoint p' + length s'  <= fromPoint p - length s' = Just $ Delete (Point (fromPoint p - length s')) s
 | fromPoint p + length s <= fromPoint p' = Just x 
 | otherwise = Nothing

-- refac, change, change
merge :: Update -> Update -> Maybe Update 
merge x@(Insert p s) y@(Insert p' s') 
 | fromPoint p {-+ length s-} <= fromPoint p' {-+ length s-}  = Just $ Insert (Point (fromPoint p' + length s)) s'
 | fromPoint p' <{-=-} fromPoint p = Just y
 | otherwise = Nothing
merge x@(Insert p s) y@(Delete p' s') 
 | fromPoint p {- + length s -} <= fromPoint p' {- + length s -} = Just $ Delete (Point (fromPoint p' + length s)) s'
 | fromPoint p' {-+ length s'-} <= fromPoint p - length s' = Just y
 | otherwise = Nothing
merge x@(Delete p s) y@(Insert p' s') 
 | fromPoint p' {- + length s' -} <= fromPoint p {- + length s' -} = Just y
 | fromPoint p + length s <= fromPoint p' = Just $ Insert (Point (fromPoint p' - length s)) s'
 | otherwise = Nothing
merge x@(Delete p s) y@(Delete p' s') 
 | fromPoint p' + length s'  <= fromPoint p - length s' = Just y
 | fromPoint p + length s <= fromPoint p' = Just $ Delete (Point (fromPoint p' - length s)) s'
 | otherwise = Nothing

------------------------------------------------------------------
---- új (refactoring) update-k kiszámolásához 
------------------------------------------------------------------

mergeUntil1' :: Update -> [Update] -> Maybe Update
mergeUntil1' x [] = Just x
mergeUntil1' x (y:ys) = case merge' y x of
	Just u  -> mergeUntil1' u ys
	Nothing -> Nothing 

mergeUntil2' :: [Update] -> [Update] -> [Update] -> [Update]
mergeUntil2' [] acc _us = L.reverse acc
mergeUntil2' (x:xs) acc us = case mergeUntil1' x us of
	Just u  -> mergeUntil2' xs (u:acc) us
	Nothing -> []
	
-- change-nek fordított sorrendeben kell lennie
mergeUntil3' :: [Update] -> [Change] -> [Update]
mergeUntil3' xs [] = xs
mergeUntil3' xs (c:cs) = case mergeUntil2' xs [] (L.reverse $ updates c) of
	[] -> []
	us -> mergeUntil3' us cs

-- change, refac, refac
merge' :: Update -> Update -> Maybe Update 
merge' x@(Insert p s) y@(Insert p' s') 
 | fromPoint p + length s <= fromPoint p' = Just $ Insert (Point (fromPoint p' - length s)) s'
 | fromPoint p' {- + length s' -} <= fromPoint p {- + length s' -} = Just y
 | otherwise = Nothing
merge' x@(Insert p s) y@(Delete p' s') 
 | fromPoint p + length s <= fromPoint p' = Just $ Delete (Point (fromPoint p' - length s)) s'
 | fromPoint p' + length s' <= fromPoint p = Just y
 | otherwise = Nothing
merge' x@(Delete p s) y@(Insert p' s') 
 | fromPoint p' {- + length s' -} <= fromPoint p {- + length s' -} = Just y
 | fromPoint p  {-+ length s-} <{-=-} fromPoint p' = Just $ Insert (Point (fromPoint p' + length s)) s'
 | otherwise = Nothing
merge' x@(Delete p s) y@(Delete p' s') 
 | fromPoint p + length s  <= fromPoint p' {- + length s -} = Just $ Delete (Point (fromPoint p' + length s)) s'
 | fromPoint p' + length s' <= fromPoint p = Just y
 | otherwise = Nothing

merge''' :: Update -> Update -> Maybe (Update, Update) 
merge''' x@(Insert p s) y@(Insert p' s') 
 | fromPoint p + length s <= fromPoint p' = Just (x, Insert (Point (fromPoint p' - length s)) s')
 | fromPoint p' {- + length s' -} <= fromPoint p {- + length s' -} = Just (Insert (Point (fromPoint p + length s')) s, y)
 | otherwise = Nothing
merge''' x@(Insert p s) y@(Delete p' s') 
 | fromPoint p + length s <= fromPoint p' = Just (x, Delete (Point (fromPoint p' - length s)) s')
 | fromPoint p' + length s' <= fromPoint p = Just (Insert (Point (fromPoint p - length s')) s, y)
 | otherwise = Nothing
merge''' x@(Delete p s) y@(Insert p' s') 
 | fromPoint p' {- + length s' -} <= fromPoint p {- + length s' -} = Just (Delete (Point (fromPoint p + length s')) s, y)
 | fromPoint p  {-+ length s-} <{-=-} fromPoint p' = Just (x, Insert (Point (fromPoint p' + length s)) s')
 | otherwise = Nothing
merge''' x@(Delete p s) y@(Delete p' s') 
 | fromPoint p {-+ length s-}  <{-=-} fromPoint p' {- + length s -} = Just (x, Delete (Point (fromPoint p' + length s)) s')
 | fromPoint p' {-+ length s'-} <= fromPoint p = Just (Delete (Point (fromPoint p - length s')) s, y)
 | otherwise = Nothing

mergeUntil1'' :: Update -> [Update] -> [Update] -> Maybe (Update, [Update])
mergeUntil1'' x [] acc = Just (x, L.reverse acc)
mergeUntil1'' x (y:ys) acc = case merge''' y x of
	Just (y', x')  -> mergeUntil1'' x' ys (y':acc)
	Nothing -> Nothing 

mergeUntil2'' :: [Update] -> [Update] -> [Update] -> ([Update],[Update])
mergeUntil2'' [] acc us = (L.reverse acc, us)
mergeUntil2'' (x:xs) acc ys = case mergeUntil1'' x ys [] of
	Just (x', ys') -> mergeUntil2'' xs (x':acc) ys'
	Nothing -> ([], [])
	
-- change-nek fordított sorrendeben kell lennie
mergeUntil3'' :: [Update] -> [Change] -> [Change] -> ([Update], [Change])
mergeUntil3'' xs [] acc = (xs, L.reverse acc)
mergeUntil3'' xs (c:cs) acc = case mergeUntil2'' xs [] (L.reverse $ updates c) of
	([], []) -> ([], [])
	(xs', c') -> mergeUntil3'' xs' cs ((c { updates = (L.reverse c')}):acc)

