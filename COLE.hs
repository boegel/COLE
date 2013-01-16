{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |COLE, a Haskell program for tuning compiler optimizations
--
-- see paper "COLE: Compiler Optimization Level Exploration"
--     by Kenneth Hoste and Lieven Eeckhout
--     in Proceedings of the 6th annual IEEE/ACM International Symposium 
--     on Code Generation and Optimization (CGO)
--     [http://boegel.kejo.be/ELIS/pub/hoste08cole_CGO08_paper.pdf]
--
-- Sept. 2011, by Kenneth Hoste
--
-- version: 0.1

module COLE(
           cole, COLEConfig(..),
           MD5, BaseFlag, OptFlag,
           EvaluationType(..),
           EvalResultType(..),
           FitnessType(..),
           ValueType(..)
           ) where

import Control.Exception (ErrorCall,Handler(..),catches,evaluate)
import Data.ByteString.Char8 (pack)
import Data.Digest.OpenSSL.MD5 (md5sum) -- nano-md5
import Data.List (foldl',intersperse,nub,sort)
import Data.Map (Map,empty,fromList,insert,lookup,(!))
import Data.Maybe (fromJust,isNothing,isJust)
import System.Directory (createDirectoryIfMissing,doesFileExist,removeFile)
import System.Exit (ExitCode(..))
import System.IO (IOMode(..),openFile,hPutStrLn,hClose)
import System.Posix.Env (putEnv)
import System.Posix.Unistd(sleep)
import System.Process (system)
import System.Random (StdGen, mkStdGen, randoms)

import GA

-- |Usefull type aliases.
type Label = String
type OptFlag = String
type BaseFlag = OptFlag
type MD5 = String
type Cache = Map MD5 (Int, Maybe EntityData)

-- |Type of value.
data ValueType = 
    -- ^ higher values indicate better (e.g. speedup)
      HigherIsBetter
    -- ^ lower values indicate better (e.g. execution time)
    | LowerIsBetter
    deriving (Read,Show)


-- |Type of evaluation result.
data EvalResultType =
    -- single value 
      Value ValueType
    -- tuple of values
    | Tuple (ValueType,ValueType)
    -- triple of values
    | Triple (ValueType,ValueType,ValueType)
    deriving (Read,Show)

-- |Type of fitness scoring.
data FitnessType =
    -- ^ measured value is used as fitness
      ByValue
    -- ^ fitness by (strict) dominance
    | StrictDominance
    -- ^ fitness by fuzzy dominance, by specified threshold
    | FuzzyDominance Double
    deriving (Read,Show)

-- |Type of evaluation.
data EvaluationType = 
    -- ^ evaluate on local machine (sequentially)
    --   one argument: evaluation script
      LocalEvaluation FilePath
    -- ^ evaluate by submiting/running PBS jobs (in parallel)
    --   three arguments: qsub arguments, jobs output dir and job script
    | EvaluationViaPBS String FilePath FilePath

-- |Data required to score a COLE entity
--
-- * label for experiment 
-- * type of fitness (by value, by strict/fuzzy dominance)
-- * type of evaluation (local, through PBS jobs, ...)
-- * type of evaluation result (value, tuple, triple)
--
data COLEData = COLEData 
                  Label -- ^ label for COLE experiment
                  FitnessType -- ^ type of fitness scoring
                  EvaluationType -- ^ evaluation type
                  EvalResultType -- ^ type of evaluation result
                  Int -- ^ number of experiments per entity
                      --   required for statically sound comparison

-- |Entity evaluation data. 
--
-- Performance data per benchmark, k times for each benchmark/metric.
data EntityData =
    -- ^ single value
      EntityDataValue [[Double]]
    -- ^ tuple
    | EntityDataTuple ([[Double]],[[Double]])
    -- ^ triple
    | EntityDataTriple ([[Double]],[[Double]],[[Double]])
    deriving (Eq,Read,Show)
 
-- |Accessing triple elements.
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

thrd3 :: (a,b,c) -> c
thrd3 (_,_,z) = z

-- |Safe parsing of text.
safeParse :: (Read a) => String -- ^ text to parse
                      -> IO (Maybe a) -- ^ parsed result (or Nothing)
safeParse str = catches 
                 (Just `fmap` evaluate (read str)) 
                 [Handler $ \(_ :: ErrorCall) -> do return Nothing]

-- Return COLE cache directory.
getCacheDir :: FilePath
getCacheDir = "COLEcache/"

-- |Return COLE cache file for given label.
cacheFileFor :: Label -- ^ label for COLE experiments
             -> FilePath -- ^ path to cache file
cacheFileFor label = getCacheDir ++ label

-- |Add entries to the cache.
addToCache :: Cache -- ^ current cache
           -> [(MD5, (Int, Maybe EntityData))] -- ^ entries to add
           -> Cache -- ^ new cache
addToCache cache ((md5,ce):xs) = addToCache cache' xs
  where
    cache' = insert md5 ce cache
addToCache cache    []    = cache

-- |Try to restore experiment result from cache.
fetchFromCache :: Cache -- ^ current cache
               -> MD5 -- ^ key for experiment
               -> Maybe (Int, Maybe EntityData) -- ^ experiment data (or Nothing)
fetchFromCache cache md5 = Data.Map.lookup md5 cache

-- |Add experiment result to cache.
writeToCache :: Cache -- ^ current cache
             -> Label -- ^ label for COLE experiment
             -> (MD5, (Int, Maybe EntityData)) -- ^ cache entry to add
             -> IO ()
writeToCache cache label (md5,ce) = do
    let cacheFile = cacheFileFor label
    createDirectoryIfMissing True getCacheDir
    if isNothing $ fetchFromCache cache md5
      then do
         h <- openFile cacheFile AppendMode
         hPutStrLn h (show (md5,ce))
         hClose h
      else return ()

-- |Parse cache, line by line, with error handling.
parseCache :: [String] -- ^ cache lines
           -> IO [(MD5,(Int,Maybe EntityData))] -- ^ parsed cache entries
parseCache (cl:cls) = do
    parsedCacheLine <- safeParse cl :: IO (Maybe (MD5,(Int,Maybe EntityData)))
    case parsedCacheLine of
      Nothing -> error $ "(parseCache) ERROR! Failed to parse cache line: " 
                      ++ cl ++ "\nCorrupt cache? See if you can fix it..."
      Just cl' -> do 
        cls' <- parseCache cls
        return (cl':cls')

parseCache [] = return []

-- |Read cache from file.
readCache :: Label -- ^ label for COLE experiment
          -> IO Cache -- ^ parsed cache
readCache label = do
    let cacheFile = cacheFileFor label
    cacheExists <- doesFileExist cacheFile
    if cacheExists
      then do
        txt <- readFile cacheFile
        -- parsing cache, with proper error handling 
        cacheEntries <- parseCache $ lines txt
        let keys = map fst cacheEntries
            values = map snd cacheEntries
        return $ fromList $ zip keys values
      else return empty


-- |Wrap evaluation result into data type.
parseEntityData :: EvalResultType -- ^ result type
                -> (MD5,Int) -- ^ key and number of flags
                -> String -- ^ string to parse
                -> String -- ^ error message
                -> IO (MD5, (Int,Maybe EntityData)) -- parsed entity data
parseEntityData (Value _) (md5,nFlags) txt errorStr = do
    parsedResult <- safeParse txt :: IO (Maybe (Maybe [[Double]]))
    case parsedResult of
          Nothing -> error errorStr
          Just Nothing -> return (md5, (nFlags, Nothing))
          Just (Just x) -> return $ (md5, (nFlags, 
                                           Just $ EntityDataValue x
                                          )
                                    )
parseEntityData (Tuple _) (md5,nFlags) txt errorStr = do
    parsedResult <- safeParse txt :: IO (Maybe (Maybe ([[Double]],[[Double]])))
    case parsedResult of
          Nothing -> error errorStr
          Just Nothing -> return (md5, (nFlags, Nothing))
          Just (Just (x,y)) -> return $ (md5, (nFlags, 
                                           Just $ EntityDataTuple (x,y)
                                          )
                                    )
parseEntityData (Triple _) (md5,nFlags) txt errorStr = do
    parsedResult <- safeParse txt :: IO (Maybe (Maybe ([[Double]],[[Double]],[[Double]])))
    case parsedResult of
          Nothing -> error errorStr
          Just Nothing -> return (md5, (nFlags, Nothing))
          Just (Just (x,y,z)) -> return $ (md5, (nFlags, 
                                           Just $ EntityDataTriple (x,y,z)
                                          )
                                    )

-- |Prepare scoring of entities (if required).
prepareEval :: EvaluationType -- ^ evaluation type
            -> String -- label for COLE experiment
            -> Int -- ^ number of experiments per entity
            -> [(Int,String,MD5)] -- ^ entities
            -> IO()
prepareEval (LocalEvaluation _) _ _ _ = return ()
prepareEval (EvaluationViaPBS qsubArgs jobsDir jobScript) label k es = do 
    createDirectoryIfMissing True jobsDir
    -- submit job script for all entities
    mapM_ (submitScriptFor (qsubArgs,jobsDir,label,jobScript) k) es
    -- wait until all jobs finish
    waitForJobs jobsDir label $ map (\x -> (fst3 x,thrd3 x)) es
    return ()

-- |Construct job label for a COLE experiment.
jobLabelFor :: String -- ^ label for COLE experiment
            -> Int -- ^ number of flags
            -> MD5 -- ^ entity key
            -> String -- ^ job label
jobLabelFor label n md5 = "COLE_" ++ label ++ "_" ++ show n ++ "-" ++ md5

submitScriptFor :: (String -- ^ qsub arguments
                   ,FilePath -- ^ jobs directory
                   ,String -- ^ label for COLE experiment
                   ,String -- ^ job script
                   )
                -> Int -- ^ number of experiments per entity
                -> (Int -- ^ number of flags
                   ,String -- ^ string with all flags
                   ,MD5 -- ^ entity key
                   )
                -> IO()
submitScriptFor (qsubArgs,jobsDir,label,jobScript) k (nFlags,flagsStr,md5) = do
    -- create job script
    let jobScript' = "!#/bin/bash\n\n " 
                  ++ "COLE_OPTFLAGS=" ++ show flagsStr ++ "\n\n"
                  ++ "COLE_EXPERIMENT_COUNT=" ++ show k ++ "\n\n"
                  ++ "# input job script starts here\n\n" ++ jobScript
        jobFileName = "job_" ++ md5 ++ ".sh"
    writeFile jobFileName jobScript'
    -- submit job script
    let jobLabel = jobLabelFor label nFlags md5
        cmd = "qsub " ++ qsubArgs ++ "-j oe " 
                      ++ "-o " ++ jobsDir ++ "/" ++ jobLabel ++ " "
                      ++ "-N " ++ jobLabel ++ " "
                      ++ "-j oe " ++ qsubArgs
                      ++ jobFileName
    ec <- system $ cmd ++ " &> " ++ jobLabel
    qsubOutput <- readFile jobLabel
    if length qsubOutput `seq` ec == ExitSuccess
      then putStrLn $ "job submitted: " 
                   ++ jobLabel 
                   ++ "; job id: " 
                   ++ qsubOutput
      else error $ "(submitScriptFor) ERROR! Failed to submit job script: " 
                ++ qsubOutput
    -- cleanup
    mapM_ removeFile [jobFileName,jobLabel]

-- |Wait until all PBS jobs for COLE experiments are done.
waitForJobs :: FilePath -- ^ jobs directory
            -> String -- ^ label for COLE experiment
            -> [(Int,MD5)] -- ^ entities (# flags and keys
            -> IO()
waitForJobs jobsDir label es = do
    _ <- sleep 60 -- sleep for a minute
    let fileNames = map ((++) ( jobsDir++"/") 
                        . \(n,md5) -> jobLabelFor label n md5) es
    filesExist <- mapM doesFileExist fileNames
    let n = length es
        nFound = filter id filesExist
    putStrLn $ "--- " ++ show nFound ++ " out of " 
            ++ show n ++ " required job results found"
    -- return if alles files were found, else wait a bit longer
    if and filesExist
      then return ()
      else waitForJobs jobsDir label es

-- |Error string to show in case parsing of evaluation/job result failed.
parseResultErrorStr :: (String -- ^ kind of evaluation ('evaluation', 'job') 
                       ,String -- ^ text to parse
                       ,String -- ^ script file name
                       ,String -- ^ flags used in experiment
                       ) 
                    -> String
parseResultErrorStr (kind,txt,script,flags) = 
     "(evaluateFor) ERROR! Failed to parse " 
  ++ kind ++ " result: " ++ txt 
  ++ "\nYou should fix your " ++ kind
  ++ " script " ++ script ++ ". " 
  ++ "Flags used:\n" ++ flags

-- |Evaluate an entity for a single benchmark using script.
evaluateFor :: EvaluationType -- ^ type of  evaluation (local,PBS,...)
            -> EvalResultType -- ^ type of evaluation result (value,tuple,...)
            -> String -- ^ COLE experiment label
            -> Int -- ^ number of experiments per entity
            -> (Int,String,MD5) -- ^ flag cnt, all flags, 
                                --   md5sum of all flags string
            -> IO (MD5, (Int, Maybe EntityData)) -- ^ evaluation result
evaluateFor (LocalEvaluation script) erType _ k (nFlags,allFlags,md5) = do
    putStrLn $ "evaluating " ++ md5 ++ " [" ++ (take 100 allFlags) 
            ++ if (length allFlags > 100) then "...]" else "]"
    let outFile = "COLE.out"
    putEnv $ "COLE_OPTFLAGS=" ++ allFlags
    putEnv $ "COLE_EXPERIMENT_COUNT=" ++ show k
    ec <- system $ script ++ " > " ++ outFile
    txt <- readFile outFile
    -- need to force evaluation of file contents for correctness 
    -- (due to laziness)
    if length txt `seq` ec == ExitSuccess
      then do
        -- parsing of result, with proper error handling
        ed <- parseEntityData erType (md5,nFlags) txt 
                (parseResultErrorStr ("evaluation",txt,script,allFlags))
        putStrLn $ "evaluation result: " ++ txt
        return ed
      else error "(evaluateFor) ERROR! Evaluation script failed?!?"

evaluateFor (EvaluationViaPBS _ jobsDir jobScript) 
            erType label _ (nFlags,allFlags,md5) = do
    let fName = jobsDir ++ "/" ++ (jobLabelFor label nFlags md5)
    fe <- doesFileExist fName
    if not fe
      then error $ "File " ++ fName ++ " disappeared (or was never there)?!?"
      else do
        txt <- readFile fName
        ed <- parseEntityData erType (md5,nFlags) txt
                (parseResultErrorStr ("job",txt,jobScript,allFlags))
        putStrLn $ "evaluation result: " ++ txt
        return ed

-- |Determine whether a value is better than another value,
-- in a fuzzy way.
(~>) :: Double  -- ^ fuzziness threshold
     -> ValueType -- ^ type of value
     -> Double -- ^ relative value difference
     -> Bool -- ^ True is first value is better, False otherwise
(~>) threshold HigherIsBetter x = x > threshold
(~>) threshold LowerIsBetter x = x < (negate threshold)

-- |Determine whether a value is not worse than another value,
-- in a fuzzy way.
(!<) :: Double  -- ^ fuzziness threshold
     -> ValueType -- ^ type of value
     -> Double -- ^ relative value difference
     -> Bool -- ^ False is first value is worse, True otherwise
(!<) threshold HigherIsBetter x = x > (negate threshold)
(!<) threshold LowerIsBetter x = x < threshold

-- |Determine whether two values are equal,
-- in a fuzzy way.
(~=) :: Double  -- ^ fuzziness threshold
     -> ValueType -- ^ type of value
     -> Double -- ^ relative value difference
     -> Bool -- ^ True is values are equal in a fuzzy way, False otherwise
(~=) threshold _ x = (abs x) < threshold
    
-- FIXME: get rid of this, and implement entity comparisons using Tukey's test
--
-- see http://en.wikipedia.org/wiki/Tukey%27s_range_test
head2 :: [[a]] -> a
head2 = head . head

-- |Determine whether one entity dominates another, in a fuzzy way.
dominatesFuzzy :: Double -- ^ fuzziness threshold
               -> EvalResultType -- ^ type of evaluation result
               -> (Int, Maybe EntityData) -- first entity
               -> (Int, Maybe EntityData) -- second entity
               -> Bool -- ^ True is first entity dominates second, False otherwise
-- Nothing can never dominate something else
dominatesFuzzy _ _ (_,Nothing) _                            = False
-- Nothing is always dominated
dominatesFuzzy _ _ (_,Just _) (_,Nothing)                   = True
-- fuzzy dominance for single values
dominatesFuzzy t (Value vt)
          (_, Just (EntityDataValue x1))
          (_, Just (EntityDataValue x2)) = (~>) t vt diff
  where
    diff = (head2 x1 - head2 x2) / head2 x1
-- fuzzy dominance for tuples
dominatesFuzzy t (Tuple (vtx,vty))
          (fCnt1, Just (EntityDataTuple (x1,y1)))
          (fCnt2, Just (EntityDataTuple (x2,y2)))
    -- significantly better first
    |  (~>) t vtx xDiff && (!<) t vty yDiff                 = True
    -- significantly better second
    |  (!<) t vtx xDiff && (~>) t vty yDiff                 = True
    -- same performance, fewer flags
    | (~=) t vtx xDiff && (~=) t vty yDiff && fCnt1 < fCnt2 = True
    -- doesn't dominate
    | otherwise                                             = False
  where
    xDiff = (head2 x1 - head2 x2) / head2 x1
    yDiff = (head2 y1 - head2 y2) / head2 y1
-- fuzzy dominance for triples
dominatesFuzzy t (Triple (vtx,vty,vtz))
          (fCnt1, Just (EntityDataTriple (x1,y1,z1)))
          (fCnt2, Just (EntityDataTriple (x2,y2,z2)))
    -- significantly better first
    | (~>) t vtx xDiff && (!<) t vty yDiff && (!<) t vtz zDiff = True
    -- significantly better second 
    | (!<) t vtx xDiff && (~>) t vty yDiff && (!<) t vtz zDiff = True
    -- significantly better third 
    | (!<) t vtx xDiff && (!<) t vty yDiff && (~>) t vtz zDiff = True
    -- same performance, fewer flags
    | (~=) t vtx xDiff && (~=) t vty yDiff
   && (~=) t vtz zDiff && fCnt1 < fCnt2                        = True
    -- doesn't dominate
    | otherwise                                                = False
  where
    xDiff = (head2 x1 - head2 x2) / head2 x1
    yDiff = (head2 y1 - head2 y2) / head2 y1
    zDiff = (head2 z1 - head2 z2) / head2 z1
dominatesFuzzy _ _ _ _ = error "(dominateFuzzy) You're not making sense..."

-- |Fast sum.
sum' :: (Num a) => [a] -> a
sum' = foldl' (+) 0

-- |Mean value of a list.
mean :: (Fractional a) => [a] -> a
mean xs = (sum' xs) / (fromIntegral $ length xs)

-- |Mean value of a matrix of values
--
-- FIXME: this is incorrect for e.g. speedups (should be harmonic mean)
meanmean :: (Fractional a) => [[a]] -> a
meanmean = mean . map mean

-- |Compute distance between two entities.
dist :: (Int,Maybe EntityData) -- first entity
     -> (Int,Maybe EntityData) -- second entity
     -> Double -- distance between entities
dist (_,Just (EntityDataValue x1))
     (_,Just (EntityDataValue x2)) = abs (meanmean x1 - meanmean x2)
dist (_,Nothing) (_,Just (EntityDataValue x)) = meanmean x
dist (_,Just (EntityDataValue x)) (_,Nothing) = meanmean x
dist (_,Just (EntityDataTuple (x1,y1)))
     (_,Just (EntityDataTuple (x2,y2))) = sqrt $ xDiff2 + yDiff2
  where
    xDiff2 = (meanmean x1 - meanmean x2)**2
    yDiff2 = (meanmean y1 - meanmean y2)**2
dist (_,Nothing) (_,Just (EntityDataTuple (x,y))) = sqrt $ (meanmean x)**2 + (meanmean y)**2
dist (_,Just (EntityDataTuple (x,y))) (_,Nothing) = sqrt $ (meanmean x)**2 + (meanmean y)**2
dist (_,Just (EntityDataTriple (x1,y1,z1)))
     (_,Just (EntityDataTriple (x2,y2,z2))) = sqrt $ xDiff2 + yDiff2 + zDiff2
  where
    xDiff2 = (meanmean x1 - meanmean x2)**2
    yDiff2 = (meanmean y1 - meanmean y2)**2
    zDiff2 = (meanmean z1 - meanmean z2)**2
dist (_,Nothing) (_,Just (EntityDataTriple (x,y,z))) = sqrt $ (meanmean x)**2 + (meanmean y)**2 + (meanmean z)**2
dist (_,Just (EntityDataTriple (x,y,z))) (_,Nothing) = sqrt $ (meanmean x)**2 + (meanmean y)**2 + (meanmean z)**2
dist (_,Nothing) (_,Nothing) = error "(dist) ERROR! Found: Nothing Nothing"
dist _ _ = error "(dist) You're not making any sense..."

-- |Calculate fitnesses of entities based on dominance.
calcFitnesses :: FitnessType -- ^ type of fitness
              -> EvalResultType -- ^ type of evaluation result
              -> Cache -- ^ evaluation results cache
              -> [MD5] -- ^ universe entity keys 
              -> [MD5] -- ^ population entity keys
              -> [Double] -- ^ fitness for population entities
-- fuzzy dominance: demand more than T% difference
calcFitnesses (FuzzyDominance threshold) erType
              cache univEntsMD5 popEntsMD5 = zipWith (+) rawFitnesses densities
  where
    univEnts = map (cache !) univEntsMD5
    -- actual entities corresponding to md5 sums
    popEnts = map (fromJust . flip Data.Map.lookup cache) popEntsMD5
    -- all entities: universe + current population
    allEnts = nub $ univEnts ++ popEnts
    -- dominance function
    dominates = dominatesFuzzy threshold erType
    -- calculate strengths for all known entities
    strengths = map (\ed -> length $ filter id 
                          $ map (dominates ed) univEnts
                    ) univEnts
    -- raw fitness entity E is sum of strengths of entities that dominate E
    rawFitnesses = map (\ed -> sum 
                             $ map (fromIntegral . snd)
                             $ filter (flip dominates ed . fst)
                             $ zip univEnts strengths) popEnts
    -- density information for entities, based on k'th nearest neighbor
    n = length allEnts
    k = round $ sqrt (fromIntegral n :: Double) :: Int
    -- k'th closest distances 
    -- (tail because first element is entity itself)
    kthDists = map (\ed -> flip (!!) k $ tail $ sort 
                         $ map (dist ed) $ allEnts) popEnts
    densities = map (\x -> 1.0 / (x + 2)) kthDists
-- strict dominance: same as fuzzy dominance, but with zero threshold
calcFitnesses StrictDominance erType
              cache univEntsMD5 popEntsMD5 = calcFitnesses (FuzzyDominance 0.0)
                                                           erType cache 
                                                           univEntsMD5 
                                                           popEntsMD5
-- by value: use measured value as fitness
calcFitnesses ByValue (Value valueType) cache _ popEntsMD5 = scores
  where
    popEnts = map (snd . fromJust . flip Data.Map.lookup cache) popEntsMD5
    toFitness Nothing = fromIntegral (maxBound :: Int)
    toFitness (Just (EntityDataValue x)) = error "(calcFitnesses::Value) need to fix this"
    toFitness _ = error "(calcFitnesses/toFitness) You're not making any sense"
    tweak HigherIsBetter = negate
    tweak LowerIsBetter = id
    scores = map (tweak valueType . toFitness) popEnts

calcFitnesses _ _ _ _ _ = error "(calcFitnesses) You're not making any sense..."

-- |Translate list of flags into list of 'bits',
-- which indicate whether a flag was on or off.
flagsToBits :: [String] -- ^ list of available flags
            -> [String] -- ^ list of selected flags
            -> [Bool] -- ^ bit list
flagsToBits (p:ps) es = (p `elem` es) : flagsToBits ps es
flagsToBits   []    _ = []

-- |Create flags string for given entity.
mkFlags :: (BaseFlag,[OptFlag]) -- ^ base and selected optimization flags
        -> String -- ^ string with all flags
mkFlags (b,e) = b ++ " " ++ (concat $ intersperse " " e)

-- |Determine key for given entity.
keyFor :: (BaseFlag,[OptFlag]) -- ^ base and selected optimization flags
       -> String -- ^ entity key
keyFor (b,e) = md5sum $ pack $ mkFlags (b,e)

-- |Instance of Entity type class for genetic algorithm
-- 
-- Each COLE entity is represented by a set of integers,
-- which describes which optimizations are selected from the pool.
instance Entity (MD5,BaseFlag,[OptFlag]) Double 
                COLEData ([BaseFlag],[OptFlag]) IO where

    -- |Generate a random entity.
    genRandom (poolBase,poolOpt) seed = return (md5,base,optFlags)
      where
        g        = mkStdGen seed
        nBase    = length poolBase
        nOpt     = length poolOpt
        (i:k:xs) = randoms g
        base     = poolBase !! (i `mod` nBase)
        indices  = take (k `mod` (nOpt+1)) $ map (flip mod nOpt) xs
        optFlags = zipWith (!!) (repeat poolOpt) $ sort $ nub indices
        md5      = keyFor (base,optFlags)
        
    -- |Crossover operator: combine two entities into a new entity.
    -- 
    -- Crossover by mixing flags between entities.
    crossover (_,poolOpt) _ seed (_,b1,e1) (_,b2,e2) = return $ Just e
      where
        g                      = mkStdGen seed
        bits1                  = flagsToBits poolOpt e1
        bits2                  = flagsToBits poolOpt e2
        nOpt                   = length poolOpt
        (b:k:rs)               = randoms g :: [Int]
        mixIs                  = map (flip mod nOpt) 
                               $ take (1 + k `mod` nOpt) rs
        mix (x:xs) (y:ys) i is =   (if i `elem` is then x else y) 
                                 : mix xs ys (i+1) is
        mix [] [] _ _          = []
        mix _  _  _ _          = error "(crossover) ERROR! mix fail!"
        newBits                = mix bits1 bits2 0 mixIs
        newFlags               = map snd $ filter fst $ zip newBits poolOpt
        newBase                = if b `mod` 2 == 0 then b1 else b2
        newMd5                 = keyFor (newBase,newFlags)
        e                      = (newMd5,newBase,newFlags)
        

    -- |Mutation operator: mutate an entity into a new entity.
    -- 
    -- Mutation by multipoint drift: enable/disable some random flags.
    mutation (poolBase,poolOpt) par seed (_,base,e) = return $ Just e'
      where
        g             = mkStdGen seed
        bits          = flagsToBits poolOpt e
        nOpt          = length poolOpt
        nBase         = length poolBase
        k             = round $ 1 / par
        (b:rs)        = take (nOpt+1) $ randoms g :: [Int]
        rs'           = map (flip mod k) rs
        drift (x,bit) = if x == 0 then not bit else bit
        newBase       = if b `mod` k == 0 
                          then poolBase !! (b `mod` nBase) 
                          else base
        newBits       = map drift $ zip rs' bits
        newFlags      = map snd $ filter fst $ zip newBits poolOpt
        newMd5        = keyFor (newBase,newFlags)
        e'            = (newMd5,newBase,newFlags)

    -- |Score an entire population of entites.
    scorePop (COLEData label
                       fitnessType
                       evalType
                       evalResultType
                       kExpPerEnt) universe pop = do
        -- evaluate all entities, by running benchmarks
        -- and aggregating all benchmark results per entity
        let entitiesInfo = map (\(md5,b,e) -> (length e, 
                                               mkFlags (b,e), 
                                               md5)
                               ) pop
            entitiesMD5  = map thrd3 entitiesInfo
        cache <- readCache label
        let entitiesData = map (fetchFromCache cache . thrd3) entitiesInfo
            -- entities to evaluate: unique entities not in cache
            entitiesInfo' = nub $ map snd 
                          $ filter (isNothing . fst) 
                          $ zip entitiesData entitiesInfo
            cachedEntities = filter (isJust . snd) 
                           $ zip (map thrd3 entitiesInfo) entitiesData
            n = length entitiesInfo'
            tn = length pop
        putStrLn "cached entities: \n"
        mapM_ (\(md5,ed) -> putStrLn $ md5 ++ " => " 
                                    ++ show (snd $ fromJust ed)) cachedEntities
        putStrLn $ "\n" ++ (show n) ++ " of " ++ show tn ++ " "
                ++ "entities not in cache, evaluating..."
        -- prepare for evaluation (if needed)
        prepareEval evalType label kExpPerEnt entitiesInfo'
        -- evaluate all non-cached entities
        entitiesData' <- mapM (evaluateFor evalType evalResultType label kExpPerEnt) 
                              entitiesInfo'
        mapM_ (writeToCache cache label) entitiesData'
        putStrLn $ "BLEH!"
        -- determine dominance fitness scores per entity
        -- * strength of entity E is number of entities dominated by E
        let cache' = addToCache cache entitiesData'
            univEntsMD5 = map fst3 universe
            fitnesses = calcFitnesses fitnessType evalResultType
                                      cache' univEntsMD5 entitiesMD5
        putStrLn $ "fitnesses: " ++ show fitnesses
        return $ Just $ map Just fitnesses

    -- |Check whether a given scored entity is perfect.
    isPerfect _ = False

    -- |Show progress made in this generation.
    showGeneration gi (_,archive) = 
         "\nGENERATION " ++ (show gi) ++ " "
      ++ "archive entities:\n\n" 
      ++ ( concat 
         $ intersperse "\n"
         $ map (\(f,m,e) -> m ++ " => " 
                         ++ e ++ " [fitness: " 
                         ++ show f ++ "]")
         $ zip3 fs md5s ents) ++ "\n"
      where
        md5s = map (fst3.snd) archive
        fs = map (fromJust . fst) archive
        ents = map (mkFlags . (\(_,y,z) -> (y,z)) . snd) archive

    -- |Determine whether evolution should continue or not, 
    --  based on lists of archive fitnesses of previous generations.
    --
    --  Note: last archives are at the head of the list.
    --
    --  Continue if progress was made in the last 3 generations,
    --  i.e. if a new entity appears in a more recent archive.
    hasConverged archives = if length archives >= 3
                              then progress lastArchives
                              else True
      where
        lastArchives = take 4 archives
        progress (a:a':as) = if or (zipWith ($) (map (\x -> not . elem x) a) $ repeat a')
                                   then False
                                   else progress (a':as)
        progress _ = True

-- |COLE configuration.
data COLEConfig = COLEConfig {
      getEntitiesCnt :: Int -- ^ number of entities per generation
    , getMaxParOptLevels :: Int -- ^ max. number of Pareto-optimal solutions
    , getMaxColeGenerations :: Int -- ^ max. number of generations
    , getColeLabel :: Label -- ^ label for COLE experiment
    , getFitnessType :: FitnessType -- ^ type of fitness scoring
    , getEvalType :: EvaluationType -- ^ evaluation type
    , getEvalResultType :: EvalResultType -- ^ type of evaluation result
    , getExpCntPerEntity :: Int -- ^ number of experiments per entity
                                --   required for statically sound comparison
    , getBaseFlags :: [BaseFlag] -- ^ available base flags
    , getOptFlags :: [OptFlag] -- ^ available optimization flags
    }

-- |COLE main function.
cole :: StdGen -- ^ random generator
     -> COLEConfig -- ^ COLE configuration
     -> IO [(Maybe Double,(MD5,BaseFlag,[OptFlag]))] -- best entities
cole g cfg = do
    let nPop = getEntitiesCnt cfg
        nArchive = getMaxParOptLevels cfg
        maxGen = getMaxColeGenerations cfg
        label = getColeLabel cfg
        ft = getFitnessType cfg
        et = getEvalType cfg
        ert = getEvalResultType cfg
        kPerEnt = getExpCntPerEntity cfg
        baseFlags = getBaseFlags cfg
        optFlags = getOptFlags cfg
        gaCfg = GAConfig 
                       nPop -- population size
                       nArchive -- archive size (best entities to keep track of)
                       maxGen -- maximum number of generations
                       0.8 -- crossover rate (% of entities by crossover)
                       0.2 -- mutation rate (% of entities by mutation)
                       0.0 -- parameter for crossover operator 
                       0.1 -- parameter for mutation operator
                       False -- whether or not to use checkpointing
                       True -- rescore archive in each generation
        coleData = COLEData label ft et ert kPerEnt
                
    evolveVerbose g gaCfg (baseFlags,optFlags) coleData 
