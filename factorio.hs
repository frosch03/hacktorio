{-# LANGUAGE NoMonomorphismRestriction, DeriveGeneric, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
module Factorio
where

import Data.Maybe
import Data.List (groupBy, sortOn)
import Data.HashMap ((!))
import qualified Data.HashMap as HM hiding ((!))
import Control.Monad.Reader

import Data.Graph.Inductive.Graph
import System.Console.CmdArgs (Data, Typeable)
    
import Recipes
import RGraph
import Commandline


    
-- type RecipeHM = HM.Map String Recipe

-- toHMrecipe :: [Recipe] -> RecipeHM
-- toHMrecipe = foldr (\nxt result -> HM.insert (name nxt) nxt result) HM.empty 


-- totalRaw'' :: RecipeHM -> [(Float, String)] -> [(Float, String)]
-- totalRaw'' rhm list 
--     | result == list
--     = list

--     | otherwise
--     = totalRaw'' rhm result 
--     where
--       result = concatMap (totalRaw' rhm) list

-- totalRaw' :: RecipeHM -> (Float, String) -> [(Float, String)]
-- totalRaw' rhm (fac, itm)
--     | lookupResult == Nothing
--     = [(fac, itm)]

--     | otherwise
--     = let p     = (product_amount . head . products . fromJust $ lookupResult)
--       in totalRaw'' rhm $ map (\i -> (fac * ((ingredient_amount i) / p), ingredient_name i)) (ingredients . fromJust $ lookupResult)
--     where
--       lookupResult = HM.lookup itm rhm 

-- totalRaw :: RecipeHM -> String -> [(Float, String)]
-- totalRaw rhm itm
--     = map (foldl1 (\a b -> ((fst a) + (fst b), snd a))) tmp2
--     where
--       tmp1 = totalRaw' rhm (1, itm)
--       tmp2 = groupBy (\a b -> snd a == snd b) $ sortOn snd tmp1


data Environment
    = Environment
      { inputFile :: String
      , atoms :: [String]
      }
    deriving (Show, Data, Typeable)

data Command
    = CreateDot
      { nodeId :: Node }

    | NameFromId
      { nodeId :: Node }

    | IdFromName
      { nodeName :: String }

    | CombinedAmount
      { nodeId :: Node }
    deriving (Show, Data, Typeable)

type Env  = ReaderT Environment IO
type Prog = ReaderT Command Env

runProg :: Prog ()
runProg = do
  filename <- lift $ asks inputFile
  atms <- lift $ asks atoms
  rs <- liftIO (getRecipesFromFile filename)
  cmd <- ask
  result <- (case cmd of
            (CreateDot nd) -> do
              let ig  = itemRecipesToGraph rs
                  bg  = getCleanBuildGraph atms ig nd
                  dot = createDotGraph bg
              liftIO $ writeFile ("/tmp/" ++ (fromJust $ lab ig nd) ++ ".dot") dot
            (NameFromId nd) -> do
              let ig  = itemRecipesToGraph rs
              liftIO . putStrLn . show . lab ig $ nd
            (IdFromName nn) -> do
              let ig  = itemRecipesToGraph rs
                  gn  = (\nme -> filter ((== nme) . snd) $ labNodes ig)
              liftIO . putStrLn . show . fmap fst . listToMaybe . gn $ nn
            (CombinedAmount nd) -> do
              let ig = itemRecipesToGraph rs
                  bg = getCleanBuildGraph atms ig nd
                  m  = soleMap bg
              liftIO . putStrLn . show . map (\(x, y) -> (fromJust $ lab ig y, x)) . simplify $ combAmnt m (m!nd)
           )
  return result
      
-- Environment "/tmp/export.json" ["iron-plate","copper-plate","steel-plate", "uranium-238", "uranium-235"]
main :: IO ()
main
    = do argus <- parseArgs
         result <- (case argus of
                     (Create_Dot fn at nd) -> runReaderT (runReaderT runProg $ CreateDot nd) $ Environment fn at
                     (Name_From_Id fn at nd) -> runReaderT (runReaderT runProg $ NameFromId nd) $ Environment fn at
                     (Combined_Amount fn at nd) -> runReaderT (runReaderT runProg $ CombinedAmount nd) $ Environment fn at
                     (Id_From_Name fn at nd) -> runReaderT (runReaderT runProg $ IdFromName nd) $ Environment fn at
                  )
         return result

       
