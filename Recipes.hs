{-# LANGUAGE DeriveGeneric #-}
module Recipes
where

import Data.Aeson
import GHC.Generics
import Text.Regex.PCRE ((=~))
import Data.List (intercalate)
import Data.ByteString.Lazy as BSL (ByteString(), readFile)


data Type
    = Item | Fluid
      deriving (Generic, Eq, Show)

data Ingredient
    = Ingredient
      { ingredient_name :: String
      , ingredient_amount :: Float
      , ingredient_type :: String
      } deriving (Generic, Eq)

data Product
    = Product
      { product_name :: String
      , product_amount :: Float
      , product_type :: String
      } deriving (Generic, Eq)

data Recipe
    = Recipe
      { name :: String
      , ingredients :: [Ingredient]
      , products :: [Product]
      , energy :: Float
      } deriving (Generic, Eq)

data Recipes
    = Recipes
      { recipes :: [Recipe]
      } deriving (Show, Generic)

instance FromJSON Type
instance FromJSON Ingredient
instance FromJSON Product
instance FromJSON Recipe
instance FromJSON Recipes




getJSON :: String -> IO ByteString
getJSON filename = BSL.readFile filename


instance Show Ingredient where
    show (Ingredient n i _) = (show i) ++ "x " ++ n

instance Show Product where
    show (Product n o _) = (show o) ++ "x " ++ n

instance Show Recipe where
    show (Recipe _ is os e) = ('(':) $  (intercalate " + " (map show is)) ++
                               ") -" ++ (show e) ++ "-> (" ++ (intercalate ", "  (map show os)) ++ ")"

getRecipesFromFile :: String -> IO [Recipe]
getRecipesFromFile filename
    = do
       ers <- (eitherDecode <$> (getJSON filename)) :: IO (Either String Recipes)
       return $ either (\x -> error $ "error while parsing json: " ++ x) (\(Recipes rs) -> rs) ers
    where
      


type RecipeFilter = Recipe -> Bool

nameIs :: String -> RecipeFilter
nameIs s 
    = ((==) s) . name

nameMatches :: String -> RecipeFilter
nameMatches s
    = ((flip (=~)) s) . name

with :: [Recipe] -> RecipeFilter -> [Recipe]
with rs rfltr
    = filter rfltr rs

without :: [Recipe] -> RecipeFilter -> [Recipe]
without rs rfltr
    = filter (not . rfltr) rs





      
