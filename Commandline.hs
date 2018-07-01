{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
module Commandline
where

import System.Console.CmdArgs hiding (name)
import Data.Graph.Inductive.Graph (Node)

data Arguments
    = Create_Dot
      { argFilename :: String
      , argAtoms :: [String]
      , node_id :: Node }
    | Name_From_Id
      { argFilename :: String
      , argAtoms :: [String]
      , node_id :: Node }
    | Id_From_Name
      { argFilename :: String
      , argAtoms :: [String]
      , node_name :: String }
    | Combined_Amount
      { argFilename :: String
      , argAtoms :: [String]
      , node_id :: Node }
    deriving (Show, Data, Typeable)

atomItems :: [String]
atomItems = ["iron-plate","copper-plate","steel-plate", "uranium-238", "uranium-235"]
nodeIdHelp :: String
nodeIdHelp = "Integer id of the item"
cmdCreateDot, cmdNameFromId, cmdCombinedAmount, cmdIdFromName :: Arguments
cmdCreateDot 
    = Create_Dot 
      { argFilename = "/tmp/export.json" &= help "Factorio recieps (default: /tmp/export.json)" &= typFile
      , argAtoms = atomItems &= help ("Atomar items (default:" ++ foldl (\rs nx -> rs ++ "\n- " ++ nx) "" atomItems ++ ")")
      , node_id = def &= help nodeIdHelp &= typ "INT"
      } &= help "Write the graph into a .dot file"
cmdNameFromId 
    = Name_From_Id 
      { argFilename = "/tmp/export.json" &= help "Factorio recieps (default: /tmp/export.json)" &= typFile
      , argAtoms = atomItems &= help ("Atomar items (default:" ++ foldl (\rs nx -> rs ++ "\n- " ++ nx) "" atomItems ++ ")")
      , node_id = def &= help nodeIdHelp &= typ "INT"
      } &= help "Return the name of the item with given id "
cmdCombinedAmount 
    = Combined_Amount 
      { argFilename = "/tmp/export.json" &= help "Factorio recieps (default: /tmp/export.json)" &= typFile
      , argAtoms = atomItems &= help ("Atomar items (default:" ++ foldl (\rs nx -> rs ++ "\n- " ++ nx) "" atomItems ++ ")")
      , node_id = def &= help nodeIdHelp &= typ "INT"
      } &= help "Calculate the combined amount of atomar r~esources of the item"
cmdIdFromName 
    = Id_From_Name 
      { argFilename = "/tmp/export.json" &= help "Factorio recieps (default: /tmp/export.json)" &= typFile
      , argAtoms = atomItems &= help ("Atomar items (default:" ++ foldl (\rs nx -> rs ++ "\n- " ++ nx) "" atomItems ++ ")")
      , node_name = def &= help "The item name" &= typ "STRING"
      } &= help "Return the id of the item with given name"

parseArgs
    = cmdArgs (modes [cmdCreateDot, cmdNameFromId, cmdCombinedAmount, cmdIdFromName]
              &= help "Return various information of factorio recieps"
              &= program "hacktorio"
              &= summary "haskell client that gives info from reciepes list\nv 0.1 - by frosch03")
