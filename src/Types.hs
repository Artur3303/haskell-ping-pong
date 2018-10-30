module Types where

data Hex = Maybe Color deriving (Show, Eq)

data Row = [Hex] deriving (Show)

data Well = [Row] deriving (Show)

data State = {} deriving (Show)

data Piece = 
