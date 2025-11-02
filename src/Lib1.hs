module Lib1
  ( examples
  , Command(..)
  , Action(..)
  , Dribbleresult(..)
  , Card(..)
  , Dumpable(..)
  ) where


data Dumpable = Examples
  deriving Show



data Action
    = Pass String String     
    | Dribble String Dribbleresult         
    | Foul String String 
    | HardFoul String String Card 
    | Shot String            
    | Goal String String
    | Steal String 
    | CompositeAction [Action]           
    deriving Show

data Dribbleresult = Success | Fail  deriving Show

data Command
    = Play String [Action]  
    | Enter String  
    | Substitution String String
    | ShowStats String
    | ShowActivePlayers
    | Dump Dumpable
    deriving Show

data Card = Yellow | Red deriving Show


examples :: [Command]
examples =
    [ Enter "Messi"
    , Enter "Di Maria"
    , Play "Messi"
        [ Pass "Messi" "Di Maria"
        , CompositeAction
            [Shot "Di Maria"
            , Goal "Di Maria" "Messi"
            ]
        ]
    , ShowStats "Messi"
    ]

