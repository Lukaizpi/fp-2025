module Lib1
    ( examples, Command(..), Action(..)
    ) where


data Dumpable = Examples
  deriving Show



data Action
    = Pass String String     
    | Dribble String [Dribbleresult]         
    | Foul String String (Maybe Card)   
    | Shot String            
    | Goal String String
    | Steal String            
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
    , Enter "Di María"
    , Enter "Mbappé"
    , Play "Messi"
        [ Pass "Messi" "Di María"
        , Shot "Di María"
        , Goal "Di María" "Messi"
        ]
    , Substitution "Mbappé" "Griezmann"
    , Play "Messi"
        [ Pass "Messi" "Griezmann"
        , Goal "Griezmann" "Messi"
        ]
    , ShowActivePlayers
    , ShowStats "Messi"
    , Dump Examples
    ]