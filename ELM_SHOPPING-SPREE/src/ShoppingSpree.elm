module Main exposing (main)

import Basics 
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import GraphicSVG.EllieApp exposing (GetKeyState,gameApp, ellieAppWithTick, EllieAppWithTick)
import String
import List
import Array
import Maybe
import Random
import Platform.Cmd
import Platform.Sub
import Dict 
import Url 
import Debug


import WrdFromLouisbourg2019Aug14

-- Retrieve a list of Words from the file (function from other file)
words = 
    WrdFromLouisbourg2019Aug14.words

-- Function for extracting relevant information from Words by decoupling tuples into appropriate key/value pairs
wordsToPairs : ((String, String, String), (String, String, (a -> List (Shape msg)) ) ) -> (String, (a -> List (Shape msg)))
wordsToPairs ( (user, theirWord, grade), (word, school, shapes) ) = (theirWord, shapes) 
    
-- Apply the above function to the Words
betterWords = List.map wordsToPairs words


-- Make a dictionary from the Words
betterWordDict =
    Dict.fromList betterWords


-- We don't get the prices from the Louisbourg Words file itself; we cross-reference the Words' "theirWord" with an additional 
-- dictionary, seen here
louisbourg2019WordsDict = Dict.fromList [
          ("cannon", 19)
        , ("fishing net", 10)
        , ("quill", 1)
        , ("sheep", 11)
        , ("fish", 5)
        , ("turkey egg", 3)
        , ("bread", 2)
        , ("fan", 7)
        , ("candle", 1)
        , ("barrel", 13)
        , ("cannon balls", 17)
        ] 

-- Make an ordered dictionary list of current compatible Words with this game 
costWithWordDict =
    Dict.fromList <| List.indexedMap (\n w -> ( n, w )) (Dict.toList (louisbourg2019WordsDict))



-- Extract a compatible word from our game dictionary
wordFromOurDictionary i = 
    case (Dict.get i costWithWordDict) of
        Just (ourWord, cost) ->
            ourWord

        Nothing ->
            ""
-- If our word is truly compatible, we get the corresponding shape from the better Words 
getWord ourWord =
    case (Dict.get ourWord betterWordDict) of
        Just shapes ->
            shapes

        Nothing ->
            \ _ -> [circle 10 |> filled red]

-- At index "index" we get "theirWord"; note that we can specify which dictionary we are extracting from, and that dictionary
-- must be provided as an argument
wordTake index dictionary = 
    case (Dict.get index dictionary) of
        Just (theirWord, shapes) ->
            theirWord

        Nothing ->
            ""

sign = group [signPost
           , signBackground
           , signWhiteSpace
           
           , text "GARAGE" |> size 16.2 |> filled white |> move(-33.5, 20)
           , text "SALE" |> size 26 |> filled white |> move(-32, 0)
           
           

        
           ] |> move(300, -170)
    

signBackground = square 70
             |> filled (rgb 204 0 0)
             |> makeTransparent 1  -- value between 0 and 1

signPost = rect 10 30
            |> filled grey
            |> move(0, -30)


signWhiteSpace = roundedRect 60 20 2
             |> filled white
             |> move (0,-20)

table = group [ 
             tableTop               
               |> move (-30,0)
           , vertRect
             |> move (-100,-1)
           , vertRect
               |> move (-120,-1)
           , horizRect
             |> move (-110,2.5)
           , vertRect
             |> move (60,-1)
           , vertRect
               |> move (40,-1)
           , horizRect
             |> move (50,2.5)
           , polygon [(-19,-1),(-4,0),(18,81),(6,81)]  
             |> filled black
             |> move (-170,-89)
             |> scale 0.6
           , polygon [(-19,-1),(-4,0),(18,81),(6,81)]  
             |> filled black
             |> move (-70,-89)    
             |> mirrorX
             |> scale 0.6
           
           ] |> scale 3.7 |> move (9,-90)



vertRect = rect 0.5 7
             |> filled black
             |> makeTransparent 0.5  -- value between 0 and 1
             

horizRect = rect 20 0.5
             |> filled black
             |> makeTransparent 0.5  -- value between 0 and 1
             

tableTop = rect 195 10
              |> filled (rgb 127 80 25) 

sun = circle 40 
              |> filled yellow
              |> move(-405, 200)

              
              

  
-- Essentially, the entire view model, since we are using a collage. Transparency effects depending on state of application are
-- omnipresent
myShapes model = [ 
                 rect 1000 550 |> filled lightBlue |> move(0, 120)                                
                , sun
                , rect 1000 300 |> filled green |> move(0, -300)
                , makeTransparent model.playingTransparency sign
                  -- Miscellaneous score trackers and large text
                , text "Shopping Spree" |> size 40 |> filled red |> move(-115, 200)
                , text model.content
                    |> size 30 |> filled black |> move(-350, 150)
                , makeTransparent model.playingTransparency (text 
                    ("You have " ++ String.fromInt(model.cashOnHand) ++ " livres to spend") 
                    |> size 15 |> filled black |> move(320, -30))
                , makeTransparent model.playingTransparency (text ("Total items bought: " ++ String.fromInt(model.itemsBought)) 
                    |> size 15 |> filled black |> move(320, -60))
                , makeTransparent model.playingTransparency (text ("Best record: " ++ String.fromInt(model.yourRecord)) 
                    |> size 15 |> filled black |> move(320, -90))
                , makeTransparent model.winTextTransparency (
                    text model.winOrLoseMessage |> size 30 |> filled red |> move(-215, -200))
                
                -- Retrieve and display first Word
                , makeTransparent model.playingTransparency (group [ (move (-400, 0) (clip (square 100 |> outlined (solid 5) red) 
                       (Maybe.withDefault (circle 10 |> filled red) 
                       (List.head ((getWord (wordFromOurDictionary model.imageIndex)) model)))))                                       
                   ]) 
                
                -- Retrieve and display second Word
                , makeTransparent model.playingTransparency (group [ move (-200, 0) (clip (square 100 |> outlined (solid 5) red) 
                       (Maybe.withDefault (circle 10 |> filled red) 
                       (List.head ((getWord (wordFromOurDictionary model.imageIndex2)) model))))                    
                   ]) 

                -- Retrieve and display third Word
                , makeTransparent model.playingTransparency (group [ move (0, 0) (clip (square 100 |> outlined (solid 5) red) 
                       (Maybe.withDefault (circle 10 |> filled red) 
                       (List.head ((getWord (wordFromOurDictionary model.imageIndex3)) model))))                    
                   ]) 

                -- Retrieve and display fourth Word
                , makeTransparent model.playingTransparency (group [ move (200, 0) (clip (square 100 |> outlined (solid 5) red) 
                       (Maybe.withDefault (circle 10 |> filled red) 
                       (List.head ((getWord (wordFromOurDictionary model.imageIndex4)) model))))                    
                   ])  

                -- Displays $n for each image, where n is an int                 
                , makeTransparent model.playingTransparency (text ("" ++ String.fromInt(
                    Maybe.withDefault 0 (Dict.get (wordFromOurDictionary model.imageIndex) louisbourg2019WordsDict)) ++ " livres"
                    ) |> size 10 |> filled black |> move(-471, -63))
                , makeTransparent model.playingTransparency (text ("" ++ String.fromInt(
                    Maybe.withDefault 0 (Dict.get (wordFromOurDictionary model.imageIndex2) louisbourg2019WordsDict)) ++ " livres"
                    ) |> size 10 |> filled black |> move(-270, -63))
                , makeTransparent model.playingTransparency (text ("" ++ String.fromInt(
                    Maybe.withDefault 0 (Dict.get (wordFromOurDictionary model.imageIndex3) louisbourg2019WordsDict)) ++ " livres"
                    ) |> size 10 |> filled black |> move(-70, -63))
                , makeTransparent model.playingTransparency (text ("" ++ String.fromInt(
                    Maybe.withDefault 0 (Dict.get (wordFromOurDictionary model.imageIndex4) louisbourg2019WordsDict)) ++ " livres"
                    ) |> size 10 |> filled black |> move(131, -63))
                 
                -- Start game button
                , makeTransparent (abs (model.playingTransparency - 1)) (group [ 
                        move (380, 20) (roundedRect 100 20 5 |> filled green |> addOutline (solid 1) purple )
                      , text "Start game"
                      |> size 18 |> filled black |> move(340, 15) 
                   ]) |> notifyTap NewGame

                -- Reset button
                , makeTransparent model.playingTransparency (group [ 
                          move (380, 60) (roundedRect 100 20 5 |> filled yellow |> addOutline (solid 1) purple )
                        , text "Reset game"
                        |> size 18 |> filled black |> move(340, 55) 
                   ]) |> notifyTap ResetGame
                
                -- "Buy {item #1}" button and associated text
                , makeTransparent model.playingTransparency (group [ move (-390, -60) (rect 80 10 |> filled (rgb 194 214 214)
                |> addOutline (solid 0.5) red ) 
                , text ("Buy " ++ wordFromOurDictionary model.imageIndex) |> size 10 |> filled black |> move(-425, -63)
                    ]) |> notifyTap (CheckIfGameOver model.imageIndex)

                -- "Buy {item #2}" button and associated text
                , makeTransparent model.playingTransparency (group [ move (-190, -60) (rect 80 10 |> filled (rgb 194 214 214) 
                |> addOutline (solid 0.5) red ) 
                , text ("Buy " ++ wordFromOurDictionary model.imageIndex2) |> size 10 |> filled black |> move(-225, -63)
                    ]) |> notifyTap (CheckIfGameOver model.imageIndex2)

                -- "Buy {item #3}" button and associated text
                , makeTransparent model.playingTransparency (group [ move (10, -60) (rect 80 10 |> filled (rgb 194 214 214) 
                |> addOutline (solid 0.5) red ) 
                , text ("Buy " ++ wordFromOurDictionary model.imageIndex3) |> size 10 |> filled black |> move(-25, -63)
                    ]) |> notifyTap (CheckIfGameOver model.imageIndex3)

                -- "Buy {item #4}" button and associated text
                , makeTransparent model.playingTransparency (group [move (210, -60) (rect 80 10 |> filled (rgb 194 214 214) 
                |> addOutline (solid 0.5) red ) 
                , text ("Buy " ++ wordFromOurDictionary model.imageIndex4) |> size 10 |> filled black |> move(175, -63)
                 ]) |> notifyTap (CheckIfGameOver model.imageIndex4)

                , makeTransparent model.playingTransparency table 
                ]       




type alias Model = 
    { time : Float,
      content : String, 
      imageIndex : Int,
      imageIndex2 : Int,
      imageIndex3 : Int,
      imageIndex4 : Int,
      wordsList : List (String, Shape ()),
      playingTransparency : Float,
      winTextTransparency : Float,
      cashOnHand : Int,
      cost : Int,
      itemsBought : Int,
      yourRecord : Int,    
      winOrLoseMessage : String,
      didWeStart : Bool,
      dud : String      
    }

-- init is a function that returns a new model along with a command msg
init : () -> (Model, Cmd Msg)
init _ = ({      
    time = 0,
    content = "Spend exactly 100 livres buying the least amount of stuff you can",
    imageIndex = 0,
    imageIndex2 = 0,
    imageIndex3 = 0,
    imageIndex4 = 0,
    playingTransparency = 0,
    winTextTransparency = 0,
    winOrLoseMessage = "",
    wordsList = [],      
    cashOnHand = 100,
    cost = 0,
    itemsBought = 0,    
    yourRecord = 0,
    didWeStart = False,
    dud = ""
    }, Cmd.none)

type Msg = Tick Float GetKeyState 
         | UpdateIndex
         | RandomIndex Int
         | RandomIndex2 Int
         | RandomIndex3 Int
         | RandomIndex4 Int
         | CashJustSpent Int
         | ResetGame
         | NewGame
         | Win
         | Lose Int
         | CheckIfGameOver Int                 
         | Dud 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick t _ -> ({ model | time = t }, Cmd.none)
        UpdateIndex ->
            case (model.didWeStart) of
                True -> 
                    -- Generate random numbers iff the game is running; generate indices only for game compatible words
                    (model, Cmd.batch [
                    Random.generate RandomIndex (Random.int 0 (List.length (Dict.toList louisbourg2019WordsDict) - 1)),
                    Random.generate RandomIndex2 (Random.int 0 (List.length (Dict.toList louisbourg2019WordsDict) - 1)),
                    Random.generate RandomIndex3 (Random.int 0 (List.length (Dict.toList louisbourg2019WordsDict) - 1)),
                    Random.generate RandomIndex4 (Random.int 0 (List.length (Dict.toList louisbourg2019WordsDict) - 1))            
                        ]                      
                            
                    )
                False ->
                    (model, Cmd.batch [])

        -- Assign random numbers to the model
        RandomIndex randomIndex ->
            ({model | imageIndex = randomIndex}, Cmd.none)
        RandomIndex2 randomIndex2 ->
            ({model | imageIndex2 = randomIndex2}, Cmd.none)
        RandomIndex3 randomIndex3 ->
            ({model | imageIndex3 = randomIndex3}, Cmd.none)
        RandomIndex4 randomIndex4 ->
            ({model | imageIndex4 = randomIndex4}, Cmd.none)

        -- Subtract the cost of the most recent item purchased from your cash on hand
        CashJustSpent array_index -> 
            update UpdateIndex {model | cashOnHand = model.cashOnHand - 
               (Maybe.withDefault 0 (Dict.get (wordFromOurDictionary array_index) louisbourg2019WordsDict))
            , itemsBought = model.itemsBought + 1}

        ResetGame -> 
            ({model | cashOnHand = 100, itemsBought = 0}, Cmd.none)
        NewGame -> 
            case (model.didWeStart) of 
                False ->
                    update UpdateIndex {model | cashOnHand = 100, itemsBought = 0, 
                    winTextTransparency = 0, playingTransparency = 1, didWeStart = True}
                True -> -- Disable function from doing anything harmful using the Dud model
                    update Dud model
        Win -> 
            ({model |               
              playingTransparency = 0,
              winTextTransparency = 1,
              didWeStart = False,
              yourRecord = if (model.itemsBought + 1 >= model.yourRecord) && model.yourRecord /= 0 
                           then model.yourRecord 
                           else model.itemsBought + 1,
              winOrLoseMessage = "Congratulations. You won in " 
                ++ (String.fromInt(model.itemsBought + 1)) ++ " turns!"
              }, Cmd.none)
        Lose array_index -> 
            -- If you spend more cash than you have, you lose. #LifeLessons. If not, pipeline to the CashJustSpent model
            if model.cashOnHand - (Maybe.withDefault 0 
                (Dict.get (wordFromOurDictionary array_index) louisbourg2019WordsDict)) < 0
            then update Dud {model |               
              playingTransparency = 0,
              winTextTransparency = 1,
              didWeStart = False,              
              winOrLoseMessage = "You spent too much! Play again?"
              }
            else update (CashJustSpent array_index) model
        CheckIfGameOver array_index -> 
            -- Check if your most recent purchase ends the game. If not, pipeline to the Lose model
            if model.cashOnHand - (Maybe.withDefault 0 
                (Dict.get (wordFromOurDictionary array_index) louisbourg2019WordsDict)) == 0 
            && model.itemsBought > 0 
            then update Win model 
            else update (Lose array_index) model
        Dud ->
            ({model | dud = ""}, Cmd.none)


view : Model -> Collage Msg
view model =
    -- SVG canvas size
    collage 1000 500 (myShapes model)
        
main : EllieAppWithTick () Model Msg
main = ellieAppWithTick Tick
        {   -- Who even knows honestly
            init = init
        ,   subscriptions = \ _ -> Sub.none
        ,   update = update        
        ,   view = \model -> { body = view model, title = "Shopping Spree" }
        
        
        }



