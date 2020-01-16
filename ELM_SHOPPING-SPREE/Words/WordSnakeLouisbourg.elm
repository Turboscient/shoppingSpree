import GraphicSVG exposing(..)
import GraphicSVG.EllieApp exposing(..)
import Url exposing(..)
import Browser exposing(..)
import Random exposing(Generator)
import Dict

import WrdFromLouisbourg2019Aug14

words = WrdFromLouisbourg2019Aug14.words -- WrdFromElmJr2019May15.words ++ WrdFromElmJr2019May16.words ++ Wrd1558528511662312.words -- Wrd1526388449251721.words ++ Wrd1526642931059256.words ++ Wrd1526743215065487.words

wordDict =
    Dict.fromList <| List.indexedMap (\n w -> ( n, w )) words

getWord i =
    case (Dict.get i wordDict) of
        Just ( (user, theirWord, grade), (word, school, shapes) ) ->
            (theirWord, shapes)

        Nothing ->
            ("", \_ -> [])
-- snake game

wordLine : Int -> List Char -> Shape Msg
wordLine n chars =
    let
        spacing = 20
        charList = chars ++ List.repeat (n - List.length chars) '_'
    in
    group <|
    List.indexedMap
        (\nn char ->
            text (String.fromChar char)
                |> size 30
                |> fixedwidth
                |> centered
                |> filled black
                |> move (toFloat nn * spacing - toFloat (n-1) * (spacing/2), 0)
        )
        charList

myShapes model =
  let
    foodChar =
      case List.head model.randWord of
        Just ch -> ch
        _ -> ' '
  in
  [
    group
      [graphPaper 10 |> move (-5,-5)
      ,  renderSnake model.snake (('>', Right)::model.snakeMeta) model.direction model.time
      ,  group
          [circle 4 |> filled red
          ,  text (String.fromChar model.nextChar)
              |> fixedwidth
              |> size 6
              |> centered
              |> filled black
              |> move(0.4,-1.4)
          ,  text (String.fromChar model.nextChar)
              |> fixedwidth
              |> size 6
              |> centered
              |> filled white
              |> move(0,-1.2)
          ] |> move (toFloat <| (Tuple.first model.food) * 10, toFloat <| (Tuple.second model.food) * 10)
      ]
      |> clip (rect (10*width) (10*height) |> ghost)
      |> move (-40,0)
 ,  text "Eat the right letters to spell the word." |> size 5|> centered |> filled black |> move (0,54)
 ,  group (model.shape { time = model.time })
      |> clip (square 100 |> ghost)
      |> scale 0.6
      |> move (55,5)
      |> addOutline (solid 5) darkGray
 ,  wordLine (String.length model.wordStr) model.found
      |> scale 0.5
      |> move (0, -58)
 ,  text ("Score: " ++ String.fromInt model.score)
      |> size 8
      |> filled black
      |> move(25,37)

 ]

getShape idx = case Dict.get idx wordDict of
                  Nothing -> \ _ -> [circle 10 |> filled red]
                  Just ( (user, theirWord, grade), (word, school, shapes) ) -> shapes
renderSnake snake snakeMeta headDir time =
  group <| List.map2 (\(x,y) (char,dir) ->
                            (if char == '>' then
                              group
                                [
                                  wedge 4.25 (0.9 + 0.1 * sin (time * 18.8495559215))
                                    |> filled green
                                , wedge 3.25 (0.8 + 0.1 * sin (time * 18.8495559215))
                                    |> filled darkGreen

                                ]|> rotate (case headDir of
                                                Right -> pi
                                                Left -> 0
                                                Up -> 1.5*pi
                                                Down -> 0.5*pi)
                            else if char == '<' then
                              group
                                [
                                  roundedRect 6.5 6.5 2
                                    |> filled green
                                , roundedRect 4.5 4.5 1.5
                                    |> filled darkGreen
                                , roundedRect 2.5 2.5 1.5
                                    |> filled (rgb 255 255 0)
                                ]|> rotate (case dir of
                                            Right -> pi
                                            Left -> 0
                                            Up -> 0.5*pi
                                            Down -> 1.5*pi)
                            else
                              group
                                [roundedRect 8.5 8.5 2
                                    |> filled green
                                ,roundedRect 6.5 6.5 1.5
                                    |> filled darkGreen
                                    , text (String.fromChar char)
                                        |> fixedwidth
                                        |> size 4
                                        |> centered
                                        |> filled (rgb 255 255 0)
                                        |> move(0,-1)]
                                    ) |> move (toFloat x * 10, toFloat y * 10)
                          ) snake snakeMeta
type Msg =
  Tick Float GetKeyState
  | OnUrlChange Url.Url
  | OnUrlRequest Browser.UrlRequest
  | NewFood (Int,Int,Char)
  | NewWord (String, List Char, { time : Float } -> List (Shape Msg))


type Direction =
  Up | Down | Left | Right

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick t (_,(x,y),_) ->
      let
        newModel = { model | time = t, direction = updateDirection (x,y) model.direction }
      in
        if (model.time - model.lastTime) > 1 / sps then
          updateSnake newModel
        else (newModel, Cmd.none)
    OnUrlChange _ -> (model, Cmd.none)
    OnUrlRequest _ -> (model, Cmd.none)
    NewFood (x,y, char) -> ({ model | food = (x,y), nextChar = char }, Cmd.none)
    NewWord (word, newList, shapes) ->
      let
        newModel = { model | wordStr = word, randWord = newList, shape = shapes }


      in

      ( newModel
      , newFood newModel
      )

foodGenerator : Model -> Generator (Int,Int,Char)
foodGenerator model =
  let
    nextChar =
      case List.head model.randWord of
        Just ch -> ch
        _ -> ' '


  in

  Random.map3 (\a b c -> (a,b,c))
    (Random.int (-width//2) (width//2))
    (Random.int (-height//2) (height//2))
    (randomChar model)

randomChar : Model -> Generator Char
randomChar model =
  let
    nextChar =
      case List.head model.randWord of
        Just ch -> ch
        _ -> ' '

  in

  (Random.weighted (50, True) [ (50, False) ]) |> Random.andThen
      (\correct -> if correct then Random.constant nextChar else randomLetter)

newFood model =
  Random.generate NewFood <| foodGenerator model

getRandomWord : Cmd Msg
getRandomWord =
    Random.generate
        NewWord
            <| (Random.int 0 (Dict.size wordDict - 1) |> Random.map
                        (\i ->
                          let
                            (word, shapes) = getWord i
                            wordChars = String.toList word
                        in
                            (word, wordChars, shapes)))

randomLetter = Random.weighted (12.02,'e')
   [ (9.10,'t')
   , (8.12,'a')
   , (7.68,'o')
   , (7.31,'i')
   , (6.95,'n')
   , (6.28,'s')
   , (6.02,'r')
   , (5.92,'h')
   , (4.32,'d')
   , (3.98,'l')
   , (2.88,'u')
   , (2.71,'c')
   , (2.61,'m')
   , (2.30,'f')
   , (2.11,'y')
   , (2.09,'w')
   , (2.03,'g')
   , (1.82,'p')
   , (1.49,'b')
   , (1.11,'v')
   , (0.69,'k')
   , (0.17,'x')
   , (0.11,'q')
   , (0.10,'j')
   , (0.07,'z')
   ]

width = 11
height = 9
timeBetweenResets = 12


updateSnake : Model -> (Model, Cmd Msg)
updateSnake model =
  let
    (shx,shy) = case (List.head model.snake) of
                  Just (sx,sy) -> (sx,sy)
                  Nothing -> (0,0)
    (nhxx,nhyy) = case model.direction of
                  Left  -> (shx - 1, shy)
                  Right -> (shx + 1, shy)
                  Up    -> (shx, shy + 1)
                  Down  -> (shx, shy - 1)
    (nhx, nhy) = (if nhxx > width // 2 then -width // 2 else if nhxx < -width//2 then width//2 else nhxx,
                if nhyy > height // 2 then -height // 2 else if nhyy < -height//2 then height//2 else nhyy)
    gotFood = (nhx,nhy) == model.food
    gotCorrect = gotFood && model.nextChar == nextChar
    snakeTail = if gotFood then
                  model.snake
                else
                 (List.take (List.length model.snake-1) model.snake)
    nextChar =
      case List.head model.randWord of
        Just ch -> ch
        _ -> ' '
    gotLetters =
      if gotCorrect then
        if gotWord then
          []
        else
          model.found ++ [nextChar]
      else
        model.found
    remainingLetters =
      if gotCorrect then
        List.drop 1 model.randWord
      else
        model.randWord
    gotWord = List.length model.randWord == 1 && gotCorrect
    newScore =
      if gotFood && not gotCorrect then
        model.score - 2
      else if gotFood then
        model.score + 1
      else
        model.score

    newModel =
      { model |
        snake = (nhx,nhy)::snakeTail
      , snakeMeta =
          if gotFood && gotCorrect then
            (nextChar, model.direction)::model.snakeMeta
          else
            model.snakeMeta
      , lastTime = model.time
      , timeUntilNext = if model.timeUntilNext == 0 || gotFood then timeBetweenResets else model.timeUntilNext - 1
      , found = gotLetters
      , randWord = remainingLetters
      , score = newScore
      }
  in
    if List.member (nhx,nhy) snakeTail then (init, getRandomWord) else
      (newModel
      , Cmd.batch [ if gotFood then newFood newModel else Cmd.none
                  , if model.timeUntilNext == 0 then newFood newModel else Cmd.none
                  , if gotWord then getRandomWord else Cmd.none
                  ] )

updateDirection : (Float,Float) -> Direction -> Direction
updateDirection (x,y) oldDir =
  case (round <| x+1,round <| y+1,oldDir) of
    (0,_,Up)   -> Left
    (0,_,Down) -> Left
    (2,_,Down)  -> Right
    (2,_,Up)    -> Right
    (_,0,Left) -> Down
    (_,0,Right)-> Down
    (_,2,Left) -> Up
    (_,2,Right)-> Up
    _      -> oldDir

sps = 3 --squares per second

type alias Model =
  {
    time : Float
  , lastTime : Float
  , snake : List (Int,Int)
  , snakeMeta : List (Char,Direction)
  , direction : Direction
  , food : (Int, Int)
  , score : Int
  , randWord : List Char
  , found : List Char
  , shape : { time : Float } -> List (Shape Msg)
  , wordStr : String
  , nextChar : Char
  , timeUntilNext : Int
  }

init : Model
init = { time = 0
       , lastTime = 0
       , snake = [(0,0),(-1,0)]
       , snakeMeta = [('<', Right)]
       , direction = Right
       , food = (-5,-5)
       , score = 0
       , randWord = []
       , found = []
       , shape = \_ -> []
       , wordStr = ""
       , nextChar = ' '
       , timeUntilNext = timeBetweenResets
       }

main : EllieAppWithTick () Model Msg
main = ellieAppWithTick Tick
  { init = \ _ -> (init, Cmd.batch [newFood init, getRandomWord] )
  , view = \model -> { body = view model, title = "Word Snake" }
  , update = update
  , subscriptions = \ _ -> Sub.none
  }

view model = collage 192 128 (myShapes model)
