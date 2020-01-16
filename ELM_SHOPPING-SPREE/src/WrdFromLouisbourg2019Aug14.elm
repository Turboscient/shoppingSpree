module WrdFromLouisbourg2019Aug14 exposing(words)
import GraphicSVG exposing(..)
words = [
    let
        myWord = "cannon"
        myCode = 549

        myShapes model = [ group
          [ rect 32.0 3.0
             |> filled brown
          , square 6.0
             |> filled darkBrown
             |> move ((-13.0), (-4.0))
          , square 6.0
             |> filled darkBrown
             |> move (13.0, (-4.0))
          , rect 49.0 8.0
             |> filled  black
             |> move ((-6.0), 11.0)
          , rect 21.0 8.0
             |> filled darkBrown
             |> move ((-1.0), 5.0)
          , rect 10.0 4.0
             |> filled darkBrown
             |> move (10.0, 3.0)
          , rect 3.0 10.0
             |> filled black
             |> move ((-22.0), 11.0)
          ]
          |> scale 1.5
          

         ] 
    in (("ElmJr",myWord,"0"),("~","~",myShapes))

  , let
        myWord = "fishing net"
        myCode = 549

        myShapes model = [ group
          [ square 2000.0
             |> filled blue
          , circle 10.0
             |> filled blue
             |> move ((-10.0), (-10.0))
          , rect 2.0 100.0
             |> filled charcoal
          , rect 2.0 100.0
             |> filled charcoal
             |> move ((-30.0),  0 )
          , rect 2.0 100.0
             |> filled charcoal
             |> move ((-15.0),  0 )
          , rect 2.0 100.0
             |> filled charcoal
             |> move (20.0,  0 )
          , rect 100.0 2.0
             |> filled charcoal
          , rect 100.0 2.0
             |> filled charcoal
             |> move ( 0 , 30.0)
          , rect 100.0 2.0
             |> filled charcoal
             |> move ( 0 , (-30.0))
          , circle 5.0
             |> filled charcoal
          , circle 5.0
             |> filled charcoal
             |> move (20.0, 0.0)
          , circle 5.0
             |> filled charcoal
             |> move ((-15.0),  0 )
          , circle 5.0
             |> filled charcoal
             |> move ((-30.0),  0 )
          , circle 5.0
             |> filled charcoal
             |> move ( 0 , 28.0)
          , circle 5.0
             |> filled charcoal
             |> move ((-15.0), 30.0)
          , circle 5.0
             |> filled charcoal
             |> move (20.0, 30.0)
          , circle 5.0
             |> filled charcoal
             |> move ((-30.0), 30.0)
          , circle 5.0
             |> filled charcoal
             |> move ( 0 , (-30.0))
          , circle 5.0
             |> filled charcoal
             |> move (20.0, (-30.0))
          , circle 5.0
             |> filled charcoal
             |> move ((-15.0), (-30.0))
          , circle 5.0
             |> filled charcoal
             |> move ((-30.0), (-30.0))
          ]
          |> scale 1.1 |> move (5,0)


         ]
    in (("ElmJr",myWord,"0"),("~","~",myShapes))

  , let
        myWord = "quill"
        myCode = 549

        myShapes model = [ group
            [ square 2000.0
               |> filled pink
            , curve (0.0,0.0) [Pull (10.0,20.0) (30.0,20.0)
                             , Pull (15.0,15.0) (0.0,0.0)
                             ]
               |> filled ( rgb 255.0 230.0 240.0)
               |> move ((-10.0), (-10.0))
            , oval 35.0 15.0
               |> filled white
               |> rotate (degrees 19)
               |> move (20.0, 12.0)
            , text "ecrire" |> filled black |> move (-35,-15)
            ]

            |> scale 1.1 |> move (5,0)


         ]
    in (("ElmJr",myWord,"0"),("~","~",myShapes))

  , let
        myWord = "sheep"
        myCode = 549

        myShapes model = [ group
          [ square 2000.0
             |> filled lightBlue
          , oval 30.0 40.0
             |> filled white
             |> move ((-10.0), 0.0)
          , oval 30.0 40.0
             |> filled white
          , oval 30.0 40.0
             |> filled white
             |> move (10.0,  0 )
          , oval 30.0 40.0
             |> filled white
             |> move ((-20.0),  0 )
          , circle 15.0
             |> filled white
             |> move (30.0, 10.0)
          , rect 7.0 9.0
             |> filled lightBrown
             |> move (13.0, (-20.5))
          , oval 5.0 11.0
             |> filled white
             |> move ((-37.0), 4.0)
          , oval 5.0 9.0
             |> filled lightGrey
             |> move (26.0, 10.0)
          , rect 7.0 9.0
             |> filled lightBrown
             |> move ((-20.0), (-21.0))
          , rect 2.0 8.0
             |> filled lightBrown
             |> move (43.0, 4.5)
          , circle 2.0
             |> filled  black
             |> move (35.0, 12.0)
          , oval 6.0 8.5
             |> filled lightBrown
             |> move (44.9, 4.5)
          , rect 4.0 1.0
             |> filled  black
             |> move (45.5, 4.0)
          , circle 3.0
             |> filled white
             |> move (35.0, 22.0)
          , circle 3.0
             |> filled white
             |> move (30.0, 22.5)
          , circle 3.0
             |> filled white
             |> move (26.0, 22.0)
          ]
           |> move (-5,0)

         ]
    in (("ElmJr",myWord,"0"),("~","~",myShapes))

  , let
        myWord = "fish"
        myCode = 549
        outlinedsircle = circle 2.0
                           |> outlined (solid 1.0) grey
                           |> move (13.0, 12.0)

        myShapes model = [ group
          [ square 2000.0
             |> filled lightBlue
          , ngon 3 6.0
             |> filled darkYellow
             |> move ((-17.0), 0.0)
          , oval 31.0 13.0
             |> filled lightYellow
          , circle 2.0
             |> filled blue
             |> move (10.0, 2.0)
          , curve (10.0,0.0) [Pull (12.0,(-2.0)) (15.0,0.0)
                            ]
             |> outlined  (solid 1)  darkOrange
             |> move (0.0, (-1.0))
          , outlinedsircle
          , outlinedsircle
             |> move (8.0, 5.0)
          , outlinedsircle
             |> move ((-3.0), 6.0)
          ]


         ]
    in (("ElmJr",myWord,"0"),("~","~",myShapes))

  , let
        myWord = "turkey egg"
        myCode = 549

        myShapes model = [ group
          [ square 200.0
             |> filled pink
          , oval 29.0 38.0
             |> filled white
          ]


         ]
    in (("ElmJr",myWord,"0"),("~","~",myShapes))

  , let
        myWord = "bread"
        myCode = 549

        myShapes model = [ group
          [ square 2000.0
             |> filled pink
          , roundedRect 30.0 17.0 10.0
             |> filled brown
          , oval 30.0 25.0
             |> filled brown
             |> move (0.0, 15.0)
          ]


         ]
    in (("ElmJr",myWord,"0"),("~","~",myShapes))

  , let
        myWord = "fan"
        myCode = 549

        fanstick = rect 1.0 52.0
                     |> filled  black
                     |> move (1.5, (-10.0))
                     |> rotate (-3.0)

        myShapes model = [ group
          [ square 200.0
             |> filled lightPurple
          , wedge 50.0 (degrees 23.0)
             |> filled lightBrown
             |> move ((-10.0), (-10.0))
             |> rotate 95.0
          , fanstick
             |> move (0.0, 0.0)
          , rect 1.0 52.0
             |> filled  black
             |> move (3.0, 10.0)
             |> rotate (degrees (-9.0))
          , rect 1.0 52.0
             |> filled  black
             |> move (8.0, 11.0)
             |> rotate (degrees (-25.0))
          , rect 1.0 52.0
             |> filled  black
             |> move (12.0, 14.0)
             |> rotate (degrees (-45.0))
          , rect 1.0 49.0
             |> filled  black
             |> move (15.0, 20.0)
             |> rotate (degrees (-65.0))
          , rect 1.0 49.0
             |> filled  black
             |> move (16.5, 24.0)
             |> rotate (degrees (-82.0))
          , rect 1.0 49.0
             |> filled  black
             |> move (16.0, 28.0)
             |> rotate (degrees (-100.0))
          ]
          |> scaleY (0.9 + 0.1 * sin (3*model.time))
          |> move (-20.0, 0)


         ]
    in (("ElmJr",myWord,"0"),("~","~",myShapes))

  , let
        myWord = "candle"
        myCode = 549

        myShapes model = [ group
          [ square 2000.0
             |> filled darkRed
          , oval 15.0 35.0
             |> filled red
             |> move ((-10.0), 40.0)
          , oval 50.0 10.0
             |> filled brown
             |> move ((-10.0), (-55.0))
          , rect 20.0 75.0
             |> filled lightOrange
             |> move ((-10.0), (-20.0))
          , oval 10.0 25.0
             |> filled orange
             |> move ((-10.0), 35.0)
          , oval 5.0 15.0
             |> filled yellow
             |> move ((-10.0), 30.0)
          , rect 3.0 30.0
             |> filled orange
             |> move ((-5.0), 2.0)
          , oval 3.0 5.0
             |> filled orange
             |> move ((-5.0), (-13.0))
          , circle 5.0
             |> filled brown
             |> move (18.0, (-52.0))
          , roundedRect 2.0 45.0 12.0
             |> filled orange
             |> move ((-10.0), (-5.0))
          , circle 3.0
             |> filled  black
             |> move (18.0, (-52.0))
          ]
           |> scale 0.7

         ]
    in (("ElmJr",myWord,"0"),("~","~",myShapes))


  , let
        myWord = "barrel"
        myCode = 549

        myShapes model = [ group
          [ oval 25.0 50.0
             |> filled darkBrown
             |> move ((-10.0), (-10.0))
          , oval 25.0 50.0
             |> filled darkBrown
             |> move (10.0, (-10.0))
          , rect 25.0 50.0
             |> filled darkBrown
             |> move (0.0, (-10.0))
          , oval 31.0 8.0
             |> filled black
             |> move (0.0, 13.0)
          , rect 1.0 40.0
             |> filled brown
             |> move (8.0, (-14.0))
          , rect 1.0 40.0
             |> filled brown
             |> move ((-8.0), (-14.0))
          ]


         ]
    in (("ElmJr",myWord,"0"),("~","~",myShapes))

  , let
        myWord = "cannon balls"
        myCode = 549

        myShapes model = [ group
          [ square 2000.0
             |> filled (rgb 0 200 50)
          , circle 11.0
             |> filled black
             |> move (7.0, (-10.0))
          , circle 11.0
             |> filled black
             |> move ((-8.0), (-10.0))
          , circle 11.0
             |> filled black
          ]


         ]
    in (("ElmJr",myWord,"0"),("~","~",myShapes))
  ]
