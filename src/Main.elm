import Browser
import Html exposing (Html, header, footer, ul, li, i, button, div, h1, p, text, input, form)
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Attributes exposing (type_, placeholder, class, value)

main =
  Browser.sandbox { init = initModel, update = update, view = view }


--########################## MODEL #########################-

type alias Player = 
    { id : Int
    , name : String
    , points : Int
    }


type alias Play = 
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }

type alias Model = 
    { players : List Player
    , playerId : Maybe Int
    , name : String
    , plays : List Play
    }

initModel : Model 
initModel = 
    { players = []
    , playerId = Nothing
    , name = ""
    , plays = []
    }


--######################## UPDATE ########################--

type Msg 
    = 
      Edit Player
    | Score Player Int
    | Input String
    | Save 
    | Cancel 
    | DeletePlay Play 


update msg model =
  case msg of
    Input name ->
      Debug.log "My model:" { model | name = name }
    Cancel -> 
      { model | name = "", playerId = Nothing }
    Save ->
      if String.isEmpty model.name then 
        model 
      else
        save model
    Score playerrr points ->
      score model playerrr points
    Edit playyer ->
      { model | name = playyer.name, playerId = Just playyer.id }
    DeletePlay play1 ->
      deletePlay model play1




  -- Helpers 
save : Model -> Model
save model = 
  case model.playerId of
      Just id ->
        edit model id
      Nothing ->
        add model

edit : Model -> Int -> Model
edit model id = 
    let 
        newPlayers = 
                List.map 
                  (\player0 ->
                        if player0.id == id then 
                          { player0 | name = model.name }
                        else 
                          player0
                  ) model.players

        newPlays =         
                List.map 
                  (\pla ->
                        if pla.playerId == id then 
                          { pla | name = model.name }
                        else 
                          pla
                  ) model.plays
    in 
        {
          model |
              players = newPlayers
            , plays = newPlays
            , playerId = Nothing
            , name = ""
        }
    

add : Model -> Model
add model = 
  let 
      player2 = 
              Player (List.length model.players) model.name 0
      newPlayers = player2::model.players
  in 
      { model | 
            players = newPlayers
          , name = ""  
      }

score : Model -> Player -> Int -> Model
score model scorer points =
    let 
        newPlayers = 
                List.map 
                    (\plyer ->
                          if plyer.id == scorer.id then
                                { plyer  
                                      | points = plyer.points + points
                                }
                          else
                                plyer
                          
                    )
                    model.players
                
        playyyy = 
            Play (List.length model.plays) scorer.id scorer.name points
    in
        { model | players = newPlayers, plays = playyyy :: model.plays }

--########################## VIEW ##########################--



view model =
  div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , playerSection model
        , playerForm model
        , playSection model
        ]


playerForm : Model -> Html Msg
playerForm model = 
    form [ onSubmit Save ]
            [ input [ type_ "text"
                    , placeholder "Enter/edit player name"
                    , onInput Input 
                    , value model.name
                    ]
                    [ ]
            , button [ type_ "submit" ][ text "Save" ]
            , button [ type_ "button" , onClick Cancel ][ text "Cancel" ]
            ]

playSection : Model -> Html Msg
playSection model = 
  div []
      [ playListHeader
      , playList model
      ]

playListHeader: Html Msg
playListHeader = 
  header []
          [ div [][ text "Plays"]
          , div [][ text "Points"]
          ]


playList : Model -> Html Msg
playList model = 
  model.plays
      |> List.map play
      |> ul []

play : Play -> Html Msg
play playyy = 
  li []
      [ i [ class "remove"
          , onClick (DeletePlay playyy)
          ]
          []
      , div [][ text playyy.name ]
      , div [][ text (String.fromInt playyy.points) ]
      ]


playerSection : Model -> Html Msg
playerSection model = 
  div []
      [ playerListHeader
      , playerList model 
      , pointTotal model
      ]

playerListHeader : Html Msg
playerListHeader = 
  header []
          [ div [][ text "Name" ] 
          , div [][ text "Points"]
          ]

playerList : Model -> Html Msg
playerList model = 
  model.players
      |> List.sortBy .name
      |> List.map player
      |> ul []

player : Player -> Html Msg
player playerr = 
  li []
      [ i [ class "edit"
          , onClick (Edit playerr)
          ]
          []
      , div [] [ text playerr.name ]
      , button [  type_ "button" 
                , onClick (Score playerr 2 )
               ]
               [ text "2pts" ]
      , button [  type_ "button" 
                , onClick (Score playerr 3 )
               ]
               [ text "3pts" ]
      , div []
            [ text (String.fromInt playerr.points) ]
      ]

pointTotal : Model -> Html Msg
pointTotal model = 
  let 
    total = 
          List.map .points model.plays
              |> List.sum
  in
    footer []
            [ div [][ text "Total:"] 
            , div [][ text ( String.fromInt total) ]
            ]

deletePlay : Model -> Play -> Model
deletePlay model playyyy = 
  let
      newPlays = 
        List.filter (\p-> p.id /= playyyy.id) model.plays

      newPlayers = 
        List.map
            (\playeeer ->
              if playeeer.id == playyyy.playerId then
                  { playeeer | points = playeeer.points - 1 * playyyy.points }
              else
                  playeeer    
              ) 
              model.players
  in 
      { model | plays = newPlays, players = newPlayers }

