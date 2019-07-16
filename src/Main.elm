import Browser
import Html exposing (Html, button, div, h1, text, input, form)
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

    _ ->
      model



--########################## VIEW ##########################--

playerForm : Model -> Html Msg
playerForm model = 
    form [ onSubmit Save ]
            [ input [ type_ "text"
                    , placeholder "Enter player name"
                    , onInput Input 
                    , value model.name
                    ]
                    [ ]
            , button [ type_ "submit" ][ text "Save" ]
            , button [ type_ "button" ][ text "Cancel" ]
            ]

view model =
  div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , playerForm model
        ]
