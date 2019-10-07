module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Cell =
    { id : Int, isMine : Bool, revealed : Bool }


type alias Model =
    { cells : List Cell }


init : Model
init =
    { cells =
        List.map (\x -> { id = x, isMine = modBy 7 x == 0, revealed = False })
            (List.range 1 100)
    }


type Msg
    = Reveal Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reveal id ->
            { model
                | cells =
                    model.cells
                        |> List.map
                            (\cell ->
                                if id == cell.id then
                                    { cell | revealed = True }

                                else
                                    cell
                            )
            }


view : Model -> Html Msg
view model =
    div [ style "display" "grid", style "grid-template-columns" "repeat(10, 50px)" ]
        (List.map (viewCell model.cells)
            model.cells
        )


numberOfBombsAround : List Cell -> Int -> Int
numberOfBombsAround cells id =
    let
        left_neighbours cell x =
            abs
                ((cell.id - 10) - x)
                == 1

        right_neighbours cell x =
            abs ((cell.id + 10) - x) == 1

        neighbours cell x =
            abs (cell.id - x) == 1
    in
    List.filter left_neighbours cells
        |> List.filter right_neighbours
        |> List.filter neighbours
        |> List.length


viewCell : List Cell -> Cell -> Html Msg
viewCell cells cell =
    button
        [ onClick (Reveal cell.id), style "width" "50px", style "height" "50px" ]
        [ if cell.revealed then
            if cell.isMine then
                text "💣"

            else
                text (String.fromInt (numberOfBombsAround cells cell.id))

          else
            text ""
        ]


revealIfId : Int -> Cell -> Cell
revealIfId id cell =
    if cell.id /= id then
        cell

    else
        { id = cell.id, isMine = cell.isMine, revealed = True }


isRevealed : Cell -> Cell
isRevealed cell =
    cell


main =
    Browser.sandbox
        { view = view
        , update = update
        , init = init
        }
