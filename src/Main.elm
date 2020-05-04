module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, p, span, text)
import Html.Events exposing (onClick)


-- MAIN


main: Program () Model Msg
main = Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }



-- MODEL


type alias CipherLetter =
    String


type alias PlainLetter =
    String


type alias Model =
    { cipher : CipherLetter
    , plain : PlainLetter
    , pairs : List ( CipherLetter, PlainLetter )
    , text : String
    }


initialModel : Model
initialModel =
    { cipher = ""
    , plain = ""
    , pairs = []
    , text = "abcd jakjsdf;l ajkwiou mzm,xiuw  ajksldj;f qewiuk ajkdjfiwm vjkladjsf jqiweru"
    }


-- UPDATE


type Msg
    = SelectChipherLetter String
    | SelectPlainLetter String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectChipherLetter cipherLetter ->
            { model | cipher = cipherLetter }

        SelectPlainLetter plainLetter ->
            { model | plain = plainLetter }



-- ----- VIEW -----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Cipher Challange Stage 1: Simple Monoalphabetic Substitution Cipher" ]
        , p [] [ text "Explanation goes here" ]
        , div [] cipherButtonsView
        , div [] plainButtonsView
        , p [] [ text model.cipher ]
        , p [] [ text model.plain ]
        , button [] [ text "Confirm" ]
        , div []
            [ h2 [] [ text "Cipher-Plain pairs" ]
            , div [] (cipherPlainPairsView model.pairs)
            ]
        , button [] [ text "Reset" ]
        , div [] [ text model.text ]
        ]



-- button view


cipherButtonsView : List (Html Msg)
cipherButtonsView =
    let
        buttonView : String -> Html Msg
        buttonView str =
            button [ onClick (SelectChipherLetter str) ] [ text str ]
    in
        List.map buttonView genAllUpperCaseStr


plainButtonsView : List (Html Msg)
plainButtonsView =
    let
        buttonView : String -> Html Msg
        buttonView str =
            button [ onClick (SelectPlainLetter str) ] [ text str ]
    in
        List.map buttonView genAllLowerCaseStr



-- cipher-plain pairs view


cipherPlainPairView : ( CipherLetter, PlainLetter ) -> Html Msg
cipherPlainPairView ( cipher, plain ) =
    div []
        [ span [] [ text (cipher ++ " -> " ++ plain) ]
        , button [] [ text "X" ]
        ]


cipherPlainPairsView : List ( CipherLetter, PlainLetter ) -> List (Html Msg)
cipherPlainPairsView pairs =
    List.map cipherPlainPairView pairs


-- ----- HELPER -----


generateChars : List Int -> List Char
generateChars unicodes =
    List.map Char.fromCode unicodes


genAllUpperCaseStr : List String
genAllUpperCaseStr =
    generateChars (List.range 65 90)
    |> List.map String.fromChar


genAllLowerCaseStr : List String
genAllLowerCaseStr =
    generateChars (List.range 97 122)
    |> List.map String.fromChar
