module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, p, text)
import Html.Attributes exposing (disabled, class)
import Html.Events exposing (onClick)

import Text


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
    div [ class "container" ]
        [ div [ class "maxWidth" ]
            [ h1 [ class "title margin-btm-m" ] [ text "Simple Monoalphabetic Substitution Cipher" ]
            , p [ class "text" ] [ text Text.explanation ]
            , div [class "separator" ] []
            , div [ class "btns-container margin-btm-s" ] cipherButtonsView
            , div [ class "btns-container margin-btm-m" ] plainButtonsView
            , div [ class "row margin-btm-l"]
                [ cipherPlainPairView model
                , confirmGenPairButtonView model
                ]
            , div []
                [ h3 [ class "sub-title margin-btm-m"] [ text "Cipher-Plain Pairs:" ]

                ]
            -- , button [ class "btn" ] [ text "Reset" ]
            , div [ class "text" ] [ text model.text ]
        ]
    ]



-- button view


cipherButtonsView : List (Html Msg)
cipherButtonsView =
    let
        buttonView : String -> Html Msg
        buttonView str =
            button
              [ onClick (SelectChipherLetter str)
              , class "btn btn-alpha"
              ]
              [ text str ]
    in
        List.map buttonView genAllUpperCaseStr


plainButtonsView : List (Html Msg)
plainButtonsView =
    let
        buttonView : String -> Html Msg
        buttonView str =
            button
              [ onClick (SelectPlainLetter str)
              , class "btn btn-alpha"
              ]
              [ text str ]
    in
        List.map buttonView genAllLowerCaseStr


confirmGenPairButtonView : Model -> Html Msg
confirmGenPairButtonView model =
    let
        shouldDisable = String.isEmpty model.cipher || String.isEmpty model.plain
    in
        button
            [ disabled shouldDisable
            , class "btn"]
            [ text "Confirm" ]




-- cipher-plain pairs view


cipherPlainPairView : Model -> Html Msg
cipherPlainPairView model =
    let
        cipher =
             if String.isEmpty model.cipher then
                "_"
             else
                model.cipher

        plain =
            if String.isEmpty model.plain then
                "_"
             else
                model.plain
    in
        div [] [ text (cipher ++ " >> " ++ plain) ]


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
