module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, p, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Text



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
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
    = SelectCipherLetter String
    | SelectPlainLetter String
    | GenerateCipherLetterPair
    | RemoveCipherLetterPair ( CipherLetter, PlainLetter )


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectCipherLetter cipherLetter ->
            { model | cipher = cipherLetter }

        SelectPlainLetter plainLetter ->
            { model | plain = plainLetter }

        GenerateCipherLetterPair ->
            { model
                | pairs = ( model.cipher, model.plain ) :: model.pairs
                , cipher = ""
                , plain = ""
            }

        RemoveCipherLetterPair ( cipher, plain ) ->
            let
                shouldBeKept : ( CipherLetter, PlainLetter ) -> Bool
                shouldBeKept ( targetCipher, targetPlain ) =
                    cipher /= targetCipher && plain /= targetPlain
            in
            { model
                | pairs = List.filter shouldBeKept model.pairs
            }



-- ----- VIEW -----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "maxWidth" ]
            [ h1 [ class "title margin-btm-m" ] [ text "Simple Monoalphabetic Substitution Cipher" ]
            , p [ class "text" ] [ text Text.explanation ]
            , div [ class "separator" ] []
            , div [ class "btns-container margin-btm-s" ] (cipherButtonsView (getCiphers model.pairs))
            , div [ class "btns-container margin-btm-m" ] (plainButtonsView (getPlains model.pairs))
            , div [ class "row margin-btm-l" ]
                [ cipherToPlainView model
                , confirmGenPairButtonView model
                ]
            , div []
                [ h3 [ class "sub-title margin-btm-s" ] [ text "Cipher-Plain Pairs:" ]
                , div [ class "row" ] (List.map cipherPlainPairView model.pairs)
                ]
            , button [ class "btn btn-reset margin-btm-l" ] [ text "Reset" ]
            , div [ class "text" ] [ text model.text ]
            ]
        ]



-- button view


cipherButtonsView : List CipherLetter -> List (Html Msg)
cipherButtonsView selectedCiphers =
    let
        buttonView : String -> Html Msg
        buttonView letter =
            let
                shouldDisabled : Bool
                shouldDisabled =
                    List.member letter selectedCiphers
            in
            button
                [ onClick (SelectCipherLetter letter)
                , disabled (List.member letter selectedCiphers)
                , classList
                    [ ( "btn btn-alpha", True )
                    , ( "btn-disabled", shouldDisabled )
                    ]
                ]
                [ text letter ]
    in
    List.map buttonView genAllUpperCaseStr


plainButtonsView : List PlainLetter -> List (Html Msg)
plainButtonsView selectedPlains =
    let
        buttonView : String -> Html Msg
        buttonView letter =
            let
                shouldDisabled : Bool
                shouldDisabled =
                    List.member letter selectedPlains
            in
            button
                [ onClick (SelectPlainLetter letter)
                , disabled (List.member letter selectedPlains)
                , classList
                    [ ( "btn btn-alpha", True )
                    , ( "btn-disabled", shouldDisabled )
                    ]
                ]
                [ text letter ]
    in
    List.map buttonView genAllLowerCaseStr


confirmGenPairButtonView : Model -> Html Msg
confirmGenPairButtonView model =
    let
        shouldDisable =
            String.isEmpty model.cipher || String.isEmpty model.plain
    in
    button
        [ disabled shouldDisable
        , class "btn"
        , onClick GenerateCipherLetterPair
        ]
        [ text "Confirm" ]



-- cipher-plain pairs view


cipherToPlainView : Model -> Html Msg
cipherToPlainView model =
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


cipherPlainPairView : ( CipherLetter, PlainLetter ) -> Html Msg
cipherPlainPairView ( cipher, plain ) =
    div [ class "cipherPlainPairContainer margin-btm-m" ]
        [ p [] [ text (cipher ++ " >> " ++ plain) ]
        , button
            [ class "btn btn-sm-red"
            , onClick (RemoveCipherLetterPair ( cipher, plain ))
            ]
            [ text "X" ]
        ]



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


type PairElPosition
    = FirstPos
    | SecondPos


getPairElements : PairElPosition -> List ( a, a ) -> List a
getPairElements pos pairs =
    case pos of
        FirstPos ->
            List.map Tuple.first pairs

        SecondPos ->
            List.map Tuple.second pairs


getCiphers : List ( CipherLetter, PlainLetter ) -> List CipherLetter
getCiphers pairs =
    getPairElements FirstPos pairs


getPlains : List ( CipherLetter, PlainLetter ) -> List PlainLetter
getPlains pairs =
    getPairElements SecondPos pairs
