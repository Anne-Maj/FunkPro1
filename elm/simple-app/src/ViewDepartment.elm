module ViewDepartment exposing (..)

import Browser

import Html exposing (Attribute, Html, button, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http

import Json.Decode as Decode
import Json.Encode as Encode

main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- MODEL--

type Model
    = Failure String
    | Waiting
    | Loading
    | Success (List Department)


type Message
    = TryAgain
    | DepartmentResult (Result Http.Error (List Department))


init : () -> ( Model, Cmd Message)
init _ =
    ( Waiting, Cmd.none )


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadStatus code ->
          "Code: "++(String.fromInt code)
        Http.NetworkError ->
         "Network Error"
        Http.BadBody err ->
         "Bad Body: "++err
        Http.Timeout ->
         "Timeout"
        Http.BadUrl string ->
         "Bad Url: "++string


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        TryAgain ->
            ( Loading, getDepartments )

        DepartmentResult result ->
            case result of
                Ok departments ->
                    ( Success departments, Cmd.none )

                Err error ->
                    (Failure (errorToString error), Cmd.none)

-- VIEW

view : Model -> Html Message
view model =
    case model of
        Waiting ->
            button [ onClick TryAgain ] [ text "View Departments" ]

        Failure msg ->
            text ("Failure! " ++ msg)

        Loading ->
            text "Loading...."

        Success departments ->
            div [ style "text-align" "center" ]
                [ table []
                    [ thead []
                        [ tr []
                            [ th [] [ text "Code" ]
                            , th [] [ text "Name" ]
                            , th [] [ text "Description" ]
                            ]
                        ]
                    , tbody [] (List.map viewDepartment departments)
                    ]
                , button [onClick TryAgain] [text "View Departments"]
                ]

viewDepartment : Department -> Html Message
viewDepartment department =
    tr []
        [ td [] [ text <| String.fromInt department.code ]
        , td [] [ text department.name ]
        , td [] [ text department.description ]

        ]

getDepartments : Cmd Message
getDepartments = Http.get
    { url = "http://localhost:8080/org/api/org/dept"
    , expect = Http.expectJson DepartmentResult allDepartmentsDecoder
    }


type alias Department =
    {
     code  : Int
    ,name: String
    ,description: String

    }

departmentDecoder: Decode.Decoder Department
departmentDecoder =
       Decode.map3 Department
       (Decode.field "code" Decode.int)
       (Decode.field "name" Decode.string)
       (Decode.field "description" Decode.string)


encodeDepartment : Department -> Encode.Value
encodeDepartment department=
    Encode.object
        [("code", Encode.int department.code)
        ,("name", Encode.string department.name)
        ,("description", Encode.string department.description)]

allDepartmentsDecoder: Decode.Decoder (List Department)
allDepartmentsDecoder =
    Decode.list departmentDecoder

subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none