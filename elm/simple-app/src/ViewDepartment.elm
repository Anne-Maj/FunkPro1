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
    = TryAgainPlease
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
        TryAgainPlease ->
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
            button [ onClick TryAgainPlease ] [ text "View Departments" ]

        Failure msg ->
            text ("Something went wrong " ++ msg)

        Loading ->
            text "Please wait ...."

        Success departments ->
            div [ style "text-align" "center" ]
                [ table tableStyle
                    [ thead []
                        [ tr trStyle
                            [ th trStyle [ text "Code" ]
                            , th trStyle [ text "Name" ]
                            , th trStyle [ text "Description" ]
                            ]
                        ]
                    , tbody [] (List.map viewDepartment departments)
                    ]
                , button [onClick TryAgainPlease] [text "View Departments"]
                ]

tableStyle : List (Attribute msg)
tableStyle =
    [ style "border-collapse" "collapse"
    , style "width" "100%"
    , style  "border" "1px solid black"
    ]
trStyle : List (Attribute msg)
trStyle =
    [style "border" "1px solid black"]


viewDepartment : Department -> Html Message
viewDepartment department =
    tr trStyle
        [ td trStyle [ text <| String.fromInt department.code ]
        , td trStyle [ text department.name ]
        , td trStyle [ text department.description ]

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