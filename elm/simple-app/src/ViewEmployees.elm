module ViewEmployees exposing (..)


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

init : () -> ( Model, Cmd Message)
init _ =
    ( Waiting, Cmd.none )


type Model
    = Failure String
    | Waiting
    | Loading
    | Success (List Employee)


type Message
    = TryAgainPlease
    | EmployeeResult (Result Http.Error (List Employee))


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        TryAgainPlease ->
            ( Loading, getEmployees )

        EmployeeResult result ->
            case result of
                Ok employees ->
                    ( Success employees, Cmd.none )

                Err error ->
                    (Failure (errorToString error), Cmd.none)



-- VIEW

view : Model -> Html Message
view model =
    case model of
        Waiting ->
            button [ onClick TryAgainPlease ] [ text "All Employees" ]

        Failure msg ->
            text ("Something went wrong " ++ msg)

        Loading ->
            text "Please wait ...."

        Success employees ->
            div [ style "text-align" "center" ]
                [ table tableStyle
                    [ thead []
                        [ tr trStyle
                            [ th trStyle [ text "ID" ]
                            , th trStyle [ text "First Name" ]
                            , th trStyle [ text "Last Name" ]
                            , th trStyle [ text "Email" ]

                            ]
                        ]
                    , tbody [] (List.map viewEmployee employees)
                    ]
                , button [onClick TryAgainPlease] [text "All Employees"]
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


viewEmployee : Employee -> Html Message
viewEmployee employee =
    tr trStyle
        [ td trStyle [ text <| String.fromInt employee.id ]
        , td trStyle [ text employee.firstName ]
        , td trStyle [ text employee.lastName ]
        , td trStyle [ text employee.email ]

        ]

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

getEmployees : Cmd Message
getEmployees = Http.get
    { url = "http://localhost:8080/org/api/org/empl"
    , expect = Http.expectJson EmployeeResult allEmployeesDecoder
    }


type alias Employee =
    {
     id  : Int
    ,firstName: String
    ,lastName: String
    ,email: String
    }


employeeDecoder: Decode.Decoder Employee
employeeDecoder =
       Decode.map4 Employee
       (Decode.field "id" Decode.int)
       (Decode.field "firstName" Decode.string)
       (Decode.field "lastName" Decode.string)
       (Decode.field "email" Decode.string)


encodeEmployee : Employee -> Encode.Value
encodeEmployee employee=
    Encode.object
        [("firstName", Encode.string employee.firstName)
        ,("lastName", Encode.string employee.lastName)
        ,("email", Encode.string employee.email)]

allEmployeesDecoder: Decode.Decoder (List Employee)
allEmployeesDecoder =
    Decode.list employeeDecoder




subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none