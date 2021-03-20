module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, li, span, text, ul)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)

main = Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Pet =
    { id: Int
    , name: String
    , age: Int
    , alive: Bool
    }

type alias Model =
    { pets: List Pet }

init : Model
init = Model
    [ Pet 7 "Ninus" 9 True
    , Pet 9 "Rufus" 3 True
    , Pet 13 "Felix" 5 True
    ]

-- UPDATE

type Message
    = CelebratePet Int
    | KillPet Int
    | NameChanged Int String

{-
getPet : Int -> List Pet -> Maybe Pet
getPet id pets =
    case pets of
        [] -> Nothing
        pet :: rest -> if pet.id == id then Just pet else (getPet id rest)

setPet : Pet -> List Pet -> List Pet
setPet pet pets =
    case pets of
        [] -> [pet]
        p :: rest -> if pet.id == p.id then pet :: rest else p :: (setPet pet rest)
-}

updatePet : Message -> Pet -> Pet
updatePet message pet =
    case message of
        CelebratePet id ->
            if pet.id == id then
                { pet | age = pet.age + 1 }
            else
                pet
        KillPet id -> if pet.id == id then { pet | alive = False } else pet
        NameChanged id name -> if pet.id == id then { pet | name = name } else pet

{-
updatePets : Int -> (Pet -> Pet) -> List Pet -> List Pet
updatePets id action pets =
    case pets of
        [] -> []
        head :: tail ->
            if head.id == id then
                action head :: tail
            else
                head :: (updatePets id action tail)

updateModel : Int -> (Pet -> Pet) -> Model -> Model
updateModel id action model =
    { model | pets = updatePets id action model.pets }
-}
update : Message -> Model -> Model
update message model =
    { model | pets = (List.map (updatePet message) model.pets) }
{-
    case message of
        CelebratePet id ->
            updateModel id ( \pet -> { pet | age = pet.age + 1 } ) model
        KillPet id ->
            { model | pets = (updatePets id ( \pet -> { pet | alive = False} ) model.pets) }
        NameChanged id name ->
            { model | pets = (updatePets id ( \pet -> { pet | name = name} ) model.pets) }
-}
{-
        CelebratePet id ->
            let
                maybePet = getPet id model.pets
            in
                case maybePet of
                    Just pet ->
                        { model | pets = (setPet { pet | age = pet.age + 1 } model.pets) }
                    Nothing ->
                        model
            -- { pet | age = (pet.age + 1) }

        KillPet ->
            { pet | alive = False }
        NameChanged newName ->
            { pet | name = newName }
-}


-- VIEW

view : Model -> Html Message
view model =
    div [ style "margin" "2em" ]
        [ ul [] (List.map viewPet model.pets)
        ]

viewPet : Pet -> Html Message
viewPet pet =
    li []
        [ span [ style "marginRight" "1ex"] [ text (String.fromInt pet.id) ]
        , input [ onInput <| NameChanged pet.id,  value pet.name ] []
        , span
            [ style "color" (if pet.alive then "green" else "red") ]
            [ text (" is "++(String.fromInt pet.age)++" years old") ]
        , button [ onClick (CelebratePet pet.id)] [ text "Celebrate" ]
        , button [ onClick (KillPet pet.id) ] [text "Kill" ]
        ]

--    div [ style "color" "red"] [ text ("Min kanin hedder "++pet.name) ]

-- <div style="color: red">Ninus</div>
-- <button onclick="...">Celebrate</button>