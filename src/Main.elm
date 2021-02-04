module Main exposing (..)

import AutoCompletion exposing (..)
import Browser
import Browser.Events exposing (onKeyDown)
import Expr exposing (..)
import Html exposing (div, option, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import HtmlDisplay exposing (..)
import Json.Decode as Decode
import Platform.Cmd exposing (none)
import Platform.Sub exposing (batch)
import Show exposing (..)
import TextDisplay exposing (show_of_expr)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = Keyboard Key


type alias Options =
    Maybe { prev : List Expr, curr : Expr, next : List Expr }


type alias EditModel =
    { focus : ExprFocus
    , options : Options
    }


type alias Model =
    { focus : ExprFocus
    , options : Options
    , lastKey : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { focus = MExp (Todo "") EmptyContext
      , options = Nothing
      , lastKey = ""
      }
    , none
    )


type Direction
    = Down
    | Up
    | Left
    | Right


type EditMsg
    = Move Direction
    | Insert Char
    | Enter
    | Backspace


type alias Keymap a =
    Key -> Maybe a


edit_keymap : Keymap EditMsg
edit_keymap { ctrl, alt, key } =
    if ctrl || alt then
        Nothing

    else
        case key of
            "ArrowUp" ->
                Just (Move Up)

            "ArrowDown" ->
                Just (Move Down)

            "ArrowLeft" ->
                Just (Move Left)

            "ArrowRight" ->
                Just (Move Right)

            "Backspace" ->
                Just Backspace

            "Enter" ->
                Just Enter

            _ ->
                case String.uncons key of
                    Just ( c, "" ) ->
                        Just (Insert c)

                    _ ->
                        Nothing


type alias Key =
    { ctrl : Bool, alt : Bool, key : String }


string_of_key : Key -> String
string_of_key { ctrl, alt, key } =
    String.concat
        [ if ctrl then
            "CTRL+"

          else
            ""
        , if alt then
            "ALT+"

          else
            ""
        , key
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        Keyboard key ->
            case edit_keymap key of
                Nothing ->
                    ( mdl, none )

                Just edit_msg ->
                    let
                        { focus, options } =
                            update_model edit_msg { focus = mdl.focus, options = mdl.options }
                    in
                    ( { focus = focus
                      , options = options
                      , lastKey = string_of_key key
                      }
                    , none
                    )


options_up : Options -> Options
options_up ops =
    case ops of
        Nothing ->
            Nothing

        Just { prev, curr, next } ->
            case prev of
                [] ->
                    ops

                e :: es ->
                    Just
                        { prev = es
                        , curr = e
                        , next = curr :: next
                        }


options_down : Options -> Options
options_down ops =
    case ops of
        Nothing ->
            Nothing

        Just { prev, curr, next } ->
            case next of
                [] ->
                    ops

                e :: es ->
                    Just
                        { prev = curr :: prev
                        , curr = e
                        , next = es
                        }


update_model : EditMsg -> EditModel -> EditModel
update_model edit_msg mdl =
    case ( edit_msg, mdl.focus ) of
        ( Move Right, _ ) ->
            { mdl | focus = next_hole mdl.focus }

        ( Move Left, _ ) ->
            { mdl | focus = prev_hole mdl.focus }

        ( _, MExp (Todo s) ctx ) ->
            case edit_msg of
                Move Up ->
                    { mdl | options = options_up mdl.options }

                Move Down ->
                    { mdl | options = options_down mdl.options }

                Backspace ->
                    if String.length s == 0 then
                        { mdl | focus = prev_hole mdl.focus, options = Nothing }

                    else
                        let
                            new_s =
                                String.slice 0 -1 s
                        in
                        { focus = MExp (Todo new_s) ctx
                        , options = options_of_list_expr (suggest_fills new_s)
                        }

                Insert c ->
                    let
                        new_s =
                            String.append s (String.fromChar c)
                    in
                    { focus = MExp (Todo new_s) ctx
                    , options = options_of_list_expr (suggest_fills new_s)
                    }

                Enter ->
                    let
                        new_focus =
                            case mdl.options of
                                Nothing ->
                                    mdl.focus

                                Just { curr } ->
                                    MExp curr ctx
                    in
                    { focus = next_hole new_focus
                    , options = Nothing
                    }

                _ ->
                    mdl

        ( Move Up, _ ) ->
            { mdl | focus = up_hole mdl.focus }

        ( Enter, _ ) ->
            { mdl | focus = next_hole mdl.focus }

        ( _, MVar x ctx ) ->
            { mdl | focus = string_update edit_msg x (\new -> MVar new ctx) }

        ( _, MLbl x ctx ) ->
            { mdl | focus = string_update edit_msg x (\new -> MLbl new ctx) }

        ( Backspace, MExp _ ctx ) ->
            { mdl | focus = MExp (Todo "") ctx }

        _ ->
            mdl


options_of_list_expr : List Expr -> Options
options_of_list_expr es =
    case es of
        [] ->
            Nothing

        e :: next ->
            Just { prev = [], curr = e, next = next }


list_expr_of_options : Options -> List Expr
list_expr_of_options ops =
    case ops of
        Nothing ->
            []

        Just { curr, next } ->
            curr :: next


string_update : EditMsg -> String -> (String -> ExprFocus) -> ExprFocus
string_update edit_msg s make_model =
    case edit_msg of
        Insert c ->
            make_model (String.append s (String.fromChar c))

        Backspace ->
            if String.length s == 0 then
                prev_hole (make_model "")

            else
                make_model (String.slice 0 -1 s)

        Enter ->
            next_hole (make_model s)

        _ ->
            make_model s


keyDownDecoder : Decode.Decoder Msg
keyDownDecoder =
    Decode.map3 keyDownToMsg
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "altKey" Decode.bool)
        (Decode.field "key" Decode.string)


keyDownToMsg : Bool -> Bool -> String -> Msg
keyDownToMsg ctrl alt s =
    Keyboard { ctrl = ctrl, alt = alt, key = s }


subscriptions : Model -> Sub Msg
subscriptions _ =
    batch [ onKeyDown keyDownDecoder ]


view : Model -> Html.Html Msg
view mdl =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "height" "100vh"
        ]
        [ div
            [ style "width" "50vw"
            ]
            [ view_model { focus = mdl.focus, options = mdl.options } ]
        , div
            [ style "white-space" "pre"
            , style "font-family" "Courier New, Courier, monospace"
            , style "width" "50vw"
            , style "flex-grow" "0"
            , style "flex-shrink" "0"
            ]
            [ text
                (show
                    (show_of_expr (plugin mdl.focus))
                )
            ]

        -- , text mdl.lastKey
        -- , view_model m
        ]


view_model : EditModel -> Html.Html Msg
view_model m =
    case m.focus of
        MVar x ctx ->
            embed_html_in_vctx (frame_html (html_var x)) ctx

        MLbl x ctx ->
            embed_html_in_lctx (frame_html (html_tag x)) ctx

        MExp (Todo s) ctx ->
            embed_html_in_ectx (frame_html (html_focused_todo s (list_expr_of_options m.options))) ctx

        MExp e ctx ->
            embed_html_in_ectx (frame_html (html_of_expr e)) ctx
