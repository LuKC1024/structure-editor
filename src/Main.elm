module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import TextDisplay exposing (show_of_expr)
import Expr exposing (..)
import AutoCompletion exposing (..)
import Html exposing (div, pre, span, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Platform.Cmd exposing (none)
import Platform.Sub exposing (batch)
import Show exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( MExp (Todo "") EmptyContext, none )


type Direction
    = Down
    | Up
    | Left
    | Right


type Msg
    = Move Direction
    | InsertString String
    | Submit
    | Delete
    | Open
    | Close
    | Space
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    ( update_model msg mdl, none )


string_update : Msg -> String -> (String -> Model) -> Model
string_update msg x make_model =
    case msg of
        Delete ->
            make_model (String.slice 0 -1 x)

        Open ->
            make_model (String.append x "(")

        Close ->
            make_model (String.append x ")")

        Space ->
            make_model (String.append x " ")

        InsertString s ->
            make_model (String.append x s)

        Submit ->
            next_hole (make_model x)

        _ ->
            make_model x


update_model : Msg -> Model -> Model
update_model msg mdl =
    case ( msg, mdl ) of
        ( Move Right, _ ) ->
            next_hole mdl

        ( Move Left, _ ) ->
            prev_hole mdl

        ( Move Up, _ ) ->
            up_hole mdl

        ( _, MVar x ctx ) ->
            string_update msg x (\new -> MVar new ctx)

        ( _, MLbl x ctx ) ->
            string_update msg x (\new -> MLbl new ctx)

        ( _, MExp (Todo s) ctx ) ->
            case msg of
                Delete ->
                    MExp (Todo (String.slice 0 -1 s)) ctx

                InsertString s2 ->
                    MExp (Todo (String.append s s2)) ctx

                Space ->
                    MExp (Todo (String.append s " ")) ctx

                Open ->
                    MExp (Todo (String.append s "(")) ctx

                Close ->
                    MExp (Todo (String.append s ")")) ctx

                Submit ->
                    case String.toInt s of
                        Just n ->
                            MExp (Num n) ctx

                        Nothing ->
                            -- if s == "+" then
                            --     next_hole (MExp (Bop Add (Todo "") (Todo "")) ctx)
                            -- else if s == "-" then
                            --     next_hole (MExp (Bop Sub (Todo "") (Todo "")) ctx)
                            -- else if s == "*" then
                            --     next_hole (MExp (Bop Mul (Todo "") (Todo "")) ctx)
                            -- else if s == "/" then
                            --     next_hole (MExp (Bop Div (Todo "") (Todo "")) ctx)
                            -- else if s == "==" then
                            --     next_hole (MExp (Bop Eq (Todo "") (Todo "")) ctx)
                            if s == "let" then
                                next_hole (MExp (Let "" (Todo "") (Todo "")) ctx)

                            else if s == "fun" then
                                next_hole (MExp (Fun "" (Todo "")) ctx)

                            else if s == "#yes" then
                                next_hole (MExp expr_inl ctx)

                            else if s == "#no" then
                                next_hole (MExp expr_inr ctx)

                            else if s == "match" then
                                next_hole (MExp expr_match ctx)

                            else if s == "label" then
                                next_hole (MExp expr_label ctx)

                            else if String.length s > 0 then
                                MExp (Var s) ctx

                            else
                                next_hole mdl

                _ ->
                    mdl

        ( Space, MExp e ctx ) ->
            MExp (Todo "") (App2 e () ctx)

        ( Submit, MExp _ _ ) ->
            next_hole mdl

        ( Delete, MExp _ ctx ) ->
            MExp (Todo "") ctx

        ( _, MExp (Var x) ctx ) ->
            string_update msg x (\new -> MExp (Var new) ctx)

        _ ->
            mdl


html_num : Int -> Html.Html msg
html_num n =
    span [ style "color" "green" ] [ text (String.fromInt n) ]


html_var : String -> Html.Html msg
html_var s =
    if String.length s == 0 then
        html_hint "?"

    else
        span
            [ style "font-style" "italic"
            , style "white-space" "pre"
            ]
            [ text s ]


html_keyword : String -> Html.Html msg
html_keyword s =
    span
        [ style "font-weight" "bold"
        , style "white-space" "pre"
        ]
        [ text s ]


html_delimit : String -> Html.Html msg
html_delimit s =
    span
        [ style "white-space" "pre"
        , style "color" "grey"
        ]
        [ text s ]


html_tag : String -> Html.Html msg
html_tag s =
    if String.length s == 0 then
        html_hint "?"

    else
        span
            [ style "white-space" "pre"
            , style "color" "brown"
            ]
            [ text s ]


html_hint : String -> Html.Html msg
html_hint s =
    span
        [ style "white-space" "pre"
        , style "color" "grey"
        ]
        [ text s ]



-- html_bop : Op2 -> Html.Html msg -> Html.Html msg -> Html.Html msg
-- html_bop o h1 h2 =
--     case o of
--         Add ->
--             span [] [ html_delimit "(", h1, html_keyword " + ", h2, html_delimit ")" ]
--         Sub ->
--             span [] [ html_delimit "(", h1, html_keyword " - ", h2, html_delimit ")" ]
--         Mul ->
--             span [] [ html_delimit "(", h1, html_keyword " * ", h2, html_delimit ")" ]
--         Div ->
--             span [] [ html_delimit "(", h1, html_keyword " / ", h2, html_delimit ")" ]
--         Eq ->
--             span [] [ html_delimit "(", h1, html_keyword " == ", h2, html_delimit ")" ]
--         Pair ->
--             span [] [ html_delimit "(", h1, html_keyword ", ", h2, html_delimit ")" ]


html_app : Html.Html msg -> Html.Html msg -> Html.Html msg
html_app h1 h2 =
    span [] [ html_delimit "(", h1, html_keyword " ", h2, html_delimit ")" ]


html_indent : Html.Html msg -> Html.Html msg
html_indent h =
    div [ style "padding-left" "2ex", style "display" "inline-block" ] [ h ]


html_vertical : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
html_vertical attrs children =
    div ([ style "display" "inline-flex", style "flex-direction" "column" ] ++ attrs)
        (List.map (\h -> span [] [ h ]) children)


html_let : Html.Html msg -> Html.Html msg -> Html.Html msg -> Html.Html msg
html_let h1 h2 h3 =
    html_vertical []
        [ span [] [ html_keyword "let ", h1, html_keyword " = ", h2 ]
        , h3
        ]


html_fun : Html.Html msg -> Html.Html msg -> Html.Html msg
html_fun h1 h2 =
    html_vertical []
        [ span [] [ html_keyword "fun ", h1, text ":" ]
        , html_indent h2
        , span [] [ html_keyword "end." ]
        ]


html_inl : Html.Html msg -> Html.Html msg
html_inl h =
    span [] [ html_tag "#yes ", h ]


html_inr : Html.Html msg -> Html.Html msg
html_inr h =
    span [] [ html_tag "#no ", h ]


html_label : Html.Html msg -> Html.Html msg -> Html.Html msg
html_label h1 h2 =
    span [] [ html_delimit "(", h1, text " = ", h2, html_delimit ")" ]


html_match : Html.Html msg -> Html.Html msg -> Html.Html msg -> Html.Html msg -> Html.Html msg -> Html.Html msg
html_match h1 h2 h3 h4 h5 =
    html_vertical []
        [ span [] [ html_keyword "match ", h1 ]
        , span [] [ html_inl h2 ]
        , html_indent h3
        , span [] [ html_inr h4 ]
        , html_indent h5
        , html_keyword "end."
        ]


make_prefix_filler : String -> Expr -> Filler
make_prefix_filler expected_s e s =
    if String.startsWith s expected_s then
        [ e ]

    else
        []


suggest_list : List (String -> List Expr)
suggest_list =
    List.map (\e -> make_prefix_filler (show (show_of_expr e)) e)
        [ expr_let
        , expr_fun
        , expr_app
        , expr_inl
        , expr_inr
        , expr_match
        , expr_label
        ]



-- , expr_bop Add
-- , expr_bop Sub
-- , expr_bop Mul
-- , expr_bop Div
-- , expr_bop Eq


suggest_fills : String -> List (Html.Html msg)
suggest_fills s =
    List.map
        (\e ->
            div
                [ style "margin" "1px"
                , style "background-color" "white"
                ]
                [ html_of_expr e ]
        )
        (List.concatMap (\f -> f s) suggest_list)


html_focused_todo : String -> Html.Html msg
html_focused_todo s =
    div
        [ style "display" "inline-block"
        , style "position" "relative"
        ]
        [ html_todo s
        , div
            [ style "position" "absolute"
            , style "top" "100%"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "background-color" "lightgrey"
            ]
            (suggest_fills s)
        ]


html_todo : String -> Html.Html msg
html_todo s =
    span
        [ style "white-space" "pre"
        ]
        [ text ("[" ++ s ++ "]") ]



-- if String.length s == 0 then
--     html_hint "..."
-- else
--     span
--         [ style "background-color" "lightgrey"
--         , style "white-space" "pre"
--         ]
--         [ text ("[" ++ s ++ "]") ]


html_of_expr : Expr -> Html.Html msg
html_of_expr e =
    case e of
        Todo s ->
            html_todo s

        Num n ->
            html_num n

        Var s ->
            html_var s

        -- Bop o e1 e2 ->
        --     html_bop o (html_of_expr e1) (html_of_expr e2)
        App e1 e2 ->
            html_app (html_of_expr e1) (html_of_expr e2)

        Let x e1 e2 ->
            html_let (html_var x) (html_of_expr e1) (html_of_expr e2)

        Fun x e1 ->
            html_fun (html_var x) (html_of_expr e1)

        Label s e1 ->
            html_label (html_tag s) (html_of_expr e1)

        Inl e1 ->
            html_inl (html_of_expr e1)

        Inr e1 ->
            html_inr (html_of_expr e1)

        Match e1 xl el xr er ->
            html_match (html_of_expr e1) (html_var xl) (html_of_expr el) (html_var xr) (html_of_expr er)


html_of_focused_expr : Expr -> Html.Html msg
html_of_focused_expr e =
    case e of
        Todo s ->
            html_focused_todo s

        _ ->
            html_of_expr e


frame_html : Html.Html msg -> Html.Html msg
frame_html h =
    span [ style "outline" "1px solid red" ] [ h ]


embed_html_in_vctx : Html.Html msg -> VarContext -> Html.Html msg
embed_html_in_vctx h ctx =
    case ctx of
        Let1 () e1 e2 rest ->
            embed_html_in_ectx (html_let h (html_of_expr e1) (html_of_expr e2)) rest

        Fun1 () e1 rest ->
            embed_html_in_ectx (html_fun h (html_of_expr e1)) rest

        Match2 e1 () el xr er rest ->
            embed_html_in_ectx (html_match (html_of_expr e1) h (html_of_expr el) (html_var xr) (html_of_expr er)) rest

        Match4 e1 xl el () er rest ->
            embed_html_in_ectx (html_match (html_of_expr e1) (html_var xl) (html_of_expr el) h (html_of_expr er)) rest


embed_html_in_lctx : Html.Html msg -> LblContext -> Html.Html msg
embed_html_in_lctx h ctx =
    case ctx of
        Label1 () e rest ->
            embed_html_in_ectx (html_label h (html_of_expr e)) rest


embed_html_in_ectx : Html.Html msg -> ExprContext -> Html.Html msg
embed_html_in_ectx h ctx =
    case ctx of
        EmptyContext ->
            h

        -- Bop1 o () e rest ->
        --     embed_html_in_ectx (html_bop o h (html_of_expr e)) rest
        -- Bop2 o e () rest ->
        --     embed_html_in_ectx (html_bop o (html_of_expr e) h) rest
        App1 () e rest ->
            embed_html_in_ectx (html_app h (html_of_expr e)) rest

        App2 e () rest ->
            embed_html_in_ectx (html_app (html_of_expr e) h) rest

        Let2 x () e2 rest ->
            embed_html_in_ectx (html_let (html_var x) h (html_of_expr e2)) rest

        Let3 x e1 () rest ->
            embed_html_in_ectx (html_let (html_var x) (html_of_expr e1) h) rest

        Fun2 x () rest ->
            embed_html_in_ectx (html_fun (html_var x) h) rest

        Inl1 () rest ->
            embed_html_in_ectx (html_inl h) rest

        Inr1 () rest ->
            embed_html_in_ectx (html_inr h) rest

        Label2 s () rest ->
            embed_html_in_ectx (html_label (html_tag s) h) rest

        Match1 () xl el xr er rest ->
            embed_html_in_ectx (html_match h (html_var xl) (html_of_expr el) (html_var xr) (html_of_expr er)) rest

        Match3 e1 xl () xr er rest ->
            embed_html_in_ectx (html_match (html_of_expr e1) (html_var xl) h (html_var xr) (html_of_expr er)) rest

        Match5 e1 xl el xr () rest ->
            embed_html_in_ectx (html_match (html_of_expr e1) (html_var xl) (html_of_expr el) (html_var xr) h) rest


keyDownDecoder : Decode.Decoder Msg
keyDownDecoder =
    Decode.map keyDownToMsg (Decode.field "key" Decode.string)


keyDownToMsg : String -> Msg
keyDownToMsg s =
    case s of
        "ArrowUp" ->
            Move Up

        "ArrowDown" ->
            Move Down

        "ArrowLeft" ->
            Move Left

        "ArrowRight" ->
            Move Right

        "Backspace" ->
            Delete

        " " ->
            Space

        "Enter" ->
            Submit

        "(" ->
            Open

        ")" ->
            Close

        _ ->
            if String.length s == 1 then
                InsertString s

            else
                Noop


subscriptions : Model -> Sub Msg
subscriptions _ =
    batch [ onKeyDown keyDownDecoder ]


view : Model -> Html.Html Msg
view m =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "height" "100vh"
        ]
        [ div
            [ style "width" "50vw"
            ]
            [ view_model m ]
        , div
            [ style "white-space" "pre"
            , style "font-family" "Courier New, Courier, monospace"
            , style "width" "50vw"
            , style "flex-grow" "0"
            , style "flex-shrink" "0"
            ]
            [ text
                (show
                    (show_of_expr (plugin m))
                )
            ]

        -- , view_model m
        ]


view_model : Model -> Html.Html Msg
view_model m =
    case m of
        MVar x ctx ->
            embed_html_in_vctx (frame_html (html_var x)) ctx

        MLbl x ctx ->
            embed_html_in_lctx (frame_html (html_tag x)) ctx

        MExp x ctx ->
            embed_html_in_ectx (frame_html (html_of_focused_expr x)) ctx
