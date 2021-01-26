module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Expr exposing (..)
import Html exposing (div, span, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Platform.Cmd exposing (none)
import Platform.Sub exposing (batch)


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
    ( ExprModel (Todo "") EmptyContext, none )


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
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    ( update_model msg mdl, none )


update_model : Msg -> Model -> Model
update_model msg mdl =
    case ( msg, mdl ) of
        ( Move Right, _ ) ->
            next_hole mdl

        ( Move Left, _ ) ->
            prev_hole mdl

        ( Move Up, _ ) ->
            up_hole mdl

        ( _, ExprModel (Todo s) ctx ) ->
            case msg of
                Delete ->
                    ExprModel (Todo (String.slice 0 -1 s)) ctx

                InsertString s2 ->
                    ExprModel (Todo (String.append s s2)) ctx

                Open ->
                    ExprModel (Todo (String.append s "(")) ctx

                Close ->
                    ExprModel (Todo (String.append s ")")) ctx

                Submit ->
                    case String.toInt s of
                        Just n ->
                            ExprModel (Num n) ctx

                        Nothing ->
                            if s == "+" then
                                next_hole (ExprModel (Bop Add (Todo "") (Todo "")) ctx)

                            else if s == "-" then
                                next_hole (ExprModel (Bop Sub (Todo "") (Todo "")) ctx)

                            else if s == "*" then
                                next_hole (ExprModel (Bop Mul (Todo "") (Todo "")) ctx)

                            else if s == "/" then
                                next_hole (ExprModel (Bop Div (Todo "") (Todo "")) ctx)

                            else if s == "==" then
                                next_hole (ExprModel (Bop Eq (Todo "") (Todo "")) ctx)

                            else if s == "let" then
                                next_hole (ExprModel (Let "" (Todo "") (Todo "")) ctx)

                            else if s == "fun" then
                                next_hole (ExprModel (Fun "" (Todo "")) ctx)

                            else if s == "#yes" then
                                next_hole (ExprModel (Inl (Todo "")) ctx)

                            else if s == "#no" then
                                next_hole (ExprModel (Inr (Todo "")) ctx)

                            else if s == "match" then
                                next_hole (ExprModel (Match (Todo "") "_" (Todo "") "_" (Todo "")) ctx)

                            else if String.length s > 0 then
                                ExprModel (Var s) ctx

                            else
                                mdl

                _ ->
                    mdl

        ( Delete, VarModel x ctx ) ->
            VarModel (String.slice 0 -1 x) ctx

        ( Open, VarModel x ctx ) ->
            VarModel (String.append x "(") ctx

        ( Close, VarModel x ctx ) ->
            VarModel (String.append x ")") ctx

        ( InsertString s, VarModel x ctx ) ->
            VarModel (String.append x s) ctx

        ( Submit, VarModel _ _ ) ->
            next_hole mdl

        ( Open, ExprModel e ctx ) ->
            ExprModel (Todo "") (App2 e () ctx)

        ( Delete, ExprModel _ ctx ) ->
            ExprModel (Todo "") ctx

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
        span [ style "font-style" "italic" ] [ text s ]


html_keyword : String -> Html.Html msg
html_keyword s =
    span
        [ style "font-weight" "bold"
        , style "white-space" "pre"
        ]
        [ text s ]


html_label : String -> Html.Html msg
html_label s =
    span
        [ style "white-space" "pre"
        , style "color" "brown"
        ]
        [ text s ]


html_delimit : String -> Html.Html msg
html_delimit s =
    span
        [ style "white-space" "pre"
        , style "color" "grey"
        ]
        [ text s ]


html_hint : String -> Html.Html msg
html_hint s =
    span
        [ style "white-space" "pre"
        , style "color" "grey"
        ]
        [ text s ]


html_bop : Op2 -> Html.Html msg -> Html.Html msg -> Html.Html msg
html_bop o h1 h2 =
    case o of
        Add ->
            span [] [ html_delimit "(", h1, html_keyword " + ", h2, html_delimit ")" ]

        Sub ->
            span [] [ html_delimit "(", h1, html_keyword " - ", h2, html_delimit ")" ]

        Mul ->
            span [] [ html_delimit "(", h1, html_keyword " * ", h2, html_delimit ")" ]

        Div ->
            span [] [ html_delimit "(", h1, html_keyword " / ", h2, html_delimit ")" ]

        Eq ->
            span [] [ html_delimit "(", h1, html_keyword " == ", h2, html_delimit ")" ]

        Pair ->
            span [] [ html_delimit "(", h1, html_keyword ", ", h2, html_delimit ")" ]


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
        [ span [] [ html_keyword "fun ", h1, html_delimit ":" ]
        , html_indent h2
        , span [] [ html_keyword "end" ]
        ]


html_inl : Html.Html msg -> Html.Html msg
html_inl h =
    span [] [ html_label "#yes ", h ]


html_inr : Html.Html msg -> Html.Html msg
html_inr h =
    span [] [ html_label "#no ", h ]


html_match : Html.Html msg -> Html.Html msg -> Html.Html msg -> Html.Html msg -> Html.Html msg -> Html.Html msg
html_match h1 h2 h3 h4 h5 =
    html_vertical []
        [ span [] [ html_keyword "match ", h1 ]
        , span []
            [ html_keyword "when "
            , html_inl h2
            , html_keyword " then"
            ]
        , html_indent h3
        , span []
            [ html_keyword "when "
            , html_inr h4
            , html_keyword " then"
            ]
        , html_indent h5
        , html_keyword "end."
        ]


html_todo : String -> Html.Html msg
html_todo s =
    if String.length s == 0 then
        html_hint "..."

    else
        div
            [ style "display" "inline-block"
            , style "position" "relative"
            ]
            [ span [ style "background-color" "lightgrey" ] [ text "[", text s, text "]" ]
            , div
                [ style "position" "absolute"
                , style "top" "100%"
                , style "display" "flex"
                ]
                [ text "hello", text "world" ]
            ]


html_of_expr : Expr -> Html.Html msg
html_of_expr e =
    case e of
        Todo s ->
            html_todo s

        Num n ->
            html_num n

        Var s ->
            html_var s

        Bop o e1 e2 ->
            html_bop o (html_of_expr e1) (html_of_expr e2)

        App e1 e2 ->
            html_app (html_of_expr e1) (html_of_expr e2)

        Let x e1 e2 ->
            html_let (html_var x) (html_of_expr e1) (html_of_expr e2)

        Fun x e1 ->
            html_fun (html_var x) (html_of_expr e1)

        Inl e1 ->
            html_inl (html_of_expr e1)

        Inr e1 ->
            html_inr (html_of_expr e1)

        Match e1 xl el xr er ->
            html_match (html_of_expr e1) (html_var xl) (html_of_expr el) (html_var xr) (html_of_expr er)


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


embed_html_in_ectx : Html.Html msg -> ExprContext -> Html.Html msg
embed_html_in_ectx h ctx =
    case ctx of
        EmptyContext ->
            h

        Bop1 o () e rest ->
            embed_html_in_ectx (html_bop o h (html_of_expr e)) rest

        Bop2 o e () rest ->
            embed_html_in_ectx (html_bop o (html_of_expr e) h) rest

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
    case m of
        VarModel x ctx ->
            embed_html_in_vctx (frame_html (html_var x)) ctx

        ExprModel x ctx ->
            embed_html_in_ectx (frame_html (html_of_expr x)) ctx
