module HtmlDisplay exposing (..)

import Expr exposing (..)
import Html exposing (div, span, text)
import Html.Attributes exposing (style)


html_num : Int -> Html.Html msg
html_num n =
    span [ style "color" "green" ] [ text (String.fromInt n) ]


html_var : String -> Html.Html msg
html_var s =
    if String.length s == 0 then
        html_hint "var"

    else
        span
            [ style "font-style" "italic"
            , style "white-space" "pre"
            ]
            [ text (String.replace " " "_" s) ]


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
        html_hint "#?"

    else
        span
            [ style "white-space" "pre"
            , style "color" "brown"
            ]
            [ text ("#" ++ s) ]


html_hint : String -> Html.Html msg
html_hint s =
    span
        [ style "white-space" "pre"
        , style "color" "grey"
        ]
        [ text s ]


html_text : String -> Html.Html msg
html_text s =
    span
        [ style "white-space" "pre"
        , style "color" "black"
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
        [ span [] [ html_keyword "let ", h1, html_keyword " = "]
        , html_indent h2 
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
html_inl h = html_label (html_tag "yes") h

html_inr : Html.Html msg -> Html.Html msg
html_inr h = html_label (html_tag "no") h


html_label : Html.Html msg -> Html.Html msg -> Html.Html msg
html_label h1 h2 =
    span [] [ h1, html_text " ", h2 ]


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


html_field : Html.Html msg -> Html.Html msg -> Html.Html msg
html_field lh eh =
    span [] [ lh, html_delimit " : ", eh ]


html_record : List (Html.Html msg) -> Html.Html msg
html_record hs =
    span [] (html_keyword "(" :: List.intersperse (html_delimit ", ") hs ++ [ html_keyword ")" ])


html_focused_todo : String -> List Expr -> Html.Html msg
html_focused_todo s es =
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
            , style "gap" "1px"
            , style "border" "1px solid lightgrey"
            ]
            (List.map
                (\e ->
                    div
                        [ style "background-color" "white"
                        ]
                        [ html_of_expr e ]
                )
                es
            )
        ]


html_todo : String -> Html.Html msg
html_todo s =
    if String.length s > 0 then
        html_text s

    else
        html_hint "expr"


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

        Inject s e1 ->
            html_label (html_tag s) (html_of_expr e1)

        Match e1 xl el xr er ->
            html_match (html_of_expr e1) (html_var xl) (html_of_expr el) (html_var xr) (html_of_expr er)

        Record fs ->
            html_record (List.map html_of_field fs)


html_of_field : ( String, Expr ) -> Html.Html msg
html_of_field ( l, e ) =
    html_field (html_tag l) (html_of_expr e)


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
        Inject1 () e rest ->
            embed_html_in_ectx (html_label h (html_of_expr e)) rest

        Record1 fs1 () e fs2 rest ->
            embed_html_in_ectx
                (html_record
                    (List.reverse (List.map html_of_field fs1)
                        ++ html_field h (html_of_expr e)
                        :: List.map html_of_field fs2
                    )
                )
                rest


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

        Inject2 s () rest ->
            embed_html_in_ectx (html_label (html_tag s) h) rest

        Match1 () xl el xr er rest ->
            embed_html_in_ectx (html_match h (html_var xl) (html_of_expr el) (html_var xr) (html_of_expr er)) rest

        Match3 e1 xl () xr er rest ->
            embed_html_in_ectx (html_match (html_of_expr e1) (html_var xl) h (html_var xr) (html_of_expr er)) rest

        Match5 e1 xl el xr () rest ->
            embed_html_in_ectx (html_match (html_of_expr e1) (html_var xl) (html_of_expr el) (html_var xr) h) rest

        Record2 fs1 l () fs2 rest ->
            embed_html_in_ectx
                (html_record
                    (List.reverse (List.map html_of_field fs1)
                        ++ html_field (html_tag l) h
                        :: List.map html_of_field fs2
                    )
                )
                rest
