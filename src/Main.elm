module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
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


type Op2
    = Add
    | Sub
    | Mul
    | Div
    | Eq
    | Pair


type Expr
    = Todo String
    | Num Int
    | Var String
    | Bop Op2 Expr Expr
    | Let String Expr Expr
    | Fun String Expr
    | App Expr Expr
    | Case Expr String Expr String Expr
    | Inl Expr
    | Inr Expr


type ExprContext
    = EmptyContext
    | Inl1 () ExprContext
    | Inr1 () ExprContext
    | Bop1 Op2 () Expr ExprContext
    | Bop2 Op2 Expr () ExprContext
    | Let2 String () Expr ExprContext
    | Let3 String Expr () ExprContext
    | Fun2 String () ExprContext
    | App1 () Expr ExprContext
    | App2 Expr () ExprContext
    | Case1 () String Expr String Expr ExprContext
    | Case3 Expr String () String Expr ExprContext
    | Case5 Expr String Expr String () ExprContext


type VarContext
    = Var1 () ExprContext
    | Let1 () Expr Expr ExprContext
    | Fun1 () Expr ExprContext
    | Case2 Expr () Expr String Expr ExprContext
    | Case4 Expr String Expr () Expr ExprContext


type Model
    = ExprModel Expr ExprContext
    | VarModel String VarContext


next_sibling : Expr -> ExprContext -> Model
next_sibling e ctx =
    case ctx of
        EmptyContext ->
            ExprModel e ctx

        Bop1 o () e2 rest ->
            ExprModel e2 (Bop2 o e () rest)

        Bop2 o e1 () rest ->
            next_sibling (Bop o e1 e) rest

        Let2 x () e2 rest ->
            ExprModel e2 (Let3 x e () rest)

        Let3 x e1 () rest ->
            next_sibling (Let x e1 e) rest

        Fun2 x () rest ->
            next_sibling (Fun x e) rest

        App1 () e2 rest ->
            ExprModel e2 (App2 e () rest)

        App2 e1 () rest ->
            next_sibling (App e1 e) rest

        Inl1 () rest ->
            next_sibling (Inl e) rest

        Inr1 () rest ->
            next_sibling (Inr e) rest

        Case1 () xl el xr er rest ->
            VarModel xl (Case2 e () el xr er rest)

        Case3 e1 xl () xr er rest ->
            VarModel xr (Case4 e1 xl e () er rest)

        Case5 e1 xl el xr () rest ->
            next_sibling (Case e1 xl el xr e) rest


next_hole : Model -> Model
next_hole m =
    case m of
        VarModel x (Var1 () ctx) ->
            next_sibling (Var x) ctx

        VarModel x (Let1 () e1 e2 ctx) ->
            ExprModel e1 (Let2 x () e2 ctx)

        VarModel x (Fun1 () e1 ctx) ->
            ExprModel e1 (Fun2 x () ctx)

        VarModel xl (Case2 e1 () el xr er ctx) ->
            ExprModel el (Case3 e1 xl () xr er ctx)

        VarModel xr (Case4 e1 xl el () er ctx) ->
            ExprModel er (Case5 e1 xl el xr () ctx)

        ExprModel (Todo s) ctx ->
            next_sibling (Todo s) ctx

        ExprModel (Num n) ctx ->
            next_sibling (Num n) ctx

        ExprModel (Var x) ctx ->
            next_sibling (Var x) ctx

        ExprModel (Bop o e1 e2) ctx ->
            ExprModel e1 (Bop1 o () e2 ctx)

        ExprModel (App e1 e2) ctx ->
            ExprModel e1 (App1 () e2 ctx)

        ExprModel (Let x e1 e2) ctx ->
            VarModel x (Let1 () e1 e2 ctx)

        ExprModel (Fun x e1) ctx ->
            VarModel x (Fun1 () e1 ctx)

        ExprModel (Case e1 xl el xr er) ctx ->
            ExprModel e1 (Case1 () xl el xr er ctx)

        ExprModel (Inl e1) ctx ->
            ExprModel e1 (Inl1 () ctx)

        ExprModel (Inr e1) ctx ->
            ExprModel e1 (Inr1 () ctx)


prev_sibling : Expr -> ExprContext -> Model
prev_sibling e ctx =
    case e of
        Var x ->
            VarModel x (Var1 () ctx)

        Todo _ ->
            ExprModel e ctx

        Num _ ->
            ExprModel e ctx

        Bop o e1 e2 ->
            prev_sibling e2 (Bop2 o e1 () ctx)

        App e1 e2 ->
            prev_sibling e2 (App2 e1 () ctx)

        Let x e1 e2 ->
            prev_sibling e2 (Let3 x e1 () ctx)

        Fun x e1 ->
            prev_sibling e1 (Fun2 x () ctx)

        Case e1 xl el xr er ->
            prev_sibling er (Case5 e1 xl el xr () ctx)

        Inl e1 ->
            prev_sibling e1 (Inl1 () ctx)

        Inr e1 ->
            prev_sibling e1 (Inr1 () ctx)


prev_hole : Model -> Model
prev_hole m =
    case m of
        ExprModel e EmptyContext ->
            prev_sibling e EmptyContext

        VarModel x (Var1 () ctx) ->
            prev_hole (ExprModel (Var x) ctx)

        ExprModel e1 (Bop1 o () e2 ctx) ->
            ExprModel (Bop o e1 e2) ctx

        ExprModel e2 (Bop2 o e1 () ctx) ->
            prev_sibling e1 (Bop1 o () e2 ctx)

        ExprModel e1 (App1 () e2 ctx) ->
            ExprModel (App e1 e2) ctx

        ExprModel e2 (App2 e1 () ctx) ->
            prev_sibling e1 (App1 () e2 ctx)

        VarModel x (Let1 () e1 e2 ctx) ->
            ExprModel (Let x e1 e2) ctx

        ExprModel e1 (Let2 x () e2 ctx) ->
            VarModel x (Let1 () e1 e2 ctx)

        ExprModel e2 (Let3 x e1 () ctx) ->
            prev_sibling e1 (Let2 x () e2 ctx)

        VarModel x (Fun1 () e1 ctx) ->
            ExprModel (Fun x e1) ctx

        ExprModel e1 (Fun2 x () ctx) ->
            VarModel x (Fun1 () e1 ctx)

        ExprModel e1 (Case1 () xl el xr er ctx) ->
            ExprModel (Case e1 xl el xr er) ctx

        VarModel xl (Case2 e1 () el xr er ctx) ->
            prev_sibling e1 (Case1 () xl el xr er ctx)

        ExprModel el (Case3 e1 xl () xr er ctx) ->
            VarModel xl (Case2 e1 () el xr er ctx)

        VarModel xr (Case4 e1 xl el () er ctx) ->
            prev_sibling el (Case3 e1 xl () xr er ctx)

        ExprModel er (Case5 e1 xl el xr () ctx) ->
            VarModel xr (Case4 e1 xl el () er ctx)

        ExprModel e1 (Inl1 () ctx) ->
            ExprModel (Inl e1) ctx

        ExprModel e1 (Inr1 () ctx) ->
            ExprModel (Inr e1) ctx


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

                            else if s == "case" then
                                next_hole (ExprModel (Case (Todo "") "x" (Todo "") "x" (Todo "")) ctx)

                            else
                                ExprModel (Var s) ctx

                _ ->
                    mdl

        ( Delete, VarModel _ ctx ) ->
            VarModel "" ctx

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
        ]
        [ text s ]


html_delimit : String -> Html.Html msg
html_delimit s =
    span
        [ style "white-space" "pre"
        , style "color" "grey"
        ]
        [ text s ]


html_bop : Op2 -> Html.Html msg -> Html.Html msg -> Html.Html msg
html_bop o h1 h2 =
    case o of
        Add ->
            span [] [ html_delimit "(", h1, html_delimit " + ", h2, html_delimit ")" ]

        Sub ->
            span [] [ html_delimit "(", h1, html_delimit " - ", h2, html_delimit ")" ]

        Mul ->
            span [] [ html_delimit "(", h1, html_delimit " * ", h2, html_delimit ")" ]

        Div ->
            span [] [ html_delimit "(", h1, html_delimit " / ", h2, html_delimit ")" ]

        Eq ->
            span [] [ html_delimit "(", h1, html_delimit " == ", h2, html_delimit ")" ]

        Pair ->
            span [] [ html_delimit "(", h1, html_delimit ", ", h2, html_delimit ")" ]


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


html_case : Html.Html msg -> Html.Html msg -> Html.Html msg -> Html.Html msg -> Html.Html msg -> Html.Html msg
html_case h1 h2 h3 h4 h5 =
    html_vertical []
        [ span [] [ html_keyword "case ", h1 ]
        , html_indent
            (span []
                [ html_keyword "when "
                , html_inl h2
                , html_delimit ":"
                ]
            )
        , html_indent (html_indent h3)
        , html_indent
            (span []
                [ html_keyword "when "
                , html_inr h4
                , html_delimit ":"
                ]
            )
        , html_indent (html_indent h5)
        , html_keyword "end"
        ]


html_todo : String -> Html.Html msg
html_todo s =
    span [ style "background-color" "lightgrey" ] [ text "[", text s, text "]" ]


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

        Case e1 xl el xr er ->
            html_case (html_of_expr e1) (html_var xl) (html_of_expr el) (html_var xr) (html_of_expr er)


frame_html : Html.Html msg -> Html.Html msg
frame_html h =
    span [ style "outline" "1px solid red" ] [ h ]


embed_html_in_vctx : Html.Html msg -> VarContext -> Html.Html msg
embed_html_in_vctx h ctx =
    case ctx of
        Var1 () rest ->
            embed_html_in_ectx h rest

        Let1 () e1 e2 rest ->
            embed_html_in_ectx (html_let h (html_of_expr e1) (html_of_expr e2)) rest

        Fun1 () e1 rest ->
            embed_html_in_ectx (html_fun h (html_of_expr e1)) rest

        Case2 e1 () el xr er rest ->
            embed_html_in_ectx (html_case (html_of_expr e1) h (html_of_expr el) (html_var xr) (html_of_expr er)) rest

        Case4 e1 xl el () er rest ->
            embed_html_in_ectx (html_case (html_of_expr e1) (html_var xl) (html_of_expr el) h (html_of_expr er)) rest


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

        Case1 () xl el xr er rest ->
            embed_html_in_ectx (html_case h (html_var xl) (html_of_expr el) (html_var xr) (html_of_expr er)) rest

        Case3 e1 xl () xr er rest ->
            embed_html_in_ectx (html_case (html_of_expr e1) (html_var xl) h (html_var xr) (html_of_expr er)) rest

        Case5 e1 xl el xr () rest ->
            embed_html_in_ectx (html_case (html_of_expr e1) (html_var xl) (html_of_expr el) (html_var xr) h) rest


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
