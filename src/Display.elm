module Display exposing (..)

import Expr exposing (..)
import Show exposing (..)


show_literal s =
    Text s


show_hint s =
    Text s


show_keyword s =
    Text s


show_delimit s =
    Text s


show_tag s =
    Text s


show_num : Int -> ShowExpr
show_num n =
    show_literal (String.fromInt n)


show_var : String -> ShowExpr
show_var s =
    if String.length s == 0 then
        show_hint "?"

    else
        Text s


show_bop : Op2 -> ShowExpr -> ShowExpr -> ShowExpr
show_bop o h1 h2 =
    case o of
        Add ->
            HBox [ show_delimit "(", h1, show_keyword " + ", h2, show_delimit ")" ]

        Sub ->
            HBox [ show_delimit "(", h1, show_keyword " - ", h2, show_delimit ")" ]

        Mul ->
            HBox [ show_delimit "(", h1, show_keyword " * ", h2, show_delimit ")" ]

        Div ->
            HBox [ show_delimit "(", h1, show_keyword " / ", h2, show_delimit ")" ]

        Eq ->
            HBox [ show_delimit "(", h1, show_keyword " == ", h2, show_delimit ")" ]

        Pair ->
            HBox [ show_delimit "(", h1, show_keyword ", ", h2, show_delimit ")" ]


show_app : ShowExpr -> ShowExpr -> ShowExpr
show_app h1 h2 =
    HBox [ show_delimit "(", h1, show_keyword " ", h2, show_delimit ")" ]


show_indent : ShowExpr -> ShowExpr
show_indent h =
    Indent h


show_vertical : List ShowExpr -> ShowExpr
show_vertical children =
    VBox children


show_let : ShowExpr -> ShowExpr -> ShowExpr -> ShowExpr
show_let h1 h2 h3 =
    show_vertical
        [ HBox [ show_keyword "let ", h1, show_keyword " = ", h2 ]
        , h3
        ]


show_fun : ShowExpr -> ShowExpr -> ShowExpr
show_fun h1 h2 =
    VBox
        [ HBox [ show_keyword "fun ", h1, Text ":" ]
        , show_indent h2
        , HBox [ show_keyword "end." ]
        ]


show_inl : ShowExpr -> ShowExpr
show_inl h =
    HBox [ show_tag "#yes ", h ]


show_inr : ShowExpr -> ShowExpr
show_inr h =
    HBox [ show_tag "#no ", h ]


show_label : ShowExpr -> ShowExpr -> ShowExpr
show_label h1 h2 =
    HBox [ Text "(", h1, Text " = ", h2, Text ")" ]


show_match : ShowExpr -> ShowExpr -> ShowExpr -> ShowExpr -> ShowExpr -> ShowExpr
show_match h1 h2 h3 h4 h5 =
    VBox
        [ HBox [ show_keyword "match ", h1 ]
        , show_inl h2
        , show_indent h3
        , show_inr h4
        , show_indent h5
        , show_keyword "end."
        ]


show_todo : String -> ShowExpr
show_todo s =
    if String.length s == 0 then
        show_hint "..."

    else
        HBox [ Text "[", Text s, Text "]" ]


show_of_expr : Expr -> ShowExpr
show_of_expr e =
    case e of
        Todo s ->
            show_todo s

        Num n ->
            show_num n

        Var s ->
            show_var s

        Bop o e1 e2 ->
            show_bop o (show_of_expr e1) (show_of_expr e2)

        App e1 e2 ->
            show_app (show_of_expr e1) (show_of_expr e2)

        Let x e1 e2 ->
            show_let (show_var x) (show_of_expr e1) (show_of_expr e2)

        Fun x e1 ->
            show_fun (show_var x) (show_of_expr e1)

        Label s e1 ->
            show_label (show_tag s) (show_of_expr e1)

        Inl e1 ->
            show_inl (show_of_expr e1)

        Inr e1 ->
            show_inr (show_of_expr e1)

        Match e1 xl el xr er ->
            show_match (show_of_expr e1) (show_var xl) (show_of_expr el) (show_var xr) (show_of_expr er)
