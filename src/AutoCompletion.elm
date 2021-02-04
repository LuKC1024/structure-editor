module AutoCompletion exposing (suggest_fills)

import Expr exposing (..)
import Show exposing (..)
import TextDisplay exposing (..)


type alias Filler =
    String -> List Expr


make_prefix_filler : String -> Expr -> Filler
make_prefix_filler expected_s e s =
    if String.startsWith s expected_s then
        [ e ]

    else
        []


suggest_by_prefix : String -> List Expr
suggest_by_prefix s =
    List.concatMap (\e -> make_prefix_filler (show (show_of_expr e)) e s)
        [ expr_let
        , expr_fun
        , expr_app
        , expr_match
        , expr_inject
        , expr_record
        ]


suggest_nums : String -> List Expr
suggest_nums s =
    case String.toInt s of
        Nothing ->
            []

        Just i ->
            [ Num i ]


suggest_vars : String -> List Expr
suggest_vars s =
    [ Var s ]


suggest_fills : String -> List Expr
suggest_fills s =
    suggest_by_prefix s ++ suggest_nums s ++ suggest_vars s
