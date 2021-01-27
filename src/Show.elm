module Show exposing (..)

import List exposing (head)


type ShowExpr
    = Text String
    | Indent ShowExpr
    | HBox (List ShowExpr)
    | VBox (List ShowExpr)


show : ShowExpr -> String
show e =
    String.join "\n" (string_list_of_term (term_of_show_expr e))


term_of_show_expr : ShowExpr -> Term
term_of_show_expr e =
    case e of
        Text s ->
            TText s

        Indent e1 ->
            TIndent (term_of_show_expr e1)

        HBox es ->
            List.foldr THCons TNil (List.map term_of_show_expr es)

        VBox es ->
            List.foldr TVCons TNil (List.map term_of_show_expr es)


type Term
    = TNil
    | TText String
    | TIndent Term
    | THCons Term Term
    | TVCons Term Term


indent : Int -> String -> String
indent n x =
    String.append (String.fromList (List.repeat n ' ')) x


string_list_of_term : Term -> List String
string_list_of_term input =
    case input of
        TNil ->
            []

        TText s ->
            [ s ]

        TIndent subterm ->
            let
                ss =
                    string_list_of_term subterm
            in
            List.map (indent 2) ss

        THCons t1 t2 ->
            let
                ss1 =
                    string_list_of_term t1

                ss2 =
                    string_list_of_term t2

                last_ss1 =
                    Maybe.withDefault "" (List.head (List.reverse ss1))

                but_last_ss1 =
                    Maybe.withDefault []
                        (Maybe.map
                            (\xs -> List.reverse xs)
                            (List.tail (List.reverse ss1))
                        )

                first_ss2 =
                    Maybe.withDefault "" (List.head ss2)

                but_first_ss2 =
                    Maybe.withDefault [] (List.tail ss2)
            in
            but_last_ss1
                ++ String.append last_ss1 first_ss2
                :: List.map (indent (String.length last_ss1)) but_first_ss2

        TVCons t1 t2 ->
            let
                ss1 =
                    string_list_of_term t1

                ss2 =
                    string_list_of_term t2
            in
            ss1 ++ ss2
