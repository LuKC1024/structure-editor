module Expr exposing (..)


type Expr
    = Todo String
    | Num Int
    | Var String
    | Let String Expr Expr
    | Fun String Expr
    | App Expr Expr
    | Match Expr String Expr String Expr
    | Inject String Expr
    | Record (List ( String, Expr ))


expr_let : Expr
expr_let =
    Let "" (Todo "") (Todo "")


expr_fun : Expr
expr_fun =
    Fun "" (Todo "")


expr_app : Expr
expr_app =
    App (Todo "") (Todo "")


expr_match : Expr
expr_match =
    Match (Todo "") "" (Todo "") "" (Todo "")


expr_inject : Expr
expr_inject =
    Inject "" (Todo "")


expr_record : Expr
expr_record =
    Record []


type ExprContext
    = EmptyContext
    | Let2 String () Expr ExprContext
    | Let3 String Expr () ExprContext
    | Fun2 String () ExprContext
    | App1 () Expr ExprContext
    | App2 Expr () ExprContext
    | Match1 () String Expr String Expr ExprContext
    | Match3 Expr String () String Expr ExprContext
    | Match5 Expr String Expr String () ExprContext
    | Inject2 String () ExprContext
    | Record2 (List ( String, Expr )) String () (List ( String, Expr )) ExprContext


type VarContext
    = Let1 () Expr Expr ExprContext
    | Fun1 () Expr ExprContext
    | Match2 Expr () Expr String Expr ExprContext
    | Match4 Expr String Expr () Expr ExprContext


type LblContext
    = Inject1 () Expr ExprContext
    | Record1 (List ( String, Expr )) () Expr (List ( String, Expr )) ExprContext


type ExprFocus
    = MExp Expr ExprContext
    | MVar String VarContext
    | MLbl String LblContext


nav_up : ExprFocus -> Maybe ExprFocus
nav_up m =
    case m of
        MExp _ EmptyContext ->
            Nothing

        -- MExp e1 (Bop1 o () e2 ctx) ->
        --     Just (MExp (Bop o e1 e2) ctx)
        -- MExp e2 (Bop2 o e1 () ctx) ->
        --     Just (MExp (Bop o e1 e2) ctx)
        MVar x (Let1 () e1 e2 ctx) ->
            Just (MExp (Let x e1 e2) ctx)

        MExp e1 (Let2 x () e2 ctx) ->
            Just (MExp (Let x e1 e2) ctx)

        MExp e2 (Let3 x e1 () ctx) ->
            Just (MExp (Let x e1 e2) ctx)

        MVar x (Fun1 () e ctx) ->
            Just (MExp (Fun x e) ctx)

        MExp e (Fun2 x () ctx) ->
            Just (MExp (Fun x e) ctx)

        MExp e1 (App1 () e2 ctx) ->
            Just (MExp (App e1 e2) ctx)

        MExp e2 (App2 e1 () ctx) ->
            Just (MExp (App e1 e2) ctx)

        MLbl x (Inject1 () e ctx) ->
            Just (MExp (Inject x e) ctx)

        MExp e (Inject2 x () ctx) ->
            Just (MExp (Inject x e) ctx)

        MExp e (Match1 () xl el xr er ctx) ->
            Just (MExp (Match e xl el xr er) ctx)

        MVar xl (Match2 e () el xr er ctx) ->
            Just (MExp (Match e xl el xr er) ctx)

        MExp el (Match3 e xl () xr er ctx) ->
            Just (MExp (Match e xl el xr er) ctx)

        MVar xr (Match4 e xl el () er ctx) ->
            Just (MExp (Match e xl el xr er) ctx)

        MExp er (Match5 e xl el xr () ctx) ->
            Just (MExp (Match e xl el xr er) ctx)

        MLbl l (Record1 fs1 () e fs2 ctx) ->
            Just (MExp (Record (List.reverse fs1 ++ (( l, e ) :: fs2))) ctx)

        MExp e (Record2 fs1 l () fs2 ctx) ->
            Just (MExp (Record (List.reverse fs1 ++ (( l, e ) :: fs2))) ctx)


nav_right : ExprFocus -> Maybe ExprFocus
nav_right =
    nav_horizontal True


nav_left : ExprFocus -> Maybe ExprFocus
nav_left =
    nav_horizontal False


nav_horizontal : Bool -> ExprFocus -> Maybe ExprFocus
nav_horizontal right m =
    case m of
        MExp _ EmptyContext ->
            Nothing

        -- MExp e1 (Bop1 o () e2 ctx) ->
        --     if right then
        --         Just (MExp e2 (Bop2 o e1 () ctx))
        --     else
        --         Nothing
        -- MExp e2 (Bop2 o e1 () ctx) ->
        --     if right then
        --         Nothing
        --     else
        --         Just (MExp e1 (Bop1 o () e2 ctx))
        MVar x (Let1 () e1 e2 ctx) ->
            if right then
                Just (MExp e1 (Let2 x () e2 ctx))

            else
                Nothing

        MExp e1 (Let2 x () e2 ctx) ->
            if right then
                Just (MExp e2 (Let3 x e1 () ctx))

            else
                Just (MVar x (Let1 () e1 e2 ctx))

        MExp e2 (Let3 x e1 () ctx) ->
            if right then
                Nothing

            else
                Just (MExp e1 (Let2 x () e2 ctx))

        MVar x (Fun1 () e ctx) ->
            if right then
                Just (MExp e (Fun2 x () ctx))

            else
                Nothing

        MExp e (Fun2 x () ctx) ->
            if right then
                Nothing

            else
                Just (MVar x (Fun1 () e ctx))

        MExp e1 (App1 () e2 ctx) ->
            if right then
                Just (MExp e2 (App2 e1 () ctx))

            else
                Nothing

        MExp e2 (App2 e1 () ctx) ->
            if right then
                Nothing

            else
                Just (MExp e1 (App1 () e2 ctx))

        MLbl x (Inject1 () e ctx) ->
            if right then
                Just (MExp e (Inject2 x () ctx))

            else
                Nothing

        MExp e (Inject2 x () ctx) ->
            if right then
                Nothing

            else
                Just (MLbl x (Inject1 () e ctx))

        MExp e (Match1 () xl el xr er ctx) ->
            if right then
                Just (MVar xl (Match2 e () el xr er ctx))

            else
                Nothing

        MVar xl (Match2 e () el xr er ctx) ->
            if right then
                Just (MExp el (Match3 e xl () xr er ctx))

            else
                Just (MExp e (Match1 () xl el xr er ctx))

        MExp el (Match3 e xl () xr er ctx) ->
            if right then
                Just (MVar xr (Match4 e xl el () er ctx))

            else
                Just (MVar xl (Match2 e () el xr er ctx))

        MVar xr (Match4 e xl el () er ctx) ->
            if right then
                Just (MExp er (Match5 e xl el xr () ctx))

            else
                Just (MExp e (Match3 e xl () xr er ctx))

        MExp er (Match5 e xl el xr () ctx) ->
            if right then
                Nothing

            else
                Just (MVar xr (Match4 e xl el () er ctx))

        MLbl l (Record1 fs1 () e fs2 ctx) ->
            if right then
                Just (MExp e (Record2 fs1 l () fs2 ctx))

            else
                case fs1 of
                    [] ->
                        Nothing

                    ( next_l, next_e ) :: next_fs1 ->
                        Just (MExp next_e (Record2 next_fs1 next_l () (( l, e ) :: fs2) ctx))

        MExp e (Record2 fs1 l () fs2 ctx) ->
            if right then
                case fs2 of
                    [] ->
                        Nothing

                    ( next_l, next_e ) :: next_fs2 ->
                        Just (MLbl next_l (Record1 (( l, e ) :: fs1) () next_e next_fs2 ctx))

            else
                Just (MExp (Record (List.reverse fs1 ++ (( l, e ) :: fs2))) ctx)


nav_first_child : ExprFocus -> Maybe ExprFocus
nav_first_child =
    nav_child True


nav_last_child : ExprFocus -> Maybe ExprFocus
nav_last_child =
    nav_child False


nav_child : Bool -> ExprFocus -> Maybe ExprFocus
nav_child first m =
    case m of
        MVar _ _ ->
            Nothing

        MLbl _ _ ->
            Nothing

        MExp (Todo _) _ ->
            Nothing

        MExp (Num _) _ ->
            Nothing

        MExp (Var _) _ ->
            Nothing

        -- MExp (Bop o e1 e2) ctx ->
        --     if first then
        --         Just (MExp e1 (Bop1 o () e2 ctx))
        --     else
        --         Just (MExp e2 (Bop2 o e1 () ctx))
        MExp (App e1 e2) ctx ->
            if first then
                Just (MExp e1 (App1 () e2 ctx))

            else
                Just (MExp e2 (App2 e1 () ctx))

        MExp (Inject x e) ctx ->
            if first then
                Just (MLbl x (Inject1 () e ctx))

            else
                Just (MExp e (Inject2 x () ctx))

        MExp (Let x e1 e2) ctx ->
            if first then
                Just (MVar x (Let1 () e1 e2 ctx))

            else
                Just (MExp e2 (Let3 x e1 () ctx))

        MExp (Fun x e) ctx ->
            if first then
                Just (MVar x (Fun1 () e ctx))

            else
                Just (MExp e (Fun2 x () ctx))

        MExp (Match e xl el xr er) ctx ->
            if first then
                Just (MExp e (Match1 () xl el xr er ctx))

            else
                Just (MExp er (Match5 e xl el xr () ctx))

        MExp (Record fs) ctx ->
            if first then
                case fs of
                    [] ->
                        Nothing

                    ( l, e ) :: rest_fs ->
                        Just (MLbl l (Record1 [] () e rest_fs ctx))

            else
                case List.reverse fs of
                    [] ->
                        Nothing

                    ( l, e ) :: rest_fs ->
                        Just (MExp e (Record2 rest_fs l () [] ctx))


right_up : ExprFocus -> ExprFocus
right_up m =
    case nav_right m of
        Just sibling_m ->
            sibling_m

        Nothing ->
            case nav_up m of
                Just parent_m ->
                    right_up parent_m

                Nothing ->
                    m


next_hole : ExprFocus -> ExprFocus
next_hole m =
    case nav_first_child m of
        Just child_m ->
            child_m

        Nothing ->
            right_up m


down_right : ExprFocus -> ExprFocus
down_right m =
    case nav_last_child m of
        Just child_m ->
            down_right child_m

        Nothing ->
            m


prev_hole : ExprFocus -> ExprFocus
prev_hole m =
    case nav_left m of
        Just sibling_m ->
            down_right sibling_m

        Nothing ->
            case nav_up m of
                Just parent_m ->
                    parent_m

                Nothing ->
                    down_right m


up_hole : ExprFocus -> ExprFocus
up_hole m =
    case nav_up m of
        Just parent_m ->
            parent_m

        Nothing ->
            m


plugin : ExprFocus -> Expr
plugin m =
    case nav_up m of
        Just parent_m ->
            plugin parent_m

        Nothing ->
            case m of
                MExp e EmptyContext ->
                    e

                _ ->
                    -- impossible
                    Num 42
