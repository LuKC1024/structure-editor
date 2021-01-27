module Expr exposing (..)


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
    | Match Expr String Expr String Expr
    | Inl Expr
    | Inr Expr
    | Label String Expr


expr_bop : Op2 -> Expr
expr_bop o =
    Bop o (Todo "") (Todo "")


expr_let : Expr
expr_let =
    Let "x" (Todo "") (Todo "")


expr_fun : Expr
expr_fun =
    Fun "x" (Todo "")


expr_app : Expr
expr_app =
    App (Todo "") (Todo "")


expr_match : Expr
expr_match =
    Match (Todo "") "" (Todo "") "" (Todo "")


expr_inl : Expr
expr_inl =
    Inl (Todo "")


expr_inr : Expr
expr_inr =
    Inr (Todo "")


expr_label : Expr
expr_label =
    Label "" (Todo "")


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
    | Match1 () String Expr String Expr ExprContext
    | Match3 Expr String () String Expr ExprContext
    | Match5 Expr String Expr String () ExprContext
    | Label2 String () ExprContext


type VarContext
    = Let1 () Expr Expr ExprContext
    | Fun1 () Expr ExprContext
    | Match2 Expr () Expr String Expr ExprContext
    | Match4 Expr String Expr () Expr ExprContext


type LblContext
    = Label1 () Expr ExprContext


type Model
    = MExp Expr ExprContext
    | MVar String VarContext
    | MLbl String LblContext


nav_up : Model -> Maybe Model
nav_up m =
    case m of
        MExp _ EmptyContext ->
            Nothing

        MExp e1 (Bop1 o () e2 ctx) ->
            Just (MExp (Bop o e1 e2) ctx)

        MExp e2 (Bop2 o e1 () ctx) ->
            Just (MExp (Bop o e1 e2) ctx)

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

        MLbl x (Label1 () e ctx) ->
            Just (MExp (Label x e) ctx)

        MExp e (Label2 x () ctx) ->
            Just (MExp (Label x e) ctx)

        MExp e (Inl1 () ctx) ->
            Just (MExp (Inl e) ctx)

        MExp e (Inr1 () ctx) ->
            Just (MExp (Inr e) ctx)

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


nav_right : Model -> Maybe Model
nav_right =
    nav_horizontal True


nav_left : Model -> Maybe Model
nav_left =
    nav_horizontal False


nav_horizontal : Bool -> Model -> Maybe Model
nav_horizontal right m =
    case m of
        MExp _ EmptyContext ->
            Nothing

        MExp e1 (Bop1 o () e2 ctx) ->
            if right then
                Just (MExp e2 (Bop2 o e1 () ctx))

            else
                Nothing

        MExp e2 (Bop2 o e1 () ctx) ->
            if right then
                Nothing

            else
                Just (MExp e1 (Bop1 o () e2 ctx))

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

        MLbl x (Label1 () e ctx) ->
            if right then
                Just (MExp e (Label2 x () ctx))

            else
                Nothing

        MExp e (Label2 x () ctx) ->
            if right then
                Nothing

            else
                Just (MLbl x (Label1 () e ctx))

        MExp _ (Inl1 () _) ->
            Nothing

        MExp _ (Inr1 () _) ->
            Nothing

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


nav_first_child : Model -> Maybe Model
nav_first_child =
    nav_child True


nav_last_child : Model -> Maybe Model
nav_last_child =
    nav_child False


nav_child : Bool -> Model -> Maybe Model
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

        MExp (Bop o e1 e2) ctx ->
            if first then
                Just (MExp e1 (Bop1 o () e2 ctx))

            else
                Just (MExp e2 (Bop2 o e1 () ctx))

        MExp (App e1 e2) ctx ->
            if first then
                Just (MExp e1 (App1 () e2 ctx))

            else
                Just (MExp e2 (App2 e1 () ctx))

        MExp (Label x e) ctx ->
            if first then
                Just (MLbl x (Label1 () e ctx))

            else
                Just (MExp e (Label2 x () ctx))

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

        MExp (Inl e) ctx ->
            Just (MExp e (Inl1 () ctx))

        MExp (Inr e) ctx ->
            Just (MExp e (Inl1 () ctx))

        MExp (Match e xl el xr er) ctx ->
            if first then
                Just (MExp e (Match1 () xl el xr er ctx))

            else
                Just (MExp er (Match5 e xl el xr () ctx))


right_up : Model -> Model
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


next_hole : Model -> Model
next_hole m =
    case nav_first_child m of
        Just child_m ->
            child_m

        Nothing ->
            right_up m


down_right : Model -> Model
down_right m =
    case nav_last_child m of
        Just child_m ->
            down_right child_m

        Nothing ->
            m


prev_hole : Model -> Model
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


up_hole : Model -> Model
up_hole m =
    case nav_up m of
        Just parent_m ->
            parent_m

        Nothing ->
            m


plugin : Model -> Expr
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
