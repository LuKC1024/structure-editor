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
    Match (Todo "") "x" (Todo "") "x" (Todo "")


expr_inl : Expr
expr_inl =
    Inl (Todo "")


expr_inr : Expr
expr_inr =
    Inr (Todo "")


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


type VarContext
    = Let1 () Expr Expr ExprContext
    | Fun1 () Expr ExprContext
    | Match2 Expr () Expr String Expr ExprContext
    | Match4 Expr String Expr () Expr ExprContext


type Model
    = ExprModel Expr ExprContext
    | VarModel String VarContext


nav_up : Model -> Maybe Model
nav_up m =
    case m of
        ExprModel _ EmptyContext ->
            Nothing

        ExprModel e1 (Bop1 o () e2 ctx) ->
            Just (ExprModel (Bop o e1 e2) ctx)

        ExprModel e2 (Bop2 o e1 () ctx) ->
            Just (ExprModel (Bop o e1 e2) ctx)

        VarModel x (Let1 () e1 e2 ctx) ->
            Just (ExprModel (Let x e1 e2) ctx)

        ExprModel e1 (Let2 x () e2 ctx) ->
            Just (ExprModel (Let x e1 e2) ctx)

        ExprModel e2 (Let3 x e1 () ctx) ->
            Just (ExprModel (Let x e1 e2) ctx)

        VarModel x (Fun1 () e ctx) ->
            Just (ExprModel (Fun x e) ctx)

        ExprModel e (Fun2 x () ctx) ->
            Just (ExprModel (Fun x e) ctx)

        ExprModel e1 (App1 () e2 ctx) ->
            Just (ExprModel (App e1 e2) ctx)

        ExprModel e2 (App2 e1 () ctx) ->
            Just (ExprModel (App e1 e2) ctx)

        ExprModel e (Inl1 () ctx) ->
            Just (ExprModel (Inl e) ctx)

        ExprModel e (Inr1 () ctx) ->
            Just (ExprModel (Inr e) ctx)

        ExprModel e (Match1 () xl el xr er ctx) ->
            Just (ExprModel (Match e xl el xr er) ctx)

        VarModel xl (Match2 e () el xr er ctx) ->
            Just (ExprModel (Match e xl el xr er) ctx)

        ExprModel el (Match3 e xl () xr er ctx) ->
            Just (ExprModel (Match e xl el xr er) ctx)

        VarModel xr (Match4 e xl el () er ctx) ->
            Just (ExprModel (Match e xl el xr er) ctx)

        ExprModel er (Match5 e xl el xr () ctx) ->
            Just (ExprModel (Match e xl el xr er) ctx)


nav_right : Model -> Maybe Model
nav_right =
    nav_horizontal True


nav_left : Model -> Maybe Model
nav_left =
    nav_horizontal False


nav_horizontal : Bool -> Model -> Maybe Model
nav_horizontal right m =
    case m of
        ExprModel _ EmptyContext ->
            Nothing

        ExprModel e1 (Bop1 o () e2 ctx) ->
            if right then
                Just (ExprModel e2 (Bop2 o e1 () ctx))

            else
                Nothing

        ExprModel e2 (Bop2 o e1 () ctx) ->
            if right then
                Nothing

            else
                Just (ExprModel e1 (Bop1 o () e2 ctx))

        VarModel x (Let1 () e1 e2 ctx) ->
            if right then
                Just (ExprModel e1 (Let2 x () e2 ctx))

            else
                Nothing

        ExprModel e1 (Let2 x () e2 ctx) ->
            if right then
                Just (ExprModel e2 (Let3 x e1 () ctx))

            else
                Just (VarModel x (Let1 () e1 e2 ctx))

        ExprModel e2 (Let3 x e1 () ctx) ->
            if right then
                Nothing

            else
                Just (ExprModel e1 (Let2 x () e2 ctx))

        VarModel x (Fun1 () e ctx) ->
            if right then
                Just (ExprModel e (Fun2 x () ctx))

            else
                Nothing

        ExprModel e (Fun2 x () ctx) ->
            if right then
                Nothing

            else
                Just (VarModel x (Fun1 () e ctx))

        ExprModel e1 (App1 () e2 ctx) ->
            if right then
                Just (ExprModel e2 (App2 e1 () ctx))

            else
                Nothing

        ExprModel e2 (App2 e1 () ctx) ->
            if right then
                Nothing

            else
                Just (ExprModel e1 (App1 () e2 ctx))

        ExprModel _ (Inl1 () _) ->
            Nothing

        ExprModel _ (Inr1 () _) ->
            Nothing

        ExprModel e (Match1 () xl el xr er ctx) ->
            if right then
                Just (VarModel xl (Match2 e () el xr er ctx))

            else
                Nothing

        VarModel xl (Match2 e () el xr er ctx) ->
            if right then
                Just (ExprModel el (Match3 e xl () xr er ctx))

            else
                Just (ExprModel e (Match1 () xl el xr er ctx))

        ExprModel el (Match3 e xl () xr er ctx) ->
            if right then
                Just (VarModel xr (Match4 e xl el () er ctx))

            else
                Just (VarModel xl (Match2 e () el xr er ctx))

        VarModel xr (Match4 e xl el () er ctx) ->
            if right then
                Just (ExprModel er (Match5 e xl el xr () ctx))

            else
                Just (ExprModel e (Match3 e xl () xr er ctx))

        ExprModel er (Match5 e xl el xr () ctx) ->
            if right then
                Nothing

            else
                Just (VarModel xr (Match4 e xl el () er ctx))


nav_first_child : Model -> Maybe Model
nav_first_child =
    nav_child True


nav_last_child : Model -> Maybe Model
nav_last_child =
    nav_child False


nav_child : Bool -> Model -> Maybe Model
nav_child first m =
    case m of
        VarModel _ _ ->
            Nothing

        ExprModel (Todo _) _ ->
            Nothing

        ExprModel (Num _) _ ->
            Nothing

        ExprModel (Var _) _ ->
            Nothing

        ExprModel (Bop o e1 e2) ctx ->
            if first then
                Just (ExprModel e1 (Bop1 o () e2 ctx))

            else
                Just (ExprModel e2 (Bop2 o e1 () ctx))

        ExprModel (App e1 e2) ctx ->
            if first then
                Just (ExprModel e1 (App1 () e2 ctx))

            else
                Just (ExprModel e2 (App2 e1 () ctx))

        ExprModel (Let x e1 e2) ctx ->
            if first then
                Just (VarModel x (Let1 () e1 e2 ctx))

            else
                Just (ExprModel e2 (Let3 x e1 () ctx))

        ExprModel (Fun x e) ctx ->
            if first then
                Just (VarModel x (Fun1 () e ctx))

            else
                Just (ExprModel e (Fun2 x () ctx))

        ExprModel (Inl e) ctx ->
            Just (ExprModel e (Inl1 () ctx))

        ExprModel (Inr e) ctx ->
            Just (ExprModel e (Inl1 () ctx))

        ExprModel (Match e xl el xr er) ctx ->
            if first then
                Just (ExprModel e (Match1 () xl el xr er ctx))

            else
                Just (ExprModel er (Match5 e xl el xr () ctx))


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
