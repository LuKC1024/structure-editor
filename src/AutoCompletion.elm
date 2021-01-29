module AutoCompletion exposing (Filler)
import Expr exposing (Expr)
type alias Filler =
    String -> List Expr
