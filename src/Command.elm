module Command exposing (..)

{-| An alternative to Path that allows easier mixing of moves and draws
-}

import Path exposing (..)


type Command
    = MoveToCommand MoveTo
    | DrawToCommand DrawTo


moveTo =
    MoveToCommand << MoveTo Absolute


lineTo =
    DrawToCommand << LineTo Absolute


toCommands : Path -> List Command
toCommands =
    List.concatMap subpathToCommands


subpathToCommands : SubPath -> List Command
subpathToCommands { moveto, drawtos } =
    MoveToCommand moveto :: List.map DrawToCommand drawtos


{-| Fails when the first command is not a move instruction
-}
fromCommands : List Command -> Maybe Path
fromCommands commands =
    case commands of
        [] ->
            Just []

        (MoveToCommand moveto) :: cs ->
            fromCommandsSafe moveto cs
                |> Just

        (DrawToCommand _) :: _ ->
            Nothing


fromCommandsSafe : MoveTo -> List Command -> Path
fromCommandsSafe moveto commands =
    List.foldr fromCommandsHelper ( { moveto = moveto, drawtos = [] }, [] ) commands
        |> uncurry (::)


fromCommandsHelper : Command -> ( SubPath, List SubPath ) -> ( SubPath, List SubPath )
fromCommandsHelper command ( current, accum ) =
    case command of
        DrawToCommand drawto ->
            ( { current | drawtos = drawto :: current.drawtos }, accum )

        MoveToCommand moveto ->
            ( { moveto = moveto, drawtos = [] }, current :: accum )
