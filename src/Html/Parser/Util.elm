module Html.Parser.Util exposing (toVirtualDom)

{-| Utility functions that may help you digging into the contents.


# Virtual DOM

@docs toVirtualDom

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Parser exposing (Node(..))
import VirtualDom


{-| Converts nodes to virtual dom nodes.
-}
toVirtualDom : List Node -> List (Html msg)
toVirtualDom nodes =
    List.map toVirtualDomEach nodes


toVirtualDomEach : Node -> Html msg
toVirtualDomEach node =
    case node of
        Element name attrs children ->
            if name == "svg" then
                svgNode name (List.map toAttribute attrs) (List.map toVirtualSvgDomEach children)

            else
                Html.node name (List.map toAttribute attrs) (toVirtualDom children)

        Text s ->
            text s

        Comment _ ->
            text ""


toVirtualSvgDomEach : Node -> Html msg
toVirtualSvgDomEach node =
    case node of
        Element name attrs children ->
            svgNode name (List.map toAttribute attrs) (List.map toVirtualSvgDomEach children)

        Text s ->
            text s

        Comment _ ->
            text ""


toAttribute : ( String, String ) -> Attribute msg
toAttribute ( name, value ) =
    attribute name value


svgNode : String -> List (Attribute msg) -> List (Html msg) -> Html msg
svgNode =
    VirtualDom.nodeNS "http://www.w3.org/2000/svg"
