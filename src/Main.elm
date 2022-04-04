module Main exposing (..)

import Browser
import Element exposing (Element, alignRight, alignTop, centerY, column, el, fill, height, none, padding, paragraph, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Parser exposing ((|.), Parser)
import Set


type alias Model =
    { text : String }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextUpdated string ->
            ( { model | text = string }, Cmd.none )

        ReplaceBeforeClicked index replacement ->
            ( { model
                | text =
                    model.text
                        |> toStructuredText
                        |> replaceBefore index replacement
                        |> toString
              }
            , Cmd.none
            )


type Msg
    = TextUpdated String
    | ReplaceBeforeClicked Int Space


init : () -> ( Model, Cmd Msg )
init () =
    ( { text = lazyGuide }, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout [ width fill, height fill, padding 5 ]
        (row [ width fill, spacing 20 ]
            [ Input.multiline [ width fill, height fill ]
                { onChange = TextUpdated
                , placeholder = Nothing
                , text = model.text
                , label =
                    Input.labelAbove []
                        -- il faut un nbsp; ici :)
                        (text "Le texte que vous voulez formater :")
                , spellcheck = True
                }
            , column [ width fill, alignTop ]
                [ paragraph []
                    [ text "Les occurrences des symboles nécessitant un espace insécable avant."
                    , text " Chaque click sur un bouton radio modifie la zone de texte à gauche,"
                    , text " vous n'avez donc plus qu'à le copier !"
                    ]
                , viewStructuredTexts (toStructuredText model.text)
                ]
            ]
        )


toStructuredText : String -> List StructuredText
toStructuredText string =
    Parser.run structuredTextParser string
        |> Result.withDefault [ Text ("PARSING ERROR, THAT'S UNFORTUNATE!!! " ++ string) ]


toString : List StructuredText -> String
toString structuredTexts =
    structuredTexts
        |> List.map
            (\s ->
                case s of
                    Symbol symbol ->
                        String.fromChar (symbolToChar symbol)

                    Text string ->
                        string
            )
        |> String.concat


replaceBefore : Int -> Space -> List StructuredText -> List StructuredText
replaceBefore index space structuredTexts =
    structuredTexts
        |> List.indexedMap
            (\currentIndex structuredText ->
                if index - 1 == currentIndex then
                    case structuredText of
                        Text string ->
                            Text (String.trimRight string ++ spaceToString space)

                        Symbol _ ->
                            structuredText

                else
                    structuredText
            )


viewStructuredTexts : List StructuredText -> Element Msg
viewStructuredTexts structuredTexts =
    let
        loop i previous nexts acc =
            case nexts of
                [] ->
                    acc

                next :: others ->
                    loop (i + 1)
                        (Just next)
                        others
                        (viewStructuredText i
                            previous
                            next
                            (List.head others)
                            :: acc
                        )
    in
    loop 0 Nothing structuredTexts []
        |> List.reverse
        |> column [ spacing 10, width fill ]


viewStructuredText : Int -> Maybe StructuredText -> StructuredText -> Maybe StructuredText -> Element Msg
viewStructuredText index previous current after =
    case current of
        Symbol symbol ->
            column [ Border.width 1, Border.color (rgb255 0 0 0), width fill, spacing 5 ]
                [ row []
                    [ el [ alignTop ] <|
                        case previous of
                            Nothing ->
                                el [ Font.color (rgb255 50 50 50) ] (text "<rien>")

                            Just (Symbol s) ->
                                text (String.fromChar (symbolToChar s))

                            Just (Text string) ->
                                text (String.right 10 string)
                    , el [ Font.bold, Font.color (rgb255 255 0 0), alignTop ] (text (String.fromChar (symbolToChar symbol)))
                    , el [ alignTop ] <|
                        case after of
                            Nothing ->
                                el [ Font.color (rgb255 50 50 50) ] (text "<rien>")

                            Just (Symbol s) ->
                                text (String.fromChar (symbolToChar s))

                            Just (Text string) ->
                                text (String.left 10 string)
                    ]
                , case previous of
                    Nothing ->
                        el [ Font.color (rgb255 50 50 50) ] (text "pas d'action possible")

                    Just (Symbol s) ->
                        el [ Font.color (rgb255 50 50 50) ] (text "pas d'action possible")

                    Just (Text string) ->
                        let
                            lastSpace =
                                String.right 1 string
                                    |> stringToSpace
                        in
                        el [ Font.color (rgb255 50 50 50), Font.size 12 ] <|
                            Input.radioRow [ spacing 10 ]
                                { onChange = ReplaceBeforeClicked index
                                , label = Input.labelAbove [] (text "Espace avant :")
                                , options =
                                    [ Input.option NoSpace (text "sans espace")
                                    , Input.option Normal (text "normal")
                                    , Input.option NonBreaking (text "insécable")
                                    ]
                                , selected = Just lastSpace
                                }
                ]

        Text _ ->
            none


stringToSpace : String -> Space
stringToSpace string =
    if string == " " then
        Normal

    else if string == nonBreakingSpace then
        NonBreaking

    else
        NoSpace


spaceToString : Space -> String
spaceToString space =
    case space of
        NoSpace ->
            ""

        Normal ->
            " "

        NonBreaking ->
            nonBreakingSpace


type Space
    = NoSpace
    | Normal
    | NonBreaking


nonBreakingSpace : String
nonBreakingSpace =
    "\u{00A0}"


type StructuredText
    = Symbol Symbol
    | Text String


type Symbol
    = QuestionMark
    | ExclamationMark
    | SemiColon
    | Colon


symbolToChar : Symbol -> Char
symbolToChar symbol =
    case symbol of
        QuestionMark ->
            '?'

        Colon ->
            ':'

        ExclamationMark ->
            '!'

        SemiColon ->
            ';'


allSymbols : List Symbol
allSymbols =
    [ QuestionMark, Colon, ExclamationMark, SemiColon ]


structuredTextParser : Parser (List StructuredText)
structuredTextParser =
    Parser.loop [] parserLoopHelp


parserLoopHelp : List StructuredText -> Parser (Parser.Step (List StructuredText) (List StructuredText))
parserLoopHelp revSturcturedTexts =
    Parser.oneOf
        [ symbolParser
            |> Parser.map
                (\symbol ->
                    Parser.Loop (Symbol symbol :: revSturcturedTexts)
                )
        , Parser.chompWhile isNotSymbol
            |> Parser.getChompedString
            |> Parser.map
                (\string ->
                    if String.isEmpty string then
                        Parser.Done (List.reverse revSturcturedTexts)

                    else
                        Parser.Loop (Text string :: revSturcturedTexts)
                )
        ]


symbolParser : Parser Symbol
symbolParser =
    Parser.oneOf
        (allSymbols
            |> List.map
                (\symbol ->
                    Parser.succeed symbol
                        |. Parser.symbol (String.fromChar (symbolToChar symbol))
                )
        )


allSymbolsString : Set.Set Char
allSymbolsString =
    Set.fromList (List.map symbolToChar allSymbols)


isNotSymbol : Char -> Bool
isNotSymbol char =
    not (Set.member char allSymbolsString)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


lazyGuide =
    """# `Html.Lazy`
             
Le paquet [`elm/html`](https://package.elm-lang.org/packages/elm/html/latest/) est utilisé pour afficher des choses à l'écran. Pour comprendre comment l'optimiser, voyons d'abord comment il fonctionne !


## Qu'est-ce que le DOM ?

Lorsque vous créez un fichier HTML, vous écrivez du HTML directement comme ça :

```html
<div>
<p>Parmi les alternatives aux chaises, on trouve :</p>
<ul>
 <li>seiza</li>
 <li>chabudai</li>
</ul>
</div>
```

On peut y penser comme produisant cette structure DOM en coulisses:

![](diagrams/dom.svg)

Les boîtes noires représentent des objets DOM lourds avec des centaines d'attributs. Et un changement de n'importe lequel d'entre eux peut déclencher des calculs très coûteux pour redessiner et réarranger le contenu de la page.


## Qu'est-ce que le _Virtual DOM_ ?

Si vous créez un fichier Elm, vous utiliserez plutôt `elm/html` pour avoir ce genre de code :

```elm
viewChairAlts : List String -> Html msg
viewChairAlts chairAlts =
div []
 [ p [] [ text "Parmi les alternatives aux chaises, on trouve :" ]
 , ul [] (List.map viewAlt chairAlts)
 ]

viewAlt : String -> Html msg
viewAlt chairAlt =
li [] [ text chairAlt ]
```


On peut voir `viewChairAlts ["seiza","chabudai"]` comme produisant en coulisse la structure de “DOM Virtuel“ suivante :

![](diagrams/vdom.svg)

Les boîtes blanches représentent des objets JavaScript légers. Ils contiennent uniquement les attributs que vous spécifiez. Leur création n'engendredra jamais de calculs pour redessiner et réarranger la page. En bref, comparés aux nœuds DOM, ils sont beaucoup moins chers à allouer !

## _Render_

Si nous travaillons toujours avec ces nœuds virtuels en Elm, comment sont-ils convertis vers le DOM affiché à l'écran ? Au démarrage d'un programme Elm, il se passe les choses suivantes:

- appel à `init` pour avoir le `Model` initial,
- appel à `view` pour avoir les nœuds virtuels initiaux.

Maintenant que nous avons les nœuds virtuels, on en produit une réplique exacte dans le DOM réel:

![](diagrams/render.svg)

Super ! Mais que se passe-t-il lorsqu'il y a du changement ? Re-construire le DOM en entier à chaque _frame_ ne fonctionne pas, que pouvons nous donc faire à la place ?


## _Diffing_

Une fois qu'on a le DOM initial, on travaillera principalement sur les nœuds virtuels. À chaque fois que le `Model` change, on exécute `view` à nouveau. À partir de là, on calcule le _“diff”_ sur les nœuds virtuels obtenus afin de trouver comment toucher au DOM le moins possible.

Supposons par exemple qu'une nouvelle alternative aux chaises soit listée dans notre `Model` ; nous voulons alors ajouter un nouveau nœud `li` pour elle. En coulisses, Elm calcule le _diff_ entre les nœuds virtuels **actuels** et les **suivants** pour détecter le moindre changement :

![](diagrams/diff.svg)

Elm a remarqué qu'un troisième `li` a été ajouté. Je l'ai marqué en vert. Elm sait maintenant exactement comment modifier le DOM réel pour le faire correspondre. Il faut juste insérer ce nouveau `li` :

![](diagrams/patch.svg)

Ce processus de _diff_ permet de modifier le DOM le moins possible. Et si aucune différence n'est détectée, on ne touche pas du tout au DOM ! Donc ce processus minimise le besoin de redessiner et réarranger le contenu de la page.

Mais peut-on faire encore moins de calculs?


## `Html.Lazy`

Le module [`Html.Lazy`](https://package.elm-lang.org/packages/elm/html/latest/Html-Lazy/) permet de ne même pas construire les nœuds virtuels ! L'idée principale est la fonction `lazy` (NdT: _"lazy"_ signifie "paresseux" en anglais) :

```elm
lazy : (a -> Html msg) -> a -> Html msg
```

Revenons sur notre exemple de chaises, on avait appelé `viewChairAlts ["seiza","chabudai"]`, mais nous aurions pu appeler `lazy viewChairAlts ["seiza","chabudai"]` à la place. La version paresseuse alloue alors un seul nœud “paresseux“ :

![](diagrams/lazy.svg)

Le nœud garde juste une référence vers la fonction et les arguments. Elm peut recombiner la fonction et les arguments pour générer toute la structure si nécessaire, mais ce n'est pas toujours nécessaire !

Un des trucs cool avec Elm est cette garantie des fonctions: “même entrée, même sortie“. Donc à chaque fois qu'on rencontre deux nœuds “paresseux“ dans le calcul du _diff_ des nœuds virtuels, on se demande: la fonction est-elle la même ? les arguments sont-ils les mêmes ? S'ils sont tous identiques, on sait que les nœuds virtuels résultants seront également les mêmes ! **Donc on peut entièrement économiser la construction des nœuds virtuels !** Si l'un d'eux a changé, on peut construire le nœud virtuel et faire un _diff_ normal.

> **Note:** à quelle condition deux valeurs sont-elles “les mêmes“? Pour optimiser les performances, on utilise l'opérateur `===` de JavaScript en coulisses :
>
> - L'égalité structurelle est utilisée pour `Int`, `Float`, `String`, `Char` et `Bool`.
> - L'égalité référentielle est utilisée pour les _records_, listes, _custom types_, dictionnaires, etc.
>
> L'égalité structurelle signifie que `4` est la même chose que `4` peu importe comment vous avez produit ces valeurs. L'égalité référentielle signifie que le pointeur en mémoire doit être le même. Utiliser l'égalité référentielle est toujours peu coûteux (en  `O(1)`), même si la structure de données contient des milliers ou millions d'éléments. Tout cela est fait pour s'assurer qu'utiliser `lazy` ne va jamais ralentir énormément votre code par accident. Toutes les vérifications sont vraiment peu coûteuses !


## Usage

Le lieu idéal pour utiliser un nœud paresseux est à la racine de votre application. De nombreuses applications ont plusieurs régions visuelles distinctes comme des _headers_, _sidebars_, résultats de recherche, etc. Et quand on change une région, on change rarement les autres. Cela appelle naturellement l'utilisation de `lazy` !

Par exemple, dans [mon implémentation de TodoMVC](https://github.com/evancz/elm-todomvc/), la fonction `view` est définie par:

```elm
view : Model -> Html Msg
view model =
div
 [ class "todomvc-wrapper"
 , style "visibility" "hidden"
 ]
 [ section
     [ class "todoapp" ]
     [ lazy viewInput model.field
     , lazy2 viewEntries model.visibility model.entries
     , lazy2 viewControls model.visibility model.entries
     ]
 , infoFooter
 ]
```

Remarquez que le champ de texte, les entrées et les contrôles sont dans des nœuds paresseux séparés. Donc je peux taper autant de lettres que je veux dans mon champ de texte sans jamais construire le nœud virtuel pour les entrées ou les contrôles. Ils ne changent pas ! Donc le premier conseil est **essayez d'utiliser des nœuds paresseux à la racine de votre application**.

Il peut également être intéressant d'utiliser `lazy` dans de longues listes d'éléments. Dans l'application TodoMVC, il est seulement question d'ajouter des entrées dans notre _todo list_. On peut raisonnablement envisager des centaines d'entrées, mais elles changent très rarement. Il s'agit donc d'un bon candidat pour `lazy` ! En transformant `viewEntry entry` en `lazy viewEntry entry` on peut éviter un paquet d'allocations qui sont rarement utiles. Donc le second conseil est **essayez d'utiliser des nœuds paresseux sur les structures répétitives où chaque élément ne change que rarement**.


## Résumé

Modifier le DOM est bien plus coûteux que n'importe quelle autre opération dans une interface graphique normale. D'après mes tests de performance, vous pouvez faire ce que vous voulez avec des structures de données complexes, mais à la fin, la seule chose importante est votre degré de succès dans l'utilisation de `lazy`.

Dans la prochaine page, nous verrons une technique pour utiliser `lazy` encore plus !
"""
