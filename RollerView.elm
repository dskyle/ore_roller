module RollerView exposing (..)

import Html
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input as Input
import Element.Events as Events
import List.Extra exposing ((!!))

import RollerModel exposing (..)
import RollerStyle exposing (..)

set_toString : Set -> String
set_toString set =
    if set.width > 1 then
        (toString set.width) ++ "×" ++ (toString set.height)
    else
        (toString set.height)

dS_toString : Int -> String
dS_toString die =
    case die of
        1 -> "−"
        2 -> "☼"
        3 -> "+"
        _ -> "X"

renderPool : Int -> Int -> Creature -> Element MyStyles MyVariations Msg
renderPool cid max_width c =
    column ActivePoolStyle [padding 5, spacing 5] [
        row NullStyle [spacing 5] <| List.append [
            el LabelStyle [] (text "Active"),
            el LabelStyle [width fill] (text "")
        ] <| let (all, one) = case c.state of
            Normal -> (True, -1)
            SpendingSkill die -> (True, die)
            _ -> (False, -1) in
                List.indexedMap (\i d ->
                    let enable = (all || one == i || d == 2)
                        important = (one == i) in
                    button ButtonStyle [
                        width (px 17),
                        vary Important important,
                        (if enable then
                            Events.onClick (SpendSkill cid i)
                        else
                            vary Ghosted True)
                    ] (text (dS_toString d))) c.skillPool,
        row NullStyle [width fill, spacing 5] [
            wrappedRow NullStyle [width fill, spacing 5] (
                let ghost = case c.state of
                    SpendingSkill die -> case c.skillPool !! die of
                        Just 1 -> 1
                        Just 3 -> 10
                        _ -> -1
                    _ -> -1 in
                    List.indexedMap (\sid x ->
                        button SetStyle [
                                padding 5,
                                vary MaxWidth (max_width >= 2 && x.width == max_width),
                                Events.onClick (SpendSet cid sid),
                                vary Ghosted (x.height == ghost)
                            ] (text <| set_toString x))
                        c.activePool),
            case c.state of
                Normal -> button ButtonStyle [
                            Events.onClick (RollOne cid),
                            alignBottom,
                            width (px 30),
                            height (px 30)] (text "🎲")
                _ -> empty
        ]
    ]

renderSpent : Int -> Creature -> Element MyStyles MyVariations Msg
renderSpent cid c = 
    column SpentPoolStyle [padding 5] [
        el LabelStyle [] (text "Spent"),
        wrappedRow NullStyle [spacing 5, padding 5] (
            List.indexedMap (\did x ->
                button DieStyle [
                        padding 5,
                        Events.onClick (UnspendDie cid did)
                    ] (text <|
                    (toString x)))
                c.spentPool)
    ]

renderDamage : Int -> Creature -> Element MyStyles MyVariations Msg
renderDamage cid c =
    let dmgCount = c.damageTokens + (List.length c.damagePool) in
    column DamagePoolStyle [padding 5, spacing 5] [
        el LabelStyle [] (text <| "Damage (" ++ (toString dmgCount ++ ")")),
        let rcount = case c.state of
            Normal -> 4
            _ -> 2 in
        grid NullStyle [spacing 5] {
            columns = List.repeat 6 (fill),
            rows = List.repeat 2 (px 20),
            cells = List.range 0 ((6 * rcount) - 1) |> List.map (\x ->
                cell {
                    start = (x % 6, x // 6),
                    width = 1, height = 1,
                    content = if x < 12 then
                        row DamageCellStyle [] [
                            el NullStyle [width fill] (text ""),
                            if c.damageTokens >= x + 1 then
                                el NullStyle [] (text "⬤")
                            else
                                case c.damagePool !! (x - c.damageTokens) of
                                    Just i -> el NullStyle [] (text (toString i))
                                    Nothing -> el NullStyle [] (text ""),
                            el NullStyle [width fill] (text "")
                        ]
                    else if x < 22 then
                        let amount = x - 11 in
                        button ButtonStyle [Events.onClick (RollDamage cid amount)]
                            (text (toString amount))
                    else if x == 22 then
                        button ButtonStyle [
                            if dmgCount > 0 then
                                Events.onClick (ModifyDamage cid -1)
                            else
                                vary Ghosted True
                            ] (text "-1")
                    else
                        button ButtonStyle [
                            if dmgCount < 6 || (List.length c.spentPool) > 0 then
                                Events.onClick (ModifyDamage cid 1)
                            else
                                vary Ghosted True
                        ] (text "+1")
                })
        },
        case c.state of
            DamagePending amt dice rolled ->
                let dlen = List.length dice
                    rlen = List.length rolled
                    tlen = dlen + rlen
                in
                wrappedRow DamageDealtStyle [padding 5, spacing 5] <| List.concat
                    [List.map (\d ->
                        el DamageCellStyle [paddingXY 5 3, vary Ghosted True] (text (toString d))) rolled,
                    List.map (\d ->
                        el DamageCellStyle [paddingXY 5 3] (text (toString d))) dice,
                    List.repeat (amt - tlen) (
                        el DamageCellStyle [paddingXY 5 3, vary Important True] (text "_"))]
            DamageDealt sets ->
                row DamageDealtStyle [padding 5, spacing 5] <| [
                    wrappedRow NullStyle [spacing 5] <|
                        List.map (\s ->
                            el DamageCellStyle [
                                paddingXY 5 3,
                                vary Important (s.height > 1 && s.width > 1),
                                vary Ghosted (s.height == 1)] (text (set_toString s))) sets,
                    el NullStyle [width fill] (text ""),
                        button CloseButtonStyle [
                            paddingXY 5 0,
                            Events.onClick (CloseDamage cid)] (text "×")]
            _ -> empty
    ]

renderCreature : Length -> Bool -> Int -> Int -> Creature -> Element MyStyles MyVariations Msg
renderCreature wide real max_width idx creature =
    let 
        mk_placeholder s =
            Input.placeholder {
                text = s,
                label = Input.hiddenLabel s
            }
    in
    column CreatureStyle
        [width wide, spacing 5, padding 5,
            vary Ghosted (not real),
            if not real then Events.onClick (EntryStarted idx) else padding 5] [
        row NullStyle [spacing 3] [
            Input.text CreatureNameStyle [
                    Events.onFocus (EntryStarted idx),
                    Events.onBlur (EntryDone idx),
                    padding 2,
                    width fill
                ] {
                value = creature.name,
                onChange = NameChange idx,
                label = mk_placeholder "Name",
                options = []},
            if real then
                button CloseButtonStyle [
                    paddingXY 5 0,
                    Events.onClick (RemoveEntry idx)] (text "×")
            else empty
        ],
        row NullStyle [spacing 3] [
            Input.multiline NullStyle [
                    Events.onFocus (EntryStarted idx),
                    Events.onBlur (EntryDone idx),
                    height (px 50)
                ] {
                value = creature.desc,
                onChange = DescChange idx,
                label = mk_placeholder "Description",
                options = []},
            let mk_spinner name cmd =
                row NullStyle [spacing 1, height fill, center] [
                    button ButtonStyle [
                        verticalCenter,
                        width (px 15),
                        Events.onClick (cmd idx -1)] (text "−"),
                    el NullStyle [width fill] (text ""),
                    el LabelStyle [verticalCenter] (text name),
                    el NullStyle [width fill] (text ""),
                    button ButtonStyle [
                        verticalCenter,
                        width (px 15),
                        Events.onClick (cmd idx 1)] (text "+")
                ] in
            column NullStyle [spacing 3] [
                mk_spinner ((toString creature.d10s) ++ "d10") ChangeD10s,
                mk_spinner ((toString creature.dSs) ++ "dS") ChangeDSs
            ]
        ],
        renderPool idx max_width creature,
        renderSpent idx creature,
        renderDamage idx creature
    ] |>
        within [if not real then el AddCreatureStyle [
            center, verticalCenter,
            height (px 100), width (px 100),
            inlineStyle [("text-align", "center"), ("vertical-align", "center")]
        ] (text "+") else empty]

get_max_width : List Creature -> Int
get_max_width creatures =
    creatures |> List.foldl (\x acc ->
        max acc (x.activePool |> List.foldl (\x acc ->
            max acc x.width) 0)) 0

renderCreatures : Length -> List Creature -> Element MyStyles MyVariations Msg
renderCreatures wide creatures =
    let max_width = get_max_width creatures in
    wrappedRow NullStyle [spacing 10, padding 3] (
        List.append (List.indexedMap (renderCreature wide True max_width) creatures)
            [renderCreature wide False max_width (List.length creatures) <|
                defaultCreature])

view : Model -> Html.Html Msg
view model =
    layout stylesheet <|
        column NullStyle [] [
            screen (
                row TopBarStyle [width fill, padding 5, spacing 5] [
                    button ButtonStyle [ paddingXY 20 3, Events.onClick Roll ] (text "Roll All"),
                    el NullStyle [width (fill)] (text ""),
                    button ButtonStyle [ paddingXY 20 3, Events.onClick ClearAll ] (text "Remove All")
                ]),
            el NullStyle [height (px 50)] (text ""),
            renderCreatures (px 280) model.creatures
        ]