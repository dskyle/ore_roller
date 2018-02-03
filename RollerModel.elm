module RollerModel exposing (..)

import Navigation
import Http
import List.Extra exposing ((!!))
import Maybe exposing (andThen, withDefault)

type alias Model = {
    creatures : List Creature
}

type alias Creature = {
    name : String,
    desc : String,
    d10s : Int,
    dSs : Int,
    activePool : List Set,
    skillPool : List Int,
    spentPool : List Int,
    damagePool : List Int,
    damageTokens : Int,
    state : CreatureState
}

type CreatureState
    = Normal
    | DamagePending Int (List Int) (List Int)
    | DamageDealt (List Set)
    | SpendingSkill Int

defaultCreature : Creature
defaultCreature = Creature "" "" 10 0 [] [] [] [] 0 Normal

type alias Set = {
    width : Int,
    height : Int
}

save_die : Int -> String
save_die die =
    if die >= 1 && die < 10 then
        toString die
    else if die == 10 then
        "0"
    else
        ""

save_dice : List Int -> String
save_dice dice =
    String.join "" <| List.map save_die dice

save_set : Set -> String
save_set set =
    String.repeat set.width (save_die set.height)

save_sets : List Set -> String
save_sets sets =
    String.join "" <| List.map save_set sets

save_cstate : CreatureState -> String
save_cstate c =
    case c of
        Normal -> "N"
        DamagePending amt dice rolled ->
            String.join "-" [
                "P",
                toString amt,
                save_dice dice,
                save_dice rolled
            ]
        DamageDealt dmg -> "D-" ++ save_sets dmg
        SpendingSkill dId -> "S-" ++ toString dId

save_state : List Creature -> String
save_state creatures =
    String.join "/" <| List.map (\c ->
            String.join "," [
                Http.encodeUri c.name,
                Http.encodeUri c.desc,
                toString c.d10s,
                toString c.dSs,
                save_sets c.activePool,
                save_dice c.skillPool,
                save_dice c.spentPool,
                save_dice c.damagePool,
                toString c.damageTokens,
                save_cstate c.state
            ]
        ) creatures

get_or : Int -> a -> List a -> a
get_or idx def list =
    Maybe.withDefault def (List.Extra.getAt idx list)

load_die : String -> Int
load_die s =
    case String.toInt s of
        Ok i -> if i == 0 then 10 else i
        Err e -> 0

load_dice : String -> List Int
load_dice s =
    String.split "" s |> List.map load_die

load_cstate : String -> CreatureState
load_cstate s =
    let a = String.split "-" s in
    case List.Extra.getAt 0 a of
        Just "P" ->
            let amt = List.Extra.getAt 1 a |> andThen (String.toInt >> Result.toMaybe) |> withDefault 3 
                dice = List.Extra.getAt 2 a |> andThen (load_dice >> Just) |> withDefault []
                rolled = List.Extra.getAt 3 a |> andThen (load_dice >> Just) |> withDefault [] in
            DamagePending amt dice rolled
        Just "D" ->
            let sets = List.Extra.getAt 1 a |> andThen (load_dice >> gen_sets >> Just) |> withDefault [] in
            DamageDealt sets
        Just "S" ->
            let die = List.Extra.getAt 1 a |> andThen (String.toInt >> Result.toMaybe) |> withDefault 0  in
            SpendingSkill die
        _ -> Normal

load_creature : String -> Creature
load_creature sc =
    let lc = String.split "," sc in
    let get x = get_or x "" lc
        def0 = Result.withDefault 0
        def10 = Result.withDefault 10
        defE = Maybe.withDefault "" in
    Creature
        (defE (Http.decodeUri (get 0)))
        (defE (Http.decodeUri (get 1)))
        (def10 (String.toInt (get 2)))
        (def0 (String.toInt (get 3)))
        (gen_sets (load_dice (get 4)))
        (load_dice (get 5))
        (load_dice (get 6))
        (load_dice (get 7))
        (def0 (String.toInt (get 8)))
        (load_cstate (get 9))

load_state : String -> List Creature
load_state s =
    if String.isEmpty s then
        []
    else
        String.split "/" s |> List.map load_creature

set_compare : Set -> Set -> Order
set_compare l r =
    if l.width > r.width then GT else
    if l.width < r.width then LT else
    if l.height > r.height then GT else
    if l.height < r.height then LT else
        EQ

desc : (a -> b -> Order) -> a -> b -> Order
desc cmp l r =
    case cmp l r of
        GT -> LT
        LT -> GT
        EQ -> EQ

gen_sets : List Int -> List Set
gen_sets list =
    List.sort list |>
    List.Extra.group |>
    List.filterMap (\x -> List.head x |>
        Maybe.andThen (\y -> Just (Set (List.length x) y))) |>
    List.sortWith (desc set_compare)

set_add_die : Int -> List Set -> List Set
set_add_die die list =
    let idx = List.Extra.findIndex (\x -> x.height == die) list in
    (case idx of
        Just idx -> List.Extra.updateAt idx (\x -> {x | width = x.width + 1}) list
        Nothing -> List.append list [Set 1 die])
    |> List.sortWith (desc set_compare)

set_split : Maybe Set -> List Int
set_split set = 
    case set of
        Just set -> List.repeat set.width set.height
        Nothing -> []

type Msg
    = NullMsg
    | NullStrMsg String
    | ClearAll
    | Roll
    | Results (List (List Int, List Int))
    | NameChange Int String
    | DescChange Int String
    | EntryDone Int
    | EntryStarted Int
    | RemoveEntry Int
    | SpendSet Int Int
    | UnspendDie Int Int
    | SpendSkill Int Int
    | AddDamage Int Int
    | DamageResult Int (List Int) (List Int)
    | CloseDamage Int
    | ChangeD10s Int Int
    | ChangeDSs Int Int
    | UrlChanged Navigation.Location