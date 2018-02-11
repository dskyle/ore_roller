import Random
import List.Extra exposing ((!!))
import Navigation
import Maybe exposing (andThen, withDefault)

import RollerModel exposing (..)
import RollerView exposing (..)

main : Program Never Model Msg
main =
    Navigation.program UrlChanged {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }

init : Navigation.Location -> (Model, Cmd Msg)
init loc = (let mod = Model (load_state (String.dropLeft 1 loc.hash)) in
            if List.isEmpty mod.creatures then
                Model [defaultCreature]
            else mod, Cmd.none)

save_cmd : Model -> Cmd Msg
save_cmd mod =
    Navigation.newUrl ("#" ++ (save_state mod.creatures))

update_cmd : Model -> Cmd Msg
update_cmd mod =
    Navigation.modifyUrl ("#" ++ (save_state mod.creatures))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let base_update cmd cid func =
        let mod = case model.creatures !! cid of
            Just c -> let cp = func c in
                {model | creatures = List.Extra.setAt cid cp model.creatures}
            Nothing -> let c = func defaultCreature in
                {model | creatures = List.append model.creatures [c]} in
        mod ! [cmd mod] in
    let simple_update = base_update save_cmd
        silent_update = base_update (always Cmd.none)
        roll = (Random.map2 (,) (
                Random.list 15 <|
                Random.int 1 10
            ) (
                Random.list 3 <|
                Random.int 1 3
            ))
        updateSets c l s =
            let dplen = List.length c.damagePool in
            {c | activePool = gen_sets (List.take (c.d10s - dplen) l),
                 skillPool = List.sort (List.take c.dSs s),
                 spentPool = [],
                 state = Normal}
    in
    case msg of
        NullMsg -> (model, Cmd.none)
        NullStrMsg s -> (model, Cmd.none)
        EntryStarted i ->
            if List.length model.creatures > i then
                (model, Cmd.none)
            else
                let mod = {model | creatures = List.append model.creatures [defaultCreature]} in
                (mod, save_cmd mod)
        RemoveEntry i ->
            let mod = {model | creatures = List.Extra.removeAt i model.creatures
                } in (mod, save_cmd mod)
        EntryDone i -> (model, save_cmd model)
        ClearAll -> let mod = {model | creatures = [defaultCreature]} in (mod, save_cmd mod)
        Roll ->
            (model, Random.generate Results <|
                Random.list (List.length model.creatures) roll)
        RollOne i ->
            (model, Random.generate (ResultsOne i) roll)
        
        Results list ->
            let l = List.Extra.zip model.creatures list in
            let mod = {model | creatures = List.map (\(c, (l, s)) ->
                updateSets c l s) l} in
            (mod, save_cmd mod)
        DamageResult cid prev dice ->
            let c = List.Extra.getAt cid model.creatures in
            case c of
                Nothing -> model ! []
                Just c ->
                    let dlen = List.length dice
                        plen = List.length prev
                        tlen = dlen + plen
                        num1s = List.Extra.count ((==) 1) dice
                        num10s = List.Extra.count ((==) 10) dice in
                    let (cp, cmd) = 
                        if num10s == 0 then
                            let pool = List.filter ((/=)1) (List.append dice prev) in
                            let pool_len = List.length pool in
                            let old_len = List.length c.damagePool in
                            let active_len = List.length c.activePool in
                            let total_len = pool_len + old_len in
                            let elig_tokens = clamp 0 6 (6 - c.damageTokens) in
                            let elig_dice = clamp 0 6 (6 - old_len) in
                            let new_tokens = clamp 0 elig_tokens pool_len in 
                            let new_dice = clamp 0 elig_dice (pool_len - elig_tokens) in
                            let elig_spent = clamp 0 c.d10s (c.d10s - active_len - old_len - new_dice) in
                            let (dmg_pool, spent_pool) = List.Extra.splitAt new_dice pool in
                            ({c | damagePool = List.append c.damagePool dmg_pool,
                                  damageTokens = c.damageTokens + new_tokens,
                                  state = DamageDealt (gen_sets <| List.append pool <| List.repeat num1s 1),
                                  spentPool = List.take elig_spent (List.concat [
                                        c.spentPool,
                                        (List.repeat num1s 1),
                                        spent_pool])}, save_cmd)
                        else
                            do_damage cid {c | state =
                                DamagePending (num10s + tlen)
                                    (List.repeat num10s 10)
                                    (List.append prev (List.filter ((/=) 10) dice))}
                    in
                    let mod = {model | creatures = List.Extra.setAt cid cp model.creatures} in
                    (mod, cmd mod)
        SpendSet cid sid ->
            let c = List.Extra.getAt cid model.creatures in
            case c of
                Nothing -> model ! []
                Just c ->
                    let (cp, cmd) = 
                        case c.state of
                            DamagePending amt dice rolled ->
                                let set = (List.Extra.getAt sid c.activePool) |> withDefault (Set 0 0) in
                                do_damage cid {c | activePool =
                                    (List.Extra.updateAt sid (\s ->
                                        Set (s.width - 1) s.height) c.activePool |>
                                        List.sortWith (desc set_compare)),
                                    state = DamagePending amt (List.append dice [set.height]) rolled
                                }
                            SpendingSkill die ->
                                let val = c.skillPool !! die in
                                let applySkill c x =
                                    let set = c.activePool !! sid |> withDefault (Set 0 0) in
                                    let new_height = (set.height + x) in
                                    if new_height > 10 || new_height < 1 then (c, always Cmd.none) else
                                    ({c | state = Normal,
                                         skillPool = List.Extra.removeAt die c.skillPool,
                                         activePool =
                                            let pool = if set.width == 1 then
                                                List.Extra.removeAt sid c.activePool
                                            else
                                                List.Extra.updateAt sid (\s -> Set (s.width - 1) s.height) c.activePool in
                                            gen_sets (List.append (set_pool_dissolve pool) [new_height])
                                    }, save_cmd) in
                                case val of
                                    Just 1 -> applySkill c -1
                                    Just 3 -> applySkill c 1
                                    _ -> (c, always Cmd.none)
                            _ ->
                                let set = (List.Extra.getAt sid c.activePool) in
                                ({c | activePool =
                                    (List.Extra.removeAt sid c.activePool),
                                    spentPool =
                                        List.append c.spentPool (set_split set) |>
                                        List.sortWith (desc compare)
                                }, save_cmd) in
                    let mod = {model | creatures = List.Extra.updateAt cid (always cp) model.creatures } in
                    (mod, cmd mod)
        UnspendDie cid did ->
            simple_update cid (\c ->
                let die = (List.Extra.getAt did c.spentPool) in
                {c | spentPool =
                    (List.Extra.removeAt did c.spentPool),
                    activePool = case die of
                        Just die -> set_add_die die c.activePool
                        Nothing -> c.activePool
                })
        AddDamage cid amt ->
            let c = List.Extra.getAt cid model.creatures in
            case c of
                Nothing -> model ! []
                Just c ->
                    let cp = {c | state = DamagePending amt [] []} in
                    let (cpp, cmd) =
                        if amt > 0 then do_damage cid cp else (do_healing (0 - amt) c, save_cmd)
                    in
                    let m = {model | creatures = List.Extra.setAt cid cpp model.creatures} in
                    m ! [cmd m]
        NameChange i s ->
            silent_update i (\c -> {c | name = s })
        DescChange i s ->
            silent_update i (\c -> {c | desc = s })
        ResultsOne i (l, s) ->
            simple_update i (\c -> updateSets c l s)
        CloseDamage cid ->
            simple_update cid (\c -> {c | state = Normal })
        SpendSkill i s ->
            simple_update i (\c -> 
                let dS = c.skillPool !! s in
                    case dS of
                        Just 2 -> {c |
                                skillPool = List.Extra.removeAt s c.skillPool,
                                state = case c.state of
                                    SpendingSkill x ->
                                        SpendingSkill (if x > s then x - 1 else x)
                                    _ -> c.state
                            }
                        _ -> case c.state of
                            Normal -> {c | state = SpendingSkill s }
                            SpendingSkill x -> {c |
                                state = if x == s then Normal else SpendingSkill s}
                            _ -> c)
        ChangeD10s cid amt ->
            simple_update cid (\c -> {c | d10s = clamp 2 15 <| c.d10s + amt })
        ChangeDSs cid amt ->
            simple_update cid (\c -> {c | dSs = clamp 0 3 <| c.dSs + amt })
        UrlChanged loc ->
            ({model | creatures = (load_state (String.dropLeft 1 loc.hash))}, Cmd.none)

list_split_last : List a -> (List a, Maybe a)
list_split_last list =
    let len = List.length list in
    let (front, back) = List.Extra.splitAt (len - 1) list in
    (front, List.head back)

do_healing : Int -> Creature -> Creature
do_healing amt c =
    if amt == 0 then c else
        let (front, last) = list_split_last c.damagePool in
        case last of
            Just i -> do_healing (amt - 1)
                {c | damagePool = front,
                     spentPool = List.append c.spentPool [i]}
            Nothing -> {c | damageTokens = max 0 (c.damageTokens - amt)}

take_damage_die : Creature -> (Maybe Int, Creature)
take_damage_die c =
    let (front, back) = list_split_last c.activePool in
    case back of
        Just set ->
            if set.width == 1 then
                (Just set.height, {c | activePool = front})
            else
                let put_back_set c s =
                        {c | activePool = List.append front [Set (set.width - 1) set.height] |>
                            List.sortWith (desc set_compare)} in
                case List.head front of
                    Just fset ->
                        if fset.width == set.width then
                            (Just set.height, put_back_set c set)
                        else
                            (Nothing, c)
                    Nothing -> (Just set.height, put_back_set c set)
        Nothing ->
            let (front, back) = list_split_last c.spentPool in
            case back of
                Just i -> (Just i, {c | spentPool = front})
                Nothing -> (Just 1, c)

set_pool_length : (List Set) -> Int
set_pool_length sets =
    List.foldl (\x a -> a + x.width) 0 sets

set_pool_dissolve : (List Set) -> (List Int)
set_pool_dissolve sets =
    List.concat <| List.map (\x -> List.repeat x.width x.height) sets

do_damage : Int -> Creature -> (Creature, Model -> Cmd Msg)
do_damage idx c =
    case c.state of
        DamagePending amt dice rolled ->
            let dlen = List.length dice
                rlen = List.length rolled
                alen = set_pool_length c.activePool
                tlen = dlen + rlen in
            if amt == tlen then
                (c, always (Random.generate (DamageResult idx rolled) <|
                        Random.list (amt - rlen) <|
                        Random.int 1 10))
            else if amt == tlen + alen then
                do_damage idx {c |
                    activePool = [],
                    state = DamagePending amt (List.append dice (set_pool_dissolve c.activePool)) rolled }
            else
                case take_damage_die c of
                    (Just i, c) -> do_damage idx {c | state =
                        DamagePending amt (List.append dice [i]) rolled }
                    (Nothing, c) -> (c, save_cmd)
        _ -> (c, always Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none