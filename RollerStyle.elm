module RollerStyle exposing (..)

import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import Style.Filter as Filter
import Style.Shadow as Shadow
import Style.Border as Border
import Color

type MyStyles =
    NullStyle | ButtonStyle | DieStyle | SetStyle | TopBarStyle | LabelStyle |
    CreatureNameStyle | CreatureStyle | FakeCreatureStyle |
    ActivePoolStyle | SpentPoolStyle | DamagePoolStyle |
    DamageCellStyle | DamageDealtStyle |
    CloseButtonStyle | AddCreatureStyle

type MyVariations =
    Ghosted | MaxWidth | Important

stylesheet : StyleSheet MyStyles MyVariations
stylesheet =
    let
        mk_border deg = Color.hsl (degrees deg) 0.5 0.4
        mk_background deg = Color.hsl (degrees deg) 0.3 0.7
        mk_background_hover deg = Color.hsl (degrees deg) 0.3 0.75
        mk_pool deg = [
            Color.border (mk_border deg),
            Color.background (mk_background deg),
            Border.all 2,
            Font.typeface [Font.sansSerif],
            Font.bold,
            Border.rounded 4,
            hover [
                Color.background (mk_background_hover deg)
            ]
        ]
        font_sans = Font.typeface [Font.sansSerif]
    in
    styleSheet [
        style NullStyle [
        ],
        style CreatureStyle [
            Color.background Color.lightGray,
            Shadow.glow Color.black 2,
            Border.rounded 4,
            variation Ghosted [
                Filter.opacity 33,
                Shadow.glow Color.black 1,
                hover [
                    Filter.opacity 66
                ]
            ]
        ],
        style ButtonStyle [
            font_sans,
            Color.background Color.lightGray,
            variation Ghosted [
                Filter.opacity 50,
                cursor "default"
            ],
            variation Important [
                Color.background (Color.hsl (degrees 60) 0.7 0.65)
            ]
        ],
        style DieStyle [
            font_sans
        ],
        style SetStyle [
            font_sans,
            variation MaxWidth [
                Color.background Color.black,
                Color.text Color.white
            ],
            variation Ghosted [
                Filter.opacity 50
            ]
        ],
        let active_style = [
                Color.background Color.white,
                Color.border Color.black,
                Border.all 1
            ]
        in style CreatureNameStyle [
            font_sans,
            Font.bold,
            Color.background Color.lightGray,
            Color.border Color.lightGray,
            Border.all 1,
            hover active_style,
            focus active_style
        ],
        style ActivePoolStyle <| mk_pool 240,
        style SpentPoolStyle <| mk_pool 120,
        style DamagePoolStyle <| mk_pool 0,
        style TopBarStyle [
            Color.background Color.white,
            Border.bottom 0.5,
            Shadow.glow Color.black 3
        ],
        style DamageCellStyle [
            Color.background (Color.hsl (degrees 0) 0.4 0.65),
            Color.text (Color.hsl (degrees 0) 0.4 0.25),
            variation Ghosted [
                Filter.opacity 50
            ],
            variation Important [
                Color.background (Color.hsl (degrees 0) 0.8 0.65),
                Font.bold
            ]
        ],
        style DamageDealtStyle [
            Color.border (Color.hsl (degrees 0) 0.6 0.45),
            Color.background (Color.hsl (degrees 0) 0.4 0.8),
            Border.all 1,
            Font.typeface [Font.sansSerif],
            Border.rounded 4,
            hover [
                Color.background (Color.hsl (degrees 0) 0.4 0.85)
            ]
        ],
        style CloseButtonStyle [
            Border.all 1,
            Color.border (Color.rgba 0 0 0 0),
            Color.background (Color.rgba 0 0 0 0),
            Color.text (Color.hsl (degrees 0) 0.8 0.25),
            Font.bold,
            Font.size 18,
            hover [
                Border.all 1,
                Border.rounded 4,
                Color.background (Color.hsl (degrees 0) 0.8 0.75),
                Color.border (Color.hsl (degrees 0) 0.8 0.25),
                Color.text (Color.hsl (degrees 0) 0.8 0.5)
            ]
        ],
        style AddCreatureStyle [
            Font.size 70,
            Font.bold,
            Color.text (Color.hsl (degrees 120) 0.4 0.2),
            Color.background (Color.hsl (degrees 120) 0.4 0.6),
            Color.border (Color.hsl (degrees 120) 0.4 0.2),
            Border.all 4,
            Border.rounded 50,
            cursor "default"
        ],
        style LabelStyle [
            cursor "default"
        ]
    ]
