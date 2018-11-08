module Data.Mgz.Constants.Objects where

-- borrowed from https://github.com/happyleavesaoc/aoc-mgz/blob/master/mgz/const.py
import RIO
data ObjectType =
    OT_Legionary
 | OT_Archer
 | OT_HandCannoneer
 | OT_EliteSkirmisher
 | OT_Skirmisher
 | OT_Longbowman
 | OT_Arrow
 | OT_ArcheryRange
 | OT_Mangudai
 | OT_Barracks
 | OT_FishingShip
 | OT_Junk
 | OT_TradeCog
 | OT_Blacksmith
 | OT_WarGalley
 | OT_Crossbowman
 | OT_TeutonicKnight
 | OT_Deadcrossbowman
 | OT_Monastery
 | OT_Fortress
 | OT_BatteringRam
 | OT_BombardCannon
 | OT_LightCavalry
 | OT_Knight
 | OT_CavalryArcher
 | OT_Cataphract
 | OT_Huskarl
 | OT_Trebuchet
 | OT_Dock
 | OT_Janissary
 | OT_WildBoar
 | OT_SiegeWorkshop
 | OT_Farm
 | OT_RoyalJanissary
 | OT_FishPerch
 | OT_Fisherman
 | OT_ForageBush
 | OT_Dolphin
 | OT_Gate
 | OT_Deer
 | OT_GoldMine
 | OT_Mill
 | OT_ShoreFish
 | OT_House
 | OT_TownCenter
 | OT_PalisadeWall
 | OT_ChuKoNu
 | OT_Militia
 | OT_ManAtArms
 | OT_HeavySwordsman
 | OT_LongSwordsman
 | OT_WatchTower
 | OT_Castle
 | OT_Villager
 | OT_Market
 | OT_Stable
 | OT_DireWolf
 | OT_Spearman
 | OT_Berserk
 | OT_Hawk
 | OT_StoneMine
 | OT_TradeWorkshop
 | OT_Deadknight
 | OT_Flare
 | OT_StoneWall
 | OT_Builder
 | OT_FortifiedPalisadeWall
 | OT_Forager
 | OT_Hunter
 | OT_Lumberjack
 | OT_StoneMiner
 | OT_Monk
 | OT_Wolf
 | OT_TradeCart
 | OT_Rubble1x1
 | OT_Rubble2x2
 | OT_Rubble3x3
 | OT_Rubble4x4
 | OT_Rubble6x6
 | OT_Rubble8x8
 | OT_FortifiedWall
 | OT_Repairer
 | OT_RelicCart
 | OT_RichardtheLionheart
 | OT_TheBlackPrince
 | OT_FriarTuck
 | OT_SheriffofNottingham
 | OT_Charlemagne
 | OT_Roland
 | OT_Belisarius
 | OT_TheodorictheGoth
 | OT_Aethelfirth
 | OT_Siegfried
 | OT_EriktheRed
 | OT_Tamerlane
 | OT_KingArthur
 | OT_Lancelot
 | OT_Gawain
 | OT_Mordred
 | OT_Archbishop
 | OT_Deadlongswordman
 | OT_Condottiero
 | OT_Slinger
 | OT_Flamethrower
 | OT_FireTower
 | OT_VladDracula
 | OT_Kitabatake
 | OT_Minamoto
 | OT_AlexanderNevski
 | OT_ElCid
 | OT_FishTrap
 | OT_RobinHood
 | OT_RabidWolf
 | OT_VMDL
 | OT_ImperialCamel
 | OT_University
 | OT_Farmer
 | OT_Falcon
 | OT_Aqueduct
 | OT_WoadRaider
 | OT_GuardTower
 | OT_Keep
 | OT_BombardTower
 | OT_WarElephant
 | OT_Cracks
 | OT_Osman
 | OT_PileofStone
 | OT_Longboat
 | OT_Amphitheatre
 | OT_PileofGold
 | OT_PileofWood
 | OT_PileofFood
 | OT_Colosseum
 | OT_Harbor
 | OT_Centurion
 | OT_Wonder
 | OT_DeadFishTrap
 | OT_Scorpion
 | OT_Mangonel
 | OT_ThrowingAxeman
 | OT_Mameluke
 | OT_Cavalier
 | OT_TreeTD
 | OT_Relic
 | OT_MonkwithRelic
 | OT_BritishRelic
 | OT_ByzantineRelic
 | OT_ChineseRelic
 | OT_FrankishRelic
 | OT_Samurai
 | OT_GothicRelic
 | OT_JapaneseRelic
 | OT_PersianRelic
 | OT_SaracenRelic
 | OT_TeutonicRelic
 | OT_TurkishRelic
 | OT_Bandit
 | OT_GrassPatch
 | OT_Bush
 | OT_Seagulls
 | OT_Bonfire
 | OT_Llama
 | OT_BlackTile
 | OT_Cuauhtemoc
 | OT_MonkwithTurkishRelic
 | OT_Mountain1
 | OT_Mountain2
 | OT_Camel
 | OT_HeavyCamel
 | OT_TrebuchetPacked
 | OT_Flowers1
 | OT_Flowers2
 | OT_Flowers3
 | OT_Flowers4
 | OT_Path4
 | OT_Path1
 | OT_Path2
 | OT_Path3
 | OT_Ruins
 | OT_BambooForestTree
 | OT_OakForestTree
 | OT_PineForestTree
 | OT_PalmForestTree
 | OT_ArmyTent
 | OT_DeadFarm
 | OT_Pikeman
 | OT_Halberdier
 | OT_NordicSwordsman
 | OT_CityWall
 | OT_SeaRocks1
 | OT_Pagoda
 | OT_SeaRocks2
 | OT_SanchiStupa
 | OT_GolGumbaz
 | OT_TreeA
 | OT_TreeB
 | OT_TreeC
 | OT_TreeD
 | OT_TreeE
 | OT_TreeF
 | OT_TreeG
 | OT_TreeH
 | OT_TreeI
 | OT_TreeJ
 | OT_TreeK
 | OT_TreeL
 | OT_ForestTree
 | OT_SnowPineTree
 | OT_JungleTree
 | OT_Stump
 | OT_CannonGalleon
 | OT_CappedRam
 | OT_CharlesMartel
 | OT_FranciscodeOrellana
 | OT_HaraldHardraade
 | OT_GonzaloPizarro
 | OT_HrolftheGanger
 | OT_FrederickBarbarossa
 | OT_JoantheMaid
 | OT_WilliamWallace
 | OT_King
 | OT_Prithviraj
 | OT_FrancescoSforza
 | OT_Petard
 | OT_Hussar
 | OT_Galleon
 | OT_PoenariCastle
 | OT_Port
 | OT_ScoutCavalry
 | OT_GreatFishMarlin
 | OT_FishDorado
 | OT_FishSalmon
 | OT_FishTuna
 | OT_FishSnapper
 | OT_Loot
 | OT_TwoHandedSwordsman
 | OT_HeavyCavalryArcher
 | OT_Bear
 | OT_Arbalest
 | OT_AdvancedHeavyCrossbowman
 | OT_Torch
 | OT_Deadpikeman
 | OT_DemolitionShip
 | OT_HeavyDemolitionShip
 | OT_FireShip
 | OT_EliteLongbowman
 | OT_EliteThrowingAxeman
 | OT_FastFireShip
 | OT_EliteLongboat
 | OT_EliteWoadRaider
 | OT_Galley
 | OT_HeavyScorpion
 | OT_TransportShip
 | OT_Deadlightcavalry
 | OT_SiegeRam
 | OT_Onager
 | OT_EliteCataphract
 | OT_EliteTeutonicKnight
 | OT_EliteHuskarl
 | OT_EliteMameluke
 | OT_EliteJanissary
 | OT_EliteWarElephant
 | OT_EliteChuKoNu
 | OT_EliteSamurai
 | OT_EliteMangudai
 | OT_LumberCamp
 | OT_Champion
 | OT_Paladin
 | OT_GoldMiner
 | OT_Genitour
 | OT_MiningCamp
 | OT_SiegeOnager
 | OT_Shepherd
 | OT_Sheep
 | OT_EliteGenitour
 | OT_Outpost
 | OT_Cathedral
 | OT_FlagA
 | OT_FlagB
 | OT_FlagC
 | OT_FlagD
 | OT_FlagE
 | OT_BridgeA_Top
 | OT_BridgeA_Middle
 | OT_BridgeA_Bottom
 | OT_BridgeB_Top
 | OT_BridgeB_Middle
 | OT_BridgeB_Bottom
 | OT_Rock1
 | OT_Pavilion
 | OT_JoanofArc
 | OT_FrankishPaladin
 | OT_SieurdeMetz
 | OT_SieurBertrand
 | OT_TempleofHeaven
 | OT_DukeDAlencon
 | OT_Penguin
 | OT_LaHire
 | OT_LorddeGraville
 | OT_JeandeLorrain
 | OT_ConstableRichemont
 | OT_GuyJosselyne
 | OT_JeanBureau
 | OT_SirJohnFastolf
 | OT_Mosque
 | OT_ReynalddeChatillon
 | OT_MasteroftheTemplar
 | OT_BadNeighbor
 | OT_GodsOwnSling
 | OT_TheAccursedTower
 | OT_TheTowerofFlies
 | OT_ArchersoftheEyes
 | OT_PieceoftheTrueCross
 | OT_Pyramid
 | OT_DomeoftheRock
 | OT_EliteCannonGalleon
 | OT_EliteBerserk
 | OT_GreatPyramid
 | OT_Subotai
 | OT_HuntingWolf
 | OT_Kushluk
 | OT_Shah
 | OT_Cow
 | OT_Saboteur
 | OT_OrnlutheWolf
 | OT_Cactus
 | OT_Skeleton
 | OT_Rugs
 | OT_Yurt
 | OT_NineBands
 | OT_Shipwreck
 | OT_Crater
 | OT_JaguarWarrior
 | OT_EliteJaguarWarrior
 | OT_Ice
 | OT_GodsOwnSlingPacked
 | OT_BadNeighborPacked
 | OT_GenghisKhan
 | OT_EmperorinaBarrel
 | OT_BambooStump
 | OT_BridgeA_Cracked
 | OT_BridgeA_BrokenTop
 | OT_BridgeA_BrokenBottom
 | OT_BridgeB_Cracked
 | OT_BridgeB_BrokenTop
 | OT_BridgeB_BrokenBottom
 | OT_Mountain3
 | OT_Mountain4
 | OT_CobraCar
 | OT_EagleScout
 | OT_EliteEagleWarrior
 | OT_EagleWarrior
 | OT_Tarkan
 | OT_EliteTarkan
 | OT_Burnedbuilding
 | OT_PlumedArcher
 | OT_ElitePlumedArcher
 | OT_Conquistador
 | OT_EliteConquistador
 | OT_Missionary
 | OT_AttilatheHun
 | OT_Canoe
 | OT_BledatheHun
 | OT_PopeLeoI
 | OT_ScythianWildWoman
 | OT_SeaTower
 | OT_SeaWall
 | OT_PalisadeGate
 | OT_IronBoar
 | OT_Jaguar
 | OT_Horse
 | OT_Macaw
 | OT_Statue
 | OT_Plant
 | OT_Sign
 | OT_Grave
 | OT_Head
 | OT_Javelina
 | OT_ElCidCampeador
 | OT_AmazonWarrior
 | OT_Monument
 | OT_WarWagon
 | OT_EliteWarWagon
 | OT_TurtleShip
 | OT_EliteTurtleShip
 | OT_Turkey
 | OT_WildHorse
 | OT_MapRevealer
 | OT_KingSancho
 | OT_RockStone
 | OT_KingAlfonso
 | OT_RockGold
 | OT_Imam
 | OT_AdmiralYiSunshin
 | OT_Nobunaga
 | OT_Donkey
 | OT_HenryV
 | OT_WilliamtheConqueror
 | OT_AmazonArcher
 | OT_ESFlag
 | OT_ScythianScout
 | OT_TorchConverting
 | OT_OldStoneHead
 | OT_RomanRuins
 | OT_HayStack
 | OT_BrokenCart
 | OT_FlowerBed
 | OT_FurioustheMonkeyBoy
 | OT_StormyDog
 | OT_GenoeseCrossbowman
 | OT_EliteGenoeseCrossbowman
 | OT_MagyarHuszar
 | OT_EliteMagyarHuszar
 | OT_QuimperCathedral
 | OT_ElephantArcher
 | OT_EliteElephantArcher
 | OT_Boyar
 | OT_EliteBoyar
 | OT_Kamayuk
 | OT_EliteKamayuk
 | OT_WildCamel
 | OT_SiegeTower
 | OT_HeavyPikeman
 | OT_EasternSwordsman
 | OT_Waterfall
 | OT_CamelGaia
 | OT_ArchofConstantine
 | OT_Rain
 | OT_FlagF
 | OT_Smoke
 | OT_WoodenBridgeA_Top
 | OT_WoodenBridgeA_Middle
 | OT_WoodenBridgeA_Bottom
 | OT_WoodenBridgeB_Top
 | OT_WoodenBridgeB_Middle
 | OT_WoodenBridgeB_Bottom
 | OT_ImpaledCorpse
 | OT_Quarry
 | OT_Lumber
 | OT_Goods
 | OT_Vulture
 | OT_Rock2
 | OT_Queen
 | OT_Sanyogita
 | OT_Prithvi
 | OT_ChandBhai
 | OT_Saladin
 | OT_Khosrau
 | OT_Jarl
 | OT_Savaran
 | OT_Barrels
 | OT_AlfredtheAlpaca
 | OT_Elephant
 | OT_DragonShip
 | OT_Flame1
 | OT_Flame2
 | OT_Flame3
 | OT_Flame4
 | OT_OrganGun
 | OT_EliteOrganGun
 | OT_Caravel
 | OT_EliteCaravel
 | OT_CamelArcher
 | OT_EliteCamelArcher
 | OT_Gbeto
 | OT_EliteGbeto
 | OT_ShotelWarrior
 | OT_EliteShotelWarrior
 | OT_Zebra
 | OT_Feitoria
 | OT_Priest
 | OT_Ostrich
 | OT_Stork
 | OT_Lion
 | OT_Crocodile
 | OT_SavannahGrassPatch
 | OT_MusaibnNusayr
 | OT_Sundjata
 | OT_TariqibnZiyad
 | OT_RicharddeClare
 | OT_Tristan
 | OT_PrincessYodit
 | OT_HenryII
 | OT_Mountain5
 | OT_Mountain6
 | OT_Mountain7
 | OT_Mountain8
 | OT_SnowMountain1
 | OT_SnowMountain2
 | OT_SnowMountain3
 | OT_RockFormation1
 | OT_RockFormation2
 | OT_RockFormation3
 | OT_DragonTree
 | OT_BaobabTree
 | OT_Bush2
 | OT_Bush3
 | OT_FruitBush
 | OT_Goat
 | OT_Fence
 | OT_AcaciaTree
 | OT_YekunoAmlak
 | OT_Yodit
 | OT_Itzcoatl
 | OT_MustafaPasha
 | OT_PacalII
 | OT_Babur
 | OT_AbrahaElephant
 | OT_GuglielmoEmbriaco
 | OT_SuDingfang
 | OT_Pachacuti
 | OT_HuaynaCapac
 | OT_MiklosToldi
 | OT_LittleJohn
 | OT_ZawiszatheBlack
 | OT_Sumanguru
 | OT_Storage
 | OT_Hut
 | OT_Granary
 | OT_Barricade
 | OT_Animalskeleton
 | OT_StelaeA
 | OT_StelaeB
 | OT_StelaeC
 | OT_Gallow
 | OT_Palace
 | OT_Tent
 | OT_SeaFortification
 | OT_FireGalley
 | OT_DemolitionRaft
 | OT_Dagnajan
 | OT_Gidajan
 | OT_BallistaElephant
 | OT_EliteBallistaElephant
 | OT_KarambitWarrior
 | OT_EliteKarambitWarrior
 | OT_Arambai
 | OT_EliteArambai
 | OT_RattanArcher
 | OT_EliteRattanArcher
 | OT_BattleElephant
 | OT_EliteBattleElephant
 | OT_KomodoDragon
 | OT_Tiger
 | OT_Rhinoceros
 | OT_BoxTurtles
 | OT_WaterBuffalo
 | OT_MangroveTree
 | OT_RainforestTree
 | OT_RockBeach
 | OT_RockJungle
 | OT_FlagG
 | OT_FlagH
 | OT_FlagI
 | OT_FlagJ
 | OT_ImperialSkirmisher
 | OT_GajahMada
 | OT_Jayanegara
 | OT_RadenWijaya
 | OT_SundaRoyalFighter
 | OT_SuryavarmanI
 | OT_UdayadityavarmanI
 | OT_Jayaviravarman
 | OT_Bayinnaung
 | OT_Tabinshwehti
 | OT_BuddhaStatueA
 | OT_BuddhaStatueB
 | OT_BuddhaStatueC
 | OT_BuddhaStatueD
 | OT_FernPatch
 | OT_TrowulanGate
 | OT_Vases
 | OT_LeLoi
 | OT_LeLai
 | OT_LeTrien
 | OT_LuuNhanChu
 | OT_BuiBi
 | OT_DinhLe
 | OT_WangTong
 | OT_Envoy
 | OT_RiceFarm
 | OT_DeadRiceFarm
 | OT_Stupa
 | OT_BridgeC_Top
 | OT_BridgeC_Middle
 | OT_BridgeC_Bottom
 | OT_BridgeD_Top
 | OT_BridgeD_Middle
 | OT_BridgeD_Bottom
 | OT_BridgeC_Cracked
 | OT_BridgeC_BrokenTop
 | OT_BridgeC_BrokenBottom
 | OT_BridgeD_Cracked
 | OT_BridgeD_BrokenTop
 | OT_BridgeD_BrokenBottom
 | OT_Sharkatzor
 | OT_EliteKarambit
 | OT_Karambit
 | OT_Unknown Int
 deriving (Show, Eq, Ord, Generic)
instance Hashable ObjectType


normaliseObjectType :: Int -> ObjectType
normaliseObjectType 1 = OT_Legionary
normaliseObjectType 4 = OT_Archer
normaliseObjectType 5 = OT_HandCannoneer
normaliseObjectType 6 = OT_EliteSkirmisher
normaliseObjectType 7 = OT_Skirmisher
normaliseObjectType 8 = OT_Longbowman
normaliseObjectType 9 = OT_Arrow
normaliseObjectType 10 = OT_ArcheryRange
normaliseObjectType 11 = OT_Mangudai
normaliseObjectType 12 = OT_Barracks
normaliseObjectType 13 = OT_FishingShip
normaliseObjectType 14 = OT_ArcheryRange
normaliseObjectType 15 = OT_Junk
normaliseObjectType 17 = OT_TradeCog
normaliseObjectType 18 = OT_Blacksmith
normaliseObjectType 19 = OT_Blacksmith
normaliseObjectType 20 = OT_Barracks
normaliseObjectType 21 = OT_WarGalley
normaliseObjectType 24 = OT_Crossbowman
normaliseObjectType 25 = OT_TeutonicKnight
normaliseObjectType 26 = OT_Deadcrossbowman
normaliseObjectType 30 = OT_Monastery
normaliseObjectType 31 = OT_Monastery
normaliseObjectType 32 = OT_Monastery
normaliseObjectType 33 = OT_Fortress
normaliseObjectType 35 = OT_BatteringRam
normaliseObjectType 36 = OT_BombardCannon
normaliseObjectType 37 = OT_LightCavalry
normaliseObjectType 38 = OT_Knight
normaliseObjectType 39 = OT_CavalryArcher
normaliseObjectType 40 = OT_Cataphract
normaliseObjectType 41 = OT_Huskarl
normaliseObjectType 42 = OT_Trebuchet
normaliseObjectType 45 = OT_Dock
normaliseObjectType 46 = OT_Janissary
normaliseObjectType 47 = OT_Dock
normaliseObjectType 48 = OT_WildBoar
normaliseObjectType 49 = OT_SiegeWorkshop
normaliseObjectType 50 = OT_Farm
normaliseObjectType 51 = OT_Dock
normaliseObjectType 52 = OT_RoyalJanissary
normaliseObjectType 53 = OT_FishPerch
normaliseObjectType 54 = OT_Arrow
normaliseObjectType 56 = OT_Fisherman
normaliseObjectType 57 = OT_Fisherman
normaliseObjectType 59 = OT_ForageBush
normaliseObjectType 61 = OT_Dolphin
normaliseObjectType 63 = OT_Gate
normaliseObjectType 64 = OT_Gate
normaliseObjectType 65 = OT_Deer
normaliseObjectType 66 = OT_GoldMine
normaliseObjectType 67 = OT_Gate
normaliseObjectType 68 = OT_Mill
normaliseObjectType 69 = OT_ShoreFish
normaliseObjectType 70 = OT_House
normaliseObjectType 71 = OT_TownCenter
normaliseObjectType 72 = OT_PalisadeWall
normaliseObjectType 73 = OT_ChuKoNu
normaliseObjectType 74 = OT_Militia
normaliseObjectType 75 = OT_ManAtArms
normaliseObjectType 76 = OT_HeavySwordsman
normaliseObjectType 77 = OT_LongSwordsman
normaliseObjectType 78 = OT_Gate
normaliseObjectType 79 = OT_WatchTower
normaliseObjectType 80 = OT_Gate
normaliseObjectType 81 = OT_Gate
normaliseObjectType 82 = OT_Castle
normaliseObjectType 83 = OT_Villager
normaliseObjectType 84 = OT_Market
normaliseObjectType 85 = OT_Gate
normaliseObjectType 86 = OT_Stable
normaliseObjectType 87 = OT_ArcheryRange
normaliseObjectType 88 = OT_Gate
normaliseObjectType 89 = OT_DireWolf
normaliseObjectType 90 = OT_Gate
normaliseObjectType 91 = OT_Gate
normaliseObjectType 92 = OT_Gate
normaliseObjectType 93 = OT_Spearman
normaliseObjectType 94 = OT_Berserk
normaliseObjectType 95 = OT_Gate
normaliseObjectType 96 = OT_Hawk
normaliseObjectType 97 = OT_Arrow
normaliseObjectType 101 = OT_Stable
normaliseObjectType 102 = OT_StoneMine
normaliseObjectType 103 = OT_Blacksmith
normaliseObjectType 104 = OT_Monastery
normaliseObjectType 105 = OT_Blacksmith
normaliseObjectType 109 = OT_TownCenter
normaliseObjectType 110 = OT_TradeWorkshop
normaliseObjectType 111 = OT_Deadknight
normaliseObjectType 112 = OT_Flare
normaliseObjectType 116 = OT_Market
normaliseObjectType 117 = OT_StoneWall
normaliseObjectType 118 = OT_Builder
normaliseObjectType 119 = OT_FortifiedPalisadeWall
normaliseObjectType 120 = OT_Forager
normaliseObjectType 122 = OT_Hunter
normaliseObjectType 123 = OT_Lumberjack
normaliseObjectType 124 = OT_StoneMiner
normaliseObjectType 125 = OT_Monk
normaliseObjectType 126 = OT_Wolf
normaliseObjectType 128 = OT_TradeCart
normaliseObjectType 129 = OT_Mill
normaliseObjectType 130 = OT_Mill
normaliseObjectType 131 = OT_Mill
normaliseObjectType 132 = OT_Barracks
normaliseObjectType 133 = OT_Dock
normaliseObjectType 137 = OT_Market
normaliseObjectType 141 = OT_TownCenter
normaliseObjectType 142 = OT_TownCenter
normaliseObjectType 143 = OT_Rubble1x1
normaliseObjectType 144 = OT_Rubble2x2
normaliseObjectType 145 = OT_Rubble3x3
normaliseObjectType 146 = OT_Rubble4x4
normaliseObjectType 147 = OT_Rubble6x6
normaliseObjectType 148 = OT_Rubble8x8
normaliseObjectType 150 = OT_SiegeWorkshop
normaliseObjectType 153 = OT_Stable
normaliseObjectType 155 = OT_FortifiedWall
normaliseObjectType 156 = OT_Repairer
normaliseObjectType 159 = OT_RelicCart
normaliseObjectType 160 = OT_RichardtheLionheart
normaliseObjectType 161 = OT_TheBlackPrince
normaliseObjectType 163 = OT_FriarTuck
normaliseObjectType 164 = OT_SheriffofNottingham
normaliseObjectType 165 = OT_Charlemagne
normaliseObjectType 166 = OT_Roland
normaliseObjectType 167 = OT_Belisarius
normaliseObjectType 168 = OT_TheodorictheGoth
normaliseObjectType 169 = OT_Aethelfirth
normaliseObjectType 170 = OT_Siegfried
normaliseObjectType 171 = OT_EriktheRed
normaliseObjectType 172 = OT_Tamerlane
normaliseObjectType 173 = OT_KingArthur
normaliseObjectType 174 = OT_Lancelot
normaliseObjectType 175 = OT_Gawain
normaliseObjectType 176 = OT_Mordred
normaliseObjectType 177 = OT_Archbishop
normaliseObjectType 179 = OT_TradeWorkshop
normaliseObjectType 180 = OT_Deadlongswordman
normaliseObjectType 184 = OT_Condottiero
normaliseObjectType 185 = OT_Slinger
normaliseObjectType 188 = OT_Flamethrower
normaliseObjectType 190 = OT_FireTower
normaliseObjectType 191 = OT_Rubble2x2
normaliseObjectType 192 = OT_Rubble2x2
normaliseObjectType 193 = OT_VladDracula
normaliseObjectType 195 = OT_Kitabatake
normaliseObjectType 196 = OT_Minamoto
normaliseObjectType 197 = OT_AlexanderNevski
normaliseObjectType 198 = OT_ElCid
normaliseObjectType 199 = OT_FishTrap
normaliseObjectType 200 = OT_RobinHood
normaliseObjectType 202 = OT_RabidWolf
normaliseObjectType 204 = OT_TradeCart
normaliseObjectType 206 = OT_VMDL
normaliseObjectType 207 = OT_ImperialCamel
normaliseObjectType 209 = OT_University
normaliseObjectType 210 = OT_University
normaliseObjectType 212 = OT_Builder
normaliseObjectType 214 = OT_Farmer
normaliseObjectType 216 = OT_Hunter
normaliseObjectType 218 = OT_Lumberjack
normaliseObjectType 220 = OT_StoneMiner
normaliseObjectType 221 = OT_Falcon
normaliseObjectType 222 = OT_Repairer
normaliseObjectType 229 = OT_Falcon
normaliseObjectType 231 = OT_Aqueduct
normaliseObjectType 232 = OT_WoadRaider
normaliseObjectType 234 = OT_GuardTower
normaliseObjectType 235 = OT_Keep
normaliseObjectType 236 = OT_BombardTower
normaliseObjectType 239 = OT_WarElephant
normaliseObjectType 241 = OT_Cracks
normaliseObjectType 246 = OT_Osman
normaliseObjectType 248 = OT_PileofStone
normaliseObjectType 250 = OT_Longboat
normaliseObjectType 251 = OT_Amphitheatre
normaliseObjectType 252 = OT_PileofGold
normaliseObjectType 253 = OT_PileofWood
normaliseObjectType 259 = OT_Farmer
normaliseObjectType 262 = OT_PileofFood
normaliseObjectType 263 = OT_Colosseum
normaliseObjectType 264 = OT_Harbor
normaliseObjectType 265 = OT_Harbor
normaliseObjectType 266 = OT_Harbor
normaliseObjectType 267 = OT_Harbor
normaliseObjectType 268 = OT_Harbor
normaliseObjectType 269 = OT_Harbor
normaliseObjectType 270 = OT_Harbor
normaliseObjectType 271 = OT_Harbor
normaliseObjectType 272 = OT_Harbor
normaliseObjectType 273 = OT_Harbor
normaliseObjectType 274 = OT_Flare
normaliseObjectType 275 = OT_Centurion
normaliseObjectType 276 = OT_Wonder
normaliseObjectType 278 = OT_DeadFishTrap
normaliseObjectType 279 = OT_Scorpion
normaliseObjectType 280 = OT_Mangonel
normaliseObjectType 281 = OT_ThrowingAxeman
normaliseObjectType 282 = OT_Mameluke
normaliseObjectType 283 = OT_Cavalier
normaliseObjectType 284 = OT_TreeTD
normaliseObjectType 285 = OT_Relic
normaliseObjectType 286 = OT_MonkwithRelic
normaliseObjectType 287 = OT_BritishRelic
normaliseObjectType 288 = OT_ByzantineRelic
normaliseObjectType 289 = OT_ChineseRelic
normaliseObjectType 290 = OT_FrankishRelic
normaliseObjectType 291 = OT_Samurai
normaliseObjectType 292 = OT_GothicRelic
normaliseObjectType 293 = OT_Villager
normaliseObjectType 294 = OT_JapaneseRelic
normaliseObjectType 295 = OT_PersianRelic
normaliseObjectType 296 = OT_SaracenRelic
normaliseObjectType 297 = OT_TeutonicRelic
normaliseObjectType 298 = OT_TurkishRelic
normaliseObjectType 299 = OT_Bandit
normaliseObjectType 301 = OT_GrassPatch
normaliseObjectType 302 = OT_Bush
normaliseObjectType 303 = OT_Seagulls
normaliseObjectType 304 = OT_Bonfire
normaliseObjectType 305 = OT_Llama
normaliseObjectType 306 = OT_BlackTile
normaliseObjectType 307 = OT_Cuauhtemoc
normaliseObjectType 309 = OT_MonkwithTurkishRelic
normaliseObjectType 310 = OT_Mountain1
normaliseObjectType 311 = OT_Mountain2
normaliseObjectType 312 = OT_Arrow
normaliseObjectType 315 = OT_Arrow
normaliseObjectType 316 = OT_Arrow
normaliseObjectType 317 = OT_Arrow
normaliseObjectType 318 = OT_Arrow
normaliseObjectType 319 = OT_Arrow
normaliseObjectType 320 = OT_Arrow
normaliseObjectType 321 = OT_Arrow
normaliseObjectType 322 = OT_Arrow
normaliseObjectType 328 = OT_Arrow
normaliseObjectType 329 = OT_Camel
normaliseObjectType 330 = OT_HeavyCamel
normaliseObjectType 331 = OT_TrebuchetPacked
normaliseObjectType 332 = OT_Flare
normaliseObjectType 333 = OT_Deer
normaliseObjectType 334 = OT_Flowers1
normaliseObjectType 335 = OT_Flowers2
normaliseObjectType 336 = OT_Flowers3
normaliseObjectType 337 = OT_Flowers4
normaliseObjectType 338 = OT_Path4
normaliseObjectType 339 = OT_Path1
normaliseObjectType 340 = OT_Path2
normaliseObjectType 341 = OT_Path3
normaliseObjectType 345 = OT_Ruins
normaliseObjectType 348 = OT_BambooForestTree
normaliseObjectType 349 = OT_OakForestTree
normaliseObjectType 350 = OT_PineForestTree
normaliseObjectType 351 = OT_PalmForestTree
normaliseObjectType 352 = OT_ArmyTent
normaliseObjectType 354 = OT_Forager
normaliseObjectType 357 = OT_DeadFarm
normaliseObjectType 358 = OT_Pikeman
normaliseObjectType 359 = OT_Halberdier
normaliseObjectType 360 = OT_Arrow
normaliseObjectType 361 = OT_NordicSwordsman
normaliseObjectType 363 = OT_Arrow
normaliseObjectType 364 = OT_Arrow
normaliseObjectType 365 = OT_Arrow
normaliseObjectType 366 = OT_Arrow
normaliseObjectType 370 = OT_CityWall
normaliseObjectType 372 = OT_Arrow
normaliseObjectType 373 = OT_Arrow
normaliseObjectType 375 = OT_Arrow
normaliseObjectType 376 = OT_Arrow
normaliseObjectType 377 = OT_Arrow
normaliseObjectType 381 = OT_Osman
normaliseObjectType 389 = OT_SeaRocks1
normaliseObjectType 390 = OT_Pagoda
normaliseObjectType 396 = OT_SeaRocks2
normaliseObjectType 397 = OT_SanchiStupa
normaliseObjectType 398 = OT_GolGumbaz
normaliseObjectType 399 = OT_TreeA
normaliseObjectType 400 = OT_TreeB
normaliseObjectType 401 = OT_TreeC
normaliseObjectType 402 = OT_TreeD
normaliseObjectType 403 = OT_TreeE
normaliseObjectType 404 = OT_TreeF
normaliseObjectType 405 = OT_TreeG
normaliseObjectType 406 = OT_TreeH
normaliseObjectType 407 = OT_TreeI
normaliseObjectType 408 = OT_TreeJ
normaliseObjectType 409 = OT_TreeK
normaliseObjectType 410 = OT_TreeL
normaliseObjectType 411 = OT_ForestTree
normaliseObjectType 413 = OT_SnowPineTree
normaliseObjectType 414 = OT_JungleTree
normaliseObjectType 415 = OT_Stump
normaliseObjectType 420 = OT_CannonGalleon
normaliseObjectType 422 = OT_CappedRam
normaliseObjectType 424 = OT_CharlesMartel
normaliseObjectType 425 = OT_FranciscodeOrellana
normaliseObjectType 426 = OT_HaraldHardraade
normaliseObjectType 427 = OT_GonzaloPizarro
normaliseObjectType 428 = OT_HrolftheGanger
normaliseObjectType 429 = OT_FrederickBarbarossa
normaliseObjectType 430 = OT_JoantheMaid
normaliseObjectType 432 = OT_WilliamWallace
normaliseObjectType 434 = OT_King
normaliseObjectType 437 = OT_Prithviraj
normaliseObjectType 439 = OT_FrancescoSforza
normaliseObjectType 440 = OT_Petard
normaliseObjectType 441 = OT_Hussar
normaliseObjectType 442 = OT_Galleon
normaliseObjectType 445 = OT_PoenariCastle
normaliseObjectType 446 = OT_Port
normaliseObjectType 448 = OT_ScoutCavalry
normaliseObjectType 450 = OT_GreatFishMarlin
normaliseObjectType 451 = OT_GreatFishMarlin
normaliseObjectType 452 = OT_Dolphin
normaliseObjectType 455 = OT_FishDorado
normaliseObjectType 456 = OT_FishSalmon
normaliseObjectType 457 = OT_FishTuna
normaliseObjectType 458 = OT_FishSnapper
normaliseObjectType 463 = OT_House
normaliseObjectType 464 = OT_House
normaliseObjectType 465 = OT_House
normaliseObjectType 466 = OT_Arrow
normaliseObjectType 472 = OT_Loot
normaliseObjectType 473 = OT_TwoHandedSwordsman
normaliseObjectType 474 = OT_HeavyCavalryArcher
normaliseObjectType 475 = OT_Arrow
normaliseObjectType 476 = OT_Arrow
normaliseObjectType 477 = OT_Arrow
normaliseObjectType 478 = OT_Arrow
normaliseObjectType 481 = OT_TownCenter
normaliseObjectType 482 = OT_TownCenter
normaliseObjectType 483 = OT_TownCenter
normaliseObjectType 484 = OT_TownCenter
normaliseObjectType 485 = OT_Arrow
normaliseObjectType 486 = OT_Bear
normaliseObjectType 487 = OT_Gate
normaliseObjectType 488 = OT_Gate
normaliseObjectType 490 = OT_Gate
normaliseObjectType 491 = OT_Gate
normaliseObjectType 492 = OT_Arbalest
normaliseObjectType 493 = OT_AdvancedHeavyCrossbowman
normaliseObjectType 498 = OT_Barracks
normaliseObjectType 499 = OT_Torch
normaliseObjectType 501 = OT_Deadpikeman
normaliseObjectType 503 = OT_Arrow
normaliseObjectType 504 = OT_Arrow
normaliseObjectType 505 = OT_Arrow
normaliseObjectType 507 = OT_Arrow
normaliseObjectType 508 = OT_Arrow
normaliseObjectType 509 = OT_Arrow
normaliseObjectType 510 = OT_Arrow
normaliseObjectType 511 = OT_Arrow
normaliseObjectType 512 = OT_Arrow
normaliseObjectType 514 = OT_Arrow
normaliseObjectType 515 = OT_Arrow
normaliseObjectType 516 = OT_Arrow
normaliseObjectType 517 = OT_Arrow
normaliseObjectType 518 = OT_Arrow
normaliseObjectType 519 = OT_Arrow
normaliseObjectType 520 = OT_Arrow
normaliseObjectType 521 = OT_Arrow
normaliseObjectType 522 = OT_Arrow
normaliseObjectType 523 = OT_Arrow
normaliseObjectType 524 = OT_Arrow
normaliseObjectType 525 = OT_Arrow
normaliseObjectType 527 = OT_DemolitionShip
normaliseObjectType 528 = OT_HeavyDemolitionShip
normaliseObjectType 529 = OT_FireShip
normaliseObjectType 530 = OT_EliteLongbowman
normaliseObjectType 531 = OT_EliteThrowingAxeman
normaliseObjectType 532 = OT_FastFireShip
normaliseObjectType 533 = OT_EliteLongboat
normaliseObjectType 534 = OT_EliteWoadRaider
normaliseObjectType 539 = OT_Galley
normaliseObjectType 542 = OT_HeavyScorpion
normaliseObjectType 545 = OT_TransportShip
normaliseObjectType 546 = OT_LightCavalry
normaliseObjectType 547 = OT_Deadlightcavalry
normaliseObjectType 548 = OT_SiegeRam
normaliseObjectType 550 = OT_Onager
normaliseObjectType 553 = OT_EliteCataphract
normaliseObjectType 554 = OT_EliteTeutonicKnight
normaliseObjectType 555 = OT_EliteHuskarl
normaliseObjectType 556 = OT_EliteMameluke
normaliseObjectType 557 = OT_EliteJanissary
normaliseObjectType 558 = OT_EliteWarElephant
normaliseObjectType 559 = OT_EliteChuKoNu
normaliseObjectType 560 = OT_EliteSamurai
normaliseObjectType 561 = OT_EliteMangudai
normaliseObjectType 562 = OT_LumberCamp
normaliseObjectType 563 = OT_LumberCamp
normaliseObjectType 564 = OT_LumberCamp
normaliseObjectType 565 = OT_LumberCamp
normaliseObjectType 566 = OT_WatchTower
normaliseObjectType 567 = OT_Champion
normaliseObjectType 569 = OT_Paladin
normaliseObjectType 579 = OT_GoldMiner
normaliseObjectType 581 = OT_GoldMiner
normaliseObjectType 583 = OT_Genitour
normaliseObjectType 584 = OT_MiningCamp
normaliseObjectType 585 = OT_MiningCamp
normaliseObjectType 586 = OT_MiningCamp
normaliseObjectType 587 = OT_MiningCamp
normaliseObjectType 588 = OT_SiegeOnager
normaliseObjectType 590 = OT_Shepherd
normaliseObjectType 592 = OT_Shepherd
normaliseObjectType 594 = OT_Sheep
normaliseObjectType 596 = OT_EliteGenitour
normaliseObjectType 597 = OT_TownCenter
normaliseObjectType 598 = OT_Outpost
normaliseObjectType 599 = OT_Cathedral
normaliseObjectType 600 = OT_FlagA
normaliseObjectType 601 = OT_FlagB
normaliseObjectType 602 = OT_FlagC
normaliseObjectType 603 = OT_FlagD
normaliseObjectType 604 = OT_FlagE
normaliseObjectType 605 = OT_BridgeA_Top
normaliseObjectType 606 = OT_BridgeA_Middle
normaliseObjectType 607 = OT_BridgeA_Bottom
normaliseObjectType 608 = OT_BridgeB_Top
normaliseObjectType 609 = OT_BridgeB_Middle
normaliseObjectType 610 = OT_BridgeB_Bottom
normaliseObjectType 611 = OT_TownCenter
normaliseObjectType 612 = OT_TownCenter
normaliseObjectType 613 = OT_TownCenter
normaliseObjectType 614 = OT_TownCenter
normaliseObjectType 615 = OT_TownCenter
normaliseObjectType 616 = OT_TownCenter
normaliseObjectType 617 = OT_TownCenter
normaliseObjectType 618 = OT_TownCenter
normaliseObjectType 619 = OT_TownCenter
normaliseObjectType 620 = OT_TownCenter
normaliseObjectType 621 = OT_TownCenter
normaliseObjectType 623 = OT_Rock1
normaliseObjectType 624 = OT_Pavilion
normaliseObjectType 625 = OT_Pavilion
normaliseObjectType 626 = OT_Pavilion
normaliseObjectType 629 = OT_JoanofArc
normaliseObjectType 632 = OT_FrankishPaladin
normaliseObjectType 633 = OT_Cuauhtemoc
normaliseObjectType 634 = OT_SieurdeMetz
normaliseObjectType 636 = OT_SieurBertrand
normaliseObjectType 637 = OT_TempleofHeaven
normaliseObjectType 638 = OT_DukeDAlencon
normaliseObjectType 639 = OT_Penguin
normaliseObjectType 640 = OT_LaHire
normaliseObjectType 642 = OT_LorddeGraville
normaliseObjectType 644 = OT_JeandeLorrain
normaliseObjectType 646 = OT_ConstableRichemont
normaliseObjectType 648 = OT_GuyJosselyne
normaliseObjectType 650 = OT_JeanBureau
normaliseObjectType 652 = OT_SirJohnFastolf
normaliseObjectType 655 = OT_Mosque
normaliseObjectType 659 = OT_Gate
normaliseObjectType 660 = OT_Gate
normaliseObjectType 661 = OT_Gate
normaliseObjectType 662 = OT_Gate
normaliseObjectType 663 = OT_Gate
normaliseObjectType 664 = OT_Gate
normaliseObjectType 665 = OT_Gate
normaliseObjectType 666 = OT_Gate
normaliseObjectType 667 = OT_Gate
normaliseObjectType 668 = OT_Gate
normaliseObjectType 669 = OT_Gate
normaliseObjectType 670 = OT_Gate
normaliseObjectType 671 = OT_Gate
normaliseObjectType 672 = OT_Gate
normaliseObjectType 673 = OT_Gate
normaliseObjectType 674 = OT_Gate
normaliseObjectType 678 = OT_ReynalddeChatillon
normaliseObjectType 680 = OT_MasteroftheTemplar
normaliseObjectType 682 = OT_BadNeighbor
normaliseObjectType 683 = OT_GodsOwnSling
normaliseObjectType 684 = OT_TheAccursedTower
normaliseObjectType 685 = OT_TheTowerofFlies
normaliseObjectType 686 = OT_ArchersoftheEyes
normaliseObjectType 688 = OT_PieceoftheTrueCross
normaliseObjectType 689 = OT_Pyramid
normaliseObjectType 690 = OT_DomeoftheRock
normaliseObjectType 691 = OT_EliteCannonGalleon
normaliseObjectType 692 = OT_Berserk
normaliseObjectType 694 = OT_EliteBerserk
normaliseObjectType 696 = OT_GreatPyramid
normaliseObjectType 698 = OT_Subotai
normaliseObjectType 700 = OT_HuntingWolf
normaliseObjectType 702 = OT_Kushluk
normaliseObjectType 704 = OT_Shah
normaliseObjectType 705 = OT_Cow
normaliseObjectType 706 = OT_Saboteur
normaliseObjectType 707 = OT_OrnlutheWolf
normaliseObjectType 709 = OT_Cactus
normaliseObjectType 710 = OT_Skeleton
normaliseObjectType 711 = OT_Rugs
normaliseObjectType 712 = OT_Yurt
normaliseObjectType 713 = OT_Yurt
normaliseObjectType 714 = OT_Yurt
normaliseObjectType 715 = OT_Yurt
normaliseObjectType 716 = OT_Yurt
normaliseObjectType 717 = OT_Yurt
normaliseObjectType 718 = OT_Yurt
normaliseObjectType 719 = OT_Yurt
normaliseObjectType 720 = OT_NineBands
normaliseObjectType 721 = OT_Shipwreck
normaliseObjectType 722 = OT_Shipwreck
normaliseObjectType 723 = OT_Crater
normaliseObjectType 725 = OT_JaguarWarrior
normaliseObjectType 726 = OT_EliteJaguarWarrior
normaliseObjectType 728 = OT_Ice
normaliseObjectType 729 = OT_GodsOwnSlingPacked
normaliseObjectType 730 = OT_BadNeighborPacked
normaliseObjectType 731 = OT_GenghisKhan
normaliseObjectType 733 = OT_EmperorinaBarrel
normaliseObjectType 737 = OT_BambooStump
normaliseObjectType 738 = OT_BridgeA_Cracked
normaliseObjectType 739 = OT_BridgeA_BrokenTop
normaliseObjectType 740 = OT_BridgeA_BrokenBottom
normaliseObjectType 741 = OT_BridgeB_Cracked
normaliseObjectType 742 = OT_BridgeB_BrokenTop
normaliseObjectType 743 = OT_BridgeB_BrokenBottom
normaliseObjectType 744 = OT_Mountain3
normaliseObjectType 745 = OT_Mountain4
normaliseObjectType 748 = OT_CobraCar
normaliseObjectType 751 = OT_EagleScout
normaliseObjectType 752 = OT_EliteEagleWarrior
normaliseObjectType 753 = OT_EagleWarrior
normaliseObjectType 755 = OT_Tarkan
normaliseObjectType 757 = OT_EliteTarkan
normaliseObjectType 758 = OT_Burnedbuilding
normaliseObjectType 759 = OT_Huskarl
normaliseObjectType 761 = OT_EliteHuskarl
normaliseObjectType 763 = OT_PlumedArcher
normaliseObjectType 765 = OT_ElitePlumedArcher
normaliseObjectType 771 = OT_Conquistador
normaliseObjectType 773 = OT_EliteConquistador
normaliseObjectType 775 = OT_Missionary
normaliseObjectType 777 = OT_AttilatheHun
normaliseObjectType 778 = OT_Canoe
normaliseObjectType 779 = OT_BledatheHun
normaliseObjectType 781 = OT_PopeLeoI
normaliseObjectType 783 = OT_ScythianWildWoman
normaliseObjectType 785 = OT_SeaTower
normaliseObjectType 788 = OT_SeaWall
normaliseObjectType 789 = OT_PalisadeGate
normaliseObjectType 790 = OT_PalisadeGate
normaliseObjectType 791 = OT_PalisadeGate
normaliseObjectType 792 = OT_PalisadeGate
normaliseObjectType 793 = OT_PalisadeGate
normaliseObjectType 794 = OT_PalisadeGate
normaliseObjectType 795 = OT_PalisadeGate
normaliseObjectType 796 = OT_PalisadeGate
normaliseObjectType 797 = OT_PalisadeGate
normaliseObjectType 798 = OT_PalisadeGate
normaliseObjectType 799 = OT_PalisadeGate
normaliseObjectType 800 = OT_PalisadeGate
normaliseObjectType 801 = OT_PalisadeGate
normaliseObjectType 802 = OT_PalisadeGate
normaliseObjectType 803 = OT_PalisadeGate
normaliseObjectType 804 = OT_PalisadeGate
normaliseObjectType 805 = OT_Dock
normaliseObjectType 806 = OT_Dock
normaliseObjectType 807 = OT_Dock
normaliseObjectType 808 = OT_Dock
normaliseObjectType 809 = OT_Stump
normaliseObjectType 810 = OT_IronBoar
normaliseObjectType 812 = OT_Jaguar
normaliseObjectType 814 = OT_Horse
normaliseObjectType 816 = OT_Macaw
normaliseObjectType 817 = OT_Statue
normaliseObjectType 818 = OT_Plant
normaliseObjectType 819 = OT_Sign
normaliseObjectType 820 = OT_Grave
normaliseObjectType 821 = OT_Head
normaliseObjectType 822 = OT_Javelina
normaliseObjectType 824 = OT_ElCidCampeador
normaliseObjectType 825 = OT_AmazonWarrior
normaliseObjectType 826 = OT_Monument
normaliseObjectType 827 = OT_WarWagon
normaliseObjectType 829 = OT_EliteWarWagon
normaliseObjectType 831 = OT_TurtleShip
normaliseObjectType 832 = OT_EliteTurtleShip
normaliseObjectType 833 = OT_Turkey
normaliseObjectType 835 = OT_WildHorse
normaliseObjectType 837 = OT_MapRevealer
normaliseObjectType 838 = OT_KingSancho
normaliseObjectType 839 = OT_RockStone
normaliseObjectType 840 = OT_KingAlfonso
normaliseObjectType 841 = OT_RockGold
normaliseObjectType 842 = OT_Imam
normaliseObjectType 844 = OT_AdmiralYiSunshin
normaliseObjectType 845 = OT_Nobunaga
normaliseObjectType 846 = OT_Donkey
normaliseObjectType 847 = OT_HenryV
normaliseObjectType 849 = OT_WilliamtheConqueror
normaliseObjectType 850 = OT_AmazonArcher
normaliseObjectType 851 = OT_ESFlag
normaliseObjectType 852 = OT_ScythianScout
normaliseObjectType 853 = OT_TorchConverting
normaliseObjectType 854 = OT_TorchConverting
normaliseObjectType 855 = OT_OldStoneHead
normaliseObjectType 856 = OT_RomanRuins
normaliseObjectType 857 = OT_HayStack
normaliseObjectType 858 = OT_BrokenCart
normaliseObjectType 859 = OT_FlowerBed
normaliseObjectType 860 = OT_FurioustheMonkeyBoy
normaliseObjectType 862 = OT_StormyDog
normaliseObjectType 863 = OT_Rubble1x1
normaliseObjectType 864 = OT_Rubble2x2
normaliseObjectType 865 = OT_Rubble3x3
normaliseObjectType 866 = OT_GenoeseCrossbowman
normaliseObjectType 868 = OT_EliteGenoeseCrossbowman
normaliseObjectType 869 = OT_MagyarHuszar
normaliseObjectType 871 = OT_EliteMagyarHuszar
normaliseObjectType 872 = OT_QuimperCathedral
normaliseObjectType 873 = OT_ElephantArcher
normaliseObjectType 875 = OT_EliteElephantArcher
normaliseObjectType 876 = OT_Boyar
normaliseObjectType 878 = OT_EliteBoyar
normaliseObjectType 879 = OT_Kamayuk
normaliseObjectType 881 = OT_EliteKamayuk
normaliseObjectType 882 = OT_Condottiero
normaliseObjectType 884 = OT_WildCamel
normaliseObjectType 885 = OT_SiegeTower
normaliseObjectType 886 = OT_Tarkan
normaliseObjectType 887 = OT_EliteTarkan
normaliseObjectType 892 = OT_HeavyPikeman
normaliseObjectType 894 = OT_EasternSwordsman
normaliseObjectType 896 = OT_Waterfall
normaliseObjectType 897 = OT_CamelGaia
normaliseObjectType 899 = OT_ArchofConstantine
normaliseObjectType 900 = OT_Rain
normaliseObjectType 901 = OT_FlagF
normaliseObjectType 902 = OT_Smoke
normaliseObjectType 904 = OT_WoodenBridgeA_Top
normaliseObjectType 905 = OT_WoodenBridgeA_Middle
normaliseObjectType 906 = OT_WoodenBridgeA_Bottom
normaliseObjectType 907 = OT_WoodenBridgeB_Top
normaliseObjectType 908 = OT_WoodenBridgeB_Middle
normaliseObjectType 909 = OT_WoodenBridgeB_Bottom
normaliseObjectType 910 = OT_ImpaledCorpse
normaliseObjectType 914 = OT_Quarry
normaliseObjectType 915 = OT_Lumber
normaliseObjectType 916 = OT_Goods
normaliseObjectType 917 = OT_Vulture
normaliseObjectType 918 = OT_Rock2
normaliseObjectType 922 = OT_MonkwithRelic
normaliseObjectType 923 = OT_Queen
normaliseObjectType 925 = OT_Sanyogita
normaliseObjectType 926 = OT_Prithvi
normaliseObjectType 927 = OT_ChandBhai
normaliseObjectType 929 = OT_Saladin
normaliseObjectType 930 = OT_Khosrau
normaliseObjectType 931 = OT_Jarl
normaliseObjectType 932 = OT_Savaran
normaliseObjectType 933 = OT_Barrels
normaliseObjectType 934 = OT_AlfredtheAlpaca
normaliseObjectType 936 = OT_Elephant
normaliseObjectType 938 = OT_DragonShip
normaliseObjectType 939 = OT_Flame1
normaliseObjectType 940 = OT_Flame2
normaliseObjectType 941 = OT_Flame3
normaliseObjectType 942 = OT_Flame4
normaliseObjectType 943 = OT_Osman
normaliseObjectType 944 = OT_RelicCart
normaliseObjectType 1001 = OT_OrganGun
normaliseObjectType 1003 = OT_EliteOrganGun
normaliseObjectType 1004 = OT_Caravel
normaliseObjectType 1006 = OT_EliteCaravel
normaliseObjectType 1007 = OT_CamelArcher
normaliseObjectType 1009 = OT_EliteCamelArcher
normaliseObjectType 1010 = OT_Genitour
normaliseObjectType 1012 = OT_EliteGenitour
normaliseObjectType 1013 = OT_Gbeto
normaliseObjectType 1015 = OT_EliteGbeto
normaliseObjectType 1016 = OT_ShotelWarrior
normaliseObjectType 1018 = OT_EliteShotelWarrior
normaliseObjectType 1019 = OT_Zebra
normaliseObjectType 1021 = OT_Feitoria
normaliseObjectType 1023 = OT_Priest
normaliseObjectType 1025 = OT_MonkwithRelic
normaliseObjectType 1026 = OT_Ostrich
normaliseObjectType 1028 = OT_Stork
normaliseObjectType 1029 = OT_Lion
normaliseObjectType 1031 = OT_Crocodile
normaliseObjectType 1033 = OT_SavannahGrassPatch
normaliseObjectType 1034 = OT_MusaibnNusayr
normaliseObjectType 1035 = OT_Sundjata
normaliseObjectType 1036 = OT_TariqibnZiyad
normaliseObjectType 1037 = OT_RicharddeClare
normaliseObjectType 1038 = OT_Tristan
normaliseObjectType 1039 = OT_PrincessYodit
normaliseObjectType 1040 = OT_HenryII
normaliseObjectType 1041 = OT_Mountain5
normaliseObjectType 1042 = OT_Mountain6
normaliseObjectType 1043 = OT_Mountain7
normaliseObjectType 1044 = OT_Mountain8
normaliseObjectType 1045 = OT_SnowMountain1
normaliseObjectType 1046 = OT_SnowMountain2
normaliseObjectType 1047 = OT_SnowMountain3
normaliseObjectType 1048 = OT_RockFormation1
normaliseObjectType 1049 = OT_RockFormation2
normaliseObjectType 1050 = OT_RockFormation3
normaliseObjectType 1051 = OT_DragonTree
normaliseObjectType 1052 = OT_BaobabTree
normaliseObjectType 1053 = OT_Bush2
normaliseObjectType 1054 = OT_Bush3
normaliseObjectType 1055 = OT_Arrow
normaliseObjectType 1056 = OT_Falcon
normaliseObjectType 1059 = OT_FruitBush
normaliseObjectType 1060 = OT_Goat
normaliseObjectType 1062 = OT_Fence
normaliseObjectType 1063 = OT_AcaciaTree
normaliseObjectType 1064 = OT_YekunoAmlak
normaliseObjectType 1065 = OT_Rubble1x1
normaliseObjectType 1066 = OT_Yodit
normaliseObjectType 1067 = OT_Itzcoatl
normaliseObjectType 1068 = OT_MustafaPasha
normaliseObjectType 1069 = OT_PacalII
normaliseObjectType 1070 = OT_Babur
normaliseObjectType 1071 = OT_AbrahaElephant
normaliseObjectType 1072 = OT_GuglielmoEmbriaco
normaliseObjectType 1073 = OT_SuDingfang
normaliseObjectType 1074 = OT_Pachacuti
normaliseObjectType 1075 = OT_HuaynaCapac
normaliseObjectType 1076 = OT_MiklosToldi
normaliseObjectType 1077 = OT_LittleJohn
normaliseObjectType 1078 = OT_ZawiszatheBlack
normaliseObjectType 1080 = OT_Sumanguru
normaliseObjectType 1081 = OT_Storage
normaliseObjectType 1082 = OT_Hut
normaliseObjectType 1083 = OT_Hut
normaliseObjectType 1084 = OT_Hut
normaliseObjectType 1085 = OT_Hut
normaliseObjectType 1086 = OT_Hut
normaliseObjectType 1087 = OT_Hut
normaliseObjectType 1088 = OT_Hut
normaliseObjectType 1089 = OT_Granary
normaliseObjectType 1090 = OT_Barricade
normaliseObjectType 1091 = OT_Animalskeleton
normaliseObjectType 1092 = OT_StelaeA
normaliseObjectType 1093 = OT_StelaeB
normaliseObjectType 1094 = OT_StelaeC
normaliseObjectType 1095 = OT_Gallow
normaliseObjectType 1096 = OT_Palace
normaliseObjectType 1097 = OT_Tent
normaliseObjectType 1098 = OT_Tent
normaliseObjectType 1099 = OT_Tent
normaliseObjectType 1100 = OT_Tent
normaliseObjectType 1101 = OT_Tent
normaliseObjectType 1102 = OT_SeaFortification
normaliseObjectType 1103 = OT_FireGalley
normaliseObjectType 1104 = OT_DemolitionRaft
normaliseObjectType 1105 = OT_SiegeTower
normaliseObjectType 1106 = OT_Dagnajan
normaliseObjectType 1109 = OT_Gidajan
normaliseObjectType 1120 = OT_BallistaElephant
normaliseObjectType 1122 = OT_EliteBallistaElephant
normaliseObjectType 1123 = OT_KarambitWarrior
normaliseObjectType 1125 = OT_EliteKarambitWarrior
normaliseObjectType 1126 = OT_Arambai
normaliseObjectType 1128 = OT_EliteArambai
normaliseObjectType 1129 = OT_RattanArcher
normaliseObjectType 1131 = OT_EliteRattanArcher
normaliseObjectType 1132 = OT_BattleElephant
normaliseObjectType 1134 = OT_EliteBattleElephant
normaliseObjectType 1135 = OT_KomodoDragon
normaliseObjectType 1137 = OT_Tiger
normaliseObjectType 1139 = OT_Rhinoceros
normaliseObjectType 1141 = OT_BoxTurtles
normaliseObjectType 1142 = OT_WaterBuffalo
normaliseObjectType 1144 = OT_MangroveTree
normaliseObjectType 1146 = OT_RainforestTree
normaliseObjectType 1148 = OT_RockBeach
normaliseObjectType 1149 = OT_RockJungle
normaliseObjectType 1150 = OT_FlagG
normaliseObjectType 1151 = OT_FlagH
normaliseObjectType 1152 = OT_FlagI
normaliseObjectType 1153 = OT_FlagJ
normaliseObjectType 1155 = OT_ImperialSkirmisher
normaliseObjectType 1157 = OT_GajahMada
normaliseObjectType 1158 = OT_Jayanegara
normaliseObjectType 1159 = OT_RadenWijaya
normaliseObjectType 1160 = OT_SundaRoyalFighter
normaliseObjectType 1162 = OT_SuryavarmanI
normaliseObjectType 1163 = OT_UdayadityavarmanI
normaliseObjectType 1164 = OT_Jayaviravarman
normaliseObjectType 1165 = OT_Bayinnaung
normaliseObjectType 1166 = OT_Tabinshwehti
normaliseObjectType 1169 = OT_Arrow
normaliseObjectType 1170 = OT_Arrow
normaliseObjectType 1171 = OT_BuddhaStatueA
normaliseObjectType 1172 = OT_BuddhaStatueB
normaliseObjectType 1173 = OT_BuddhaStatueC
normaliseObjectType 1174 = OT_BuddhaStatueD
normaliseObjectType 1175 = OT_FernPatch
normaliseObjectType 1176 = OT_TrowulanGate
normaliseObjectType 1177 = OT_Vases
normaliseObjectType 1178 = OT_LeLoi
normaliseObjectType 1179 = OT_LeLai
normaliseObjectType 1180 = OT_LeLai
normaliseObjectType 1181 = OT_LeTrien
normaliseObjectType 1182 = OT_LuuNhanChu
normaliseObjectType 1183 = OT_BuiBi
normaliseObjectType 1184 = OT_DinhLe
normaliseObjectType 1185 = OT_WangTong
normaliseObjectType 1186 = OT_Envoy
normaliseObjectType 1187 = OT_RiceFarm
normaliseObjectType 1188 = OT_DeadRiceFarm
normaliseObjectType 1189 = OT_Harbor
normaliseObjectType 1191 = OT_Stupa
normaliseObjectType 1192 = OT_Farmer
normaliseObjectType 1196 = OT_ArmyTent
normaliseObjectType 1197 = OT_ArmyTent
normaliseObjectType 1198 = OT_ArmyTent
normaliseObjectType 1199 = OT_ArmyTent
normaliseObjectType 1200 = OT_ArmyTent
normaliseObjectType 1201 = OT_Pagoda
normaliseObjectType 1202 = OT_Pagoda
normaliseObjectType 1203 = OT_Pagoda
normaliseObjectType 1204 = OT_BridgeC_Top
normaliseObjectType 1205 = OT_BridgeC_Middle
normaliseObjectType 1206 = OT_BridgeC_Bottom
normaliseObjectType 1207 = OT_BridgeD_Top
normaliseObjectType 1208 = OT_BridgeD_Middle
normaliseObjectType 1209 = OT_BridgeD_Bottom
normaliseObjectType 1210 = OT_BridgeC_Cracked
normaliseObjectType 1211 = OT_BridgeC_BrokenTop
normaliseObjectType 1212 = OT_BridgeC_BrokenBottom
normaliseObjectType 1213 = OT_BridgeD_Cracked
normaliseObjectType 1214 = OT_BridgeD_BrokenTop
normaliseObjectType 1215 = OT_BridgeD_BrokenBottom
normaliseObjectType 1216 = OT_SanchiStupa
normaliseObjectType 1217 = OT_GolGumbaz
normaliseObjectType 1218 = OT_Barricade
normaliseObjectType 1219 = OT_Barricade
normaliseObjectType 1220 = OT_Barricade
normaliseObjectType 1222 = OT_Sharkatzor
normaliseObjectType 1223 = OT_Arrow
normaliseObjectType 106 = OT_OrganGun
normaliseObjectType 114 = OT_EliteOrganGun
normaliseObjectType 162 = OT_Caravel
normaliseObjectType 183 = OT_EliteCaravel
normaliseObjectType 203 = OT_CamelArcher
normaliseObjectType 208 = OT_EliteCamelArcher
normaliseObjectType 223 = OT_Genitour
normaliseObjectType 230 = OT_EliteGenitour
normaliseObjectType 260 = OT_Gbeto
normaliseObjectType 418 = OT_EliteGbeto
normaliseObjectType 453 = OT_ShotelWarrior
normaliseObjectType 459 = OT_EliteShotelWarrior
normaliseObjectType 467 = OT_FireShip
normaliseObjectType 494 = OT_SiegeTower
normaliseObjectType 653 = OT_DemolitionShip
normaliseObjectType 732 = OT_Genitour
normaliseObjectType 734 = OT_Feitoria
normaliseObjectType 760 = OT_BallistaElephant
normaliseObjectType 762 = OT_ImperialSkirmisher
normaliseObjectType 766 = OT_EliteBattleElephant
normaliseObjectType 774 = OT_BattleElephant
normaliseObjectType 782 = OT_EliteRattanArcher
normaliseObjectType 784 = OT_RattanArcher
normaliseObjectType 811 = OT_EliteArambai
normaliseObjectType 823 = OT_Arambai
normaliseObjectType 830 = OT_EliteKarambit
normaliseObjectType 836 = OT_Karambit
normaliseObjectType 891 = OT_EliteBallistaElephant
normaliseObjectType n = OT_Unknown n
