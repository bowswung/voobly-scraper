module Data.Mgz.Constants.Techs where
-- borrowed from https://github.com/happyleavesaoc/aoc-mgz/blob/master/mgz/const.py

import RIO

data Tech =
    Tech_EliteTarkan
  | Tech_Yeomen
  | Tech_ElDorado
  | Tech_FurorCeltica
  | Tech_Drill
  | Tech_Mahouts
  | Tech_TownWatch
  | Tech_Zealotry
  | Tech_Artillery
  | Tech_Crenellations
  | Tech_CropRotation
  | Tech_HeavyPlow
  | Tech_HorseCollar
  | Tech_Guilds
  | Tech_Anarchy
  | Tech_Banking
  | Tech_Cartography
  | Tech_Atheism
  | Tech_Loom
  | Tech_Coinage
  | Tech_GarlandWars
  | Tech_ElitePlumedArcher
  | Tech_WarGalley
  | Tech_Galleon
  | Tech_CannonGalleon
  | Tech_Husbandry
  | Tech_Faith
  | Tech_Chemistry
  | Tech_Caravan
  | Tech_Berserkergang
  | Tech_Masonry
  | Tech_Architecture
  | Tech_Rocketry
  | Tech_TreadmillCrane
  | Tech_GoldMining
  | Tech_Kataparuto
  | Tech_EliteConquistador
  | Tech_Logistica
  | Tech_Keep
  | Tech_BombardTower
  | Tech_Gillnets
  | Tech_Forging
  | Tech_IronCasting
  | Tech_ScaleMailArmor
  | Tech_BlastFurnace
  | Tech_ChainMailArmor
  | Tech_PlateMailArmor
  | Tech_PlateBardingArmor
  | Tech_ScaleBardingArmor
  | Tech_ChainBardingArmor
  | Tech_BeardedAxe
  | Tech_HandCannon
  | Tech_Tracking
  | Tech_Ballistics
  | Tech_Scorpion
  | Tech_CappedRam
  | Tech_EliteSkirmisher
  | Tech_Crossbowman
  | Tech_FeudalAge
  | Tech_CastleAge
  | Tech_ImperialAge
  | Tech_DarkAge
  | Tech_GuardTower
  | Tech_GoldShaftMining
  | Tech_BombardCannon
  | Tech_FortifiedWall
  | Tech_Pikeman
  | Tech_Fletching
  | Tech_BodkinArrow
  | Tech_Bracer
  | Tech_DoubleBitAxe
  | Tech_BowSaw
  | Tech_LongSwordsman
  | Tech_Cavalier
  | Tech_PaddedArcherArmor
  | Tech_LeatherArcherArmor
  | Tech_Wheelbarrow
  | Tech_Squires
  | Tech_TwoHandedSwordsman
  | Tech_HeavyCavArcher
  | Tech_RingArcherArmor
  | Tech_TwoManSaw
  | Tech_ManAtArms
  | Tech_BlockPrinting
  | Tech_Sanctity
  | Tech_Illumination
  | Tech_HeavyCamel
  | Tech_Arbalest
  | Tech_HeavyScorpion
  | Tech_HeavyDemolitionShip
  | Tech_FastFireShip
  | Tech_HandCart
  | Tech_Fervor
  | Tech_LightCavalry
  | Tech_SiegeRam
  | Tech_Onager
  | Tech_Champion
  | Tech_Paladin
  | Tech_StoneMining
  | Tech_StoneShaftMining
  | Tech_TownPatrol
  | Tech_TuskSwords
  | Tech_DoubleCrossbow
  | Tech_ForcedLevy
  | Tech_PaperMoney
  | Tech_Conscription
  | Tech_Redemption
  | Tech_Atonement
  | Tech_SiegeOnager
  | Tech_Sappers
  | Tech_MurderHoles
  | Tech_EliteLongbowman
  | Tech_EliteCataphract
  | Tech_EliteChuKoNu
  | Tech_EliteThrowingAxeman
  | Tech_EliteTeutonicKnight
  | Tech_EliteHuskarl
  | Tech_EliteSamurai
  | Tech_EliteWarElephant
  | Tech_EliteMameluke
  | Tech_EliteJanissary
  | Tech_EliteWoadRaider
  | Tech_EliteMangudai
  | Tech_EliteLongboat
  | Tech_Shipwright
  | Tech_Careening
  | Tech_DryDock
  | Tech_EliteCannonGalleon
  | Tech_SiegeEngineers
  | Tech_Hoardings
  | Tech_HeatedShot
  | Tech_EagleWarrior
  | Tech_EliteBerserk
  | Tech_SpiesTreason
  | Tech_Hussar
  | Tech_Halberdier
  | Tech_EliteJaguarWarrior
  | Tech_EliteEagleWarrior
  | Tech_Bloodlines
  | Tech_ParthianTactics
  | Tech_ThumbRing
  | Tech_Theocracy
  | Tech_Heresy
  | Tech_Supremacy
  | Tech_HerbalMedicine
  | Tech_Shinkichon
  | Tech_EliteTurtleShip
  | Tech_EliteWarWagon
  | Tech_Perfusion
  | Tech_Atlatl
  | Tech_Warwolf
  | Tech_GreatWall
  | Tech_Chieftains
  | Tech_GreekFire
  | Tech_EliteGenoeseCrossbowman
  | Tech_EliteMagyarHuszar
  | Tech_EliteElephantArcher
  | Tech_Stronghold
  | Tech_Marauders
  | Tech_Yasama
  | Tech_ObsidianArrows
  | Tech_Panokseon
  | Tech_Nomads
  | Tech_BoilingOil
  | Tech_Ironclad
  | Tech_Madrasah
  | Tech_Sipahi
  | Tech_Inquisition
  | Tech_Chivalry
  | Tech_Pavise
  | Tech_SilkRoad
  | Tech_EliteBoyar
  | Tech_Sultans
  | Tech_Shatagni
  | Tech_EliteKamayuk
  | Tech_Orthodoxy
  | Tech_Druzhina
  | Tech_Mercenaries
  | Tech_RecurveBow
  | Tech_AndeanSling
  | Tech_Couriers
  | Tech_ImperialCamel
  | Tech_Revetments
  | Tech_HuntingDogs
  | Tech_FireTower
  | Tech_Britons
  | Tech_Franks
  | Tech_Goths
  | Tech_Teutons
  | Tech_Japanese
  | Tech_Chinese
  | Tech_Byzantines
  | Tech_Persians
  | Tech_Saracens
  | Tech_Turks
  | Tech_Vikings
  | Tech_Mongols
  | Tech_Celts
  | Tech_Spanish
  | Tech_Aztecs
  | Tech_Mayans
  | Tech_Huns
  | Tech_Koreans
  | Tech_Italians
  | Tech_Indians
  | Tech_Incas
  | Tech_Magyars
  | Tech_Slavs
  | Tech_EnableSheep
  | Tech_EnableLlamas
  | Tech_EnableCows
  | Tech_EnableTurkeys
  | Tech_EliteOrganGun
  | Tech_EliteCamelArcher
  | Tech_EliteGbeto
  | Tech_EliteShotelWarrior
  | Tech_Carrack
  | Tech_Arquebus
  | Tech_RoyalHeirs
  | Tech_TorsionEngines
  | Tech_Tigui
  | Tech_Farimba
  | Tech_Kasbah
  | Tech_MaghrabiCamels
  | Tech_Portuguese
  | Tech_Ethiopians
  | Tech_Malians
  | Tech_Berbers
  | Tech_EliteCaravel
  | Tech_EliteGenitour
  | Tech_FreeCartography
  | Tech_Arson
  | Tech_Arrowslits
  | Tech_EliteBallistaElephant
  | Tech_EliteKarambitWarrior
  | Tech_EliteArambai
  | Tech_EliteRattanArcher
  | Tech_Thalassocracy
  | Tech_Howdah
  | Tech_ManipurCavalry
  | Tech_Chatras
  | Tech_EliteBattleElephant
  | Tech_Khmer
  | Tech_Malay
  | Tech_Burmese
  | Tech_Vietnamese
  | Tech_ImperialSkirmisher
  | Tech_SetMaximumPopulationNoHouses
  | Tech_DisableVietnameseVision
  | Tech_Unknown Int
  deriving (Show, Eq, Ord, Generic)
instance Hashable Tech

normalizeTech :: Int -> Tech
normalizeTech 2 = Tech_EliteTarkan
normalizeTech 3 = Tech_Yeomen
normalizeTech 4 = Tech_ElDorado
normalizeTech 5 = Tech_FurorCeltica
normalizeTech 6 = Tech_Drill
normalizeTech 7 = Tech_Mahouts
normalizeTech 8 = Tech_TownWatch
normalizeTech 9 = Tech_Zealotry
normalizeTech 10 = Tech_Artillery
normalizeTech 11 = Tech_Crenellations
normalizeTech 12 = Tech_CropRotation
normalizeTech 13 = Tech_HeavyPlow
normalizeTech 14 = Tech_HorseCollar
normalizeTech 15 = Tech_Guilds
normalizeTech 16 = Tech_Anarchy
normalizeTech 17 = Tech_Banking
normalizeTech 19 = Tech_Cartography
normalizeTech 21 = Tech_Atheism
normalizeTech 22 = Tech_Loom
normalizeTech 23 = Tech_Coinage
normalizeTech 24 = Tech_GarlandWars
normalizeTech 27 = Tech_ElitePlumedArcher
normalizeTech 34 = Tech_WarGalley
normalizeTech 35 = Tech_Galleon
normalizeTech 37 = Tech_CannonGalleon
normalizeTech 39 = Tech_Husbandry
normalizeTech 45 = Tech_Faith
normalizeTech 47 = Tech_Chemistry
normalizeTech 48 = Tech_Caravan
normalizeTech 49 = Tech_Berserkergang
normalizeTech 50 = Tech_Masonry
normalizeTech 51 = Tech_Architecture
normalizeTech 52 = Tech_Rocketry
normalizeTech 54 = Tech_TreadmillCrane
normalizeTech 55 = Tech_GoldMining
normalizeTech 59 = Tech_Kataparuto
normalizeTech 60 = Tech_EliteConquistador
normalizeTech 61 = Tech_Logistica
normalizeTech 63 = Tech_Keep
normalizeTech 64 = Tech_BombardTower
normalizeTech 65 = Tech_Gillnets
normalizeTech 67 = Tech_Forging
normalizeTech 68 = Tech_IronCasting
normalizeTech 74 = Tech_ScaleMailArmor
normalizeTech 75 = Tech_BlastFurnace
normalizeTech 76 = Tech_ChainMailArmor
normalizeTech 77 = Tech_PlateMailArmor
normalizeTech 80 = Tech_PlateBardingArmor
normalizeTech 81 = Tech_ScaleBardingArmor
normalizeTech 82 = Tech_ChainBardingArmor
normalizeTech 83 = Tech_BeardedAxe
normalizeTech 85 = Tech_HandCannon
normalizeTech 90 = Tech_Tracking
normalizeTech 93 = Tech_Ballistics
normalizeTech 94 = Tech_Scorpion
normalizeTech 96 = Tech_CappedRam
normalizeTech 98 = Tech_EliteSkirmisher
normalizeTech 100 = Tech_Crossbowman
normalizeTech 101 = Tech_FeudalAge
normalizeTech 102 = Tech_CastleAge
normalizeTech 103 = Tech_ImperialAge
normalizeTech 104 = Tech_DarkAge
normalizeTech 140 = Tech_GuardTower
normalizeTech 182 = Tech_GoldShaftMining
normalizeTech 188 = Tech_BombardCannon
normalizeTech 194 = Tech_FortifiedWall
normalizeTech 197 = Tech_Pikeman
normalizeTech 199 = Tech_Fletching
normalizeTech 200 = Tech_BodkinArrow
normalizeTech 201 = Tech_Bracer
normalizeTech 202 = Tech_DoubleBitAxe
normalizeTech 203 = Tech_BowSaw
normalizeTech 207 = Tech_LongSwordsman
normalizeTech 209 = Tech_Cavalier
normalizeTech 211 = Tech_PaddedArcherArmor
normalizeTech 212 = Tech_LeatherArcherArmor
normalizeTech 213 = Tech_Wheelbarrow
normalizeTech 215 = Tech_Squires
normalizeTech 217 = Tech_TwoHandedSwordsman
normalizeTech 218 = Tech_HeavyCavArcher
normalizeTech 219 = Tech_RingArcherArmor
normalizeTech 221 = Tech_TwoManSaw
normalizeTech 222 = Tech_ManAtArms
normalizeTech 230 = Tech_BlockPrinting
normalizeTech 231 = Tech_Sanctity
normalizeTech 233 = Tech_Illumination
normalizeTech 236 = Tech_HeavyCamel
normalizeTech 237 = Tech_Arbalest
normalizeTech 239 = Tech_HeavyScorpion
normalizeTech 244 = Tech_HeavyDemolitionShip
normalizeTech 246 = Tech_FastFireShip
normalizeTech 249 = Tech_HandCart
normalizeTech 252 = Tech_Fervor
normalizeTech 254 = Tech_LightCavalry
normalizeTech 255 = Tech_SiegeRam
normalizeTech 257 = Tech_Onager
normalizeTech 261 = Tech_MaghrabiCamels
normalizeTech 262 = Tech_Farimba
normalizeTech 264 = Tech_Champion
normalizeTech 265 = Tech_Paladin
normalizeTech 278 = Tech_StoneMining
normalizeTech 279 = Tech_StoneShaftMining
normalizeTech 280 = Tech_TownPatrol
normalizeTech 285 = Tech_TuskSwords
normalizeTech 287 = Tech_DoubleCrossbow
normalizeTech 289 = Tech_ForcedLevy
normalizeTech 306 = Tech_PaperMoney
normalizeTech 314 = Tech_EliteRattanArcher
normalizeTech 315 = Tech_Conscription
normalizeTech 316 = Tech_Redemption
normalizeTech 319 = Tech_Atonement
normalizeTech 320 = Tech_SiegeOnager
normalizeTech 321 = Tech_Sappers
normalizeTech 322 = Tech_MurderHoles
normalizeTech 360 = Tech_EliteLongbowman
normalizeTech 361 = Tech_EliteCataphract
normalizeTech 362 = Tech_EliteChuKoNu
normalizeTech 363 = Tech_EliteThrowingAxeman
normalizeTech 364 = Tech_EliteTeutonicKnight
normalizeTech 365 = Tech_EliteHuskarl
normalizeTech 366 = Tech_EliteSamurai
normalizeTech 367 = Tech_EliteWarElephant
normalizeTech 368 = Tech_EliteMameluke
normalizeTech 369 = Tech_EliteJanissary
normalizeTech 370 = Tech_EliteWoadRaider
normalizeTech 371 = Tech_EliteMangudai
normalizeTech 372 = Tech_EliteLongboat
normalizeTech 373 = Tech_Shipwright
normalizeTech 374 = Tech_Careening
normalizeTech 375 = Tech_DryDock
normalizeTech 376 = Tech_EliteCannonGalleon
normalizeTech 377 = Tech_SiegeEngineers
normalizeTech 379 = Tech_Hoardings
normalizeTech 380 = Tech_HeatedShot
normalizeTech 384 = Tech_EagleWarrior
normalizeTech 398 = Tech_EliteBerserk
normalizeTech 408 = Tech_SpiesTreason
normalizeTech 428 = Tech_Hussar
normalizeTech 429 = Tech_Halberdier
normalizeTech 432 = Tech_EliteJaguarWarrior
normalizeTech 434 = Tech_EliteEagleWarrior
normalizeTech 435 = Tech_Bloodlines
normalizeTech 436 = Tech_ParthianTactics
normalizeTech 437 = Tech_ThumbRing
normalizeTech 438 = Tech_Theocracy
normalizeTech 439 = Tech_Heresy
normalizeTech 440 = Tech_Supremacy
normalizeTech 441 = Tech_HerbalMedicine
normalizeTech 445 = Tech_Shinkichon
normalizeTech 448 = Tech_EliteTurtleShip
normalizeTech 450 = Tech_EliteWarWagon
normalizeTech 457 = Tech_Perfusion
normalizeTech 460 = Tech_Atlatl
normalizeTech 461 = Tech_Warwolf
normalizeTech 462 = Tech_GreatWall
normalizeTech 463 = Tech_Chieftains
normalizeTech 464 = Tech_GreekFire
normalizeTech 468 = Tech_EliteGenoeseCrossbowman
normalizeTech 472 = Tech_EliteMagyarHuszar
normalizeTech 481 = Tech_EliteElephantArcher
normalizeTech 482 = Tech_Stronghold
normalizeTech 483 = Tech_Marauders
normalizeTech 484 = Tech_Yasama
normalizeTech 485 = Tech_ObsidianArrows
normalizeTech 486 = Tech_Panokseon
normalizeTech 487 = Tech_Nomads
normalizeTech 488 = Tech_BoilingOil
normalizeTech 489 = Tech_Ironclad
normalizeTech 490 = Tech_Madrasah
normalizeTech 491 = Tech_Sipahi
normalizeTech 492 = Tech_Inquisition
normalizeTech 493 = Tech_Chivalry
normalizeTech 494 = Tech_Pavise
normalizeTech 499 = Tech_SilkRoad
normalizeTech 504 = Tech_EliteBoyar
normalizeTech 506 = Tech_Sultans
normalizeTech 507 = Tech_Shatagni
normalizeTech 509 = Tech_EliteKamayuk
normalizeTech 512 = Tech_Orthodoxy
normalizeTech 513 = Tech_Druzhina
normalizeTech 514 = Tech_Mercenaries
normalizeTech 515 = Tech_RecurveBow
normalizeTech 516 = Tech_AndeanSling
normalizeTech 517 = Tech_Couriers
normalizeTech 521 = Tech_ImperialCamel
normalizeTech 525 = Tech_Revetments
normalizeTech 526 = Tech_HuntingDogs
normalizeTech 527 = Tech_FireTower
normalizeTech 529 = Tech_Britons
normalizeTech 530 = Tech_Franks
normalizeTech 531 = Tech_Goths
normalizeTech 532 = Tech_Teutons
normalizeTech 533 = Tech_Japanese
normalizeTech 534 = Tech_Chinese
normalizeTech 535 = Tech_Byzantines
normalizeTech 536 = Tech_Persians
normalizeTech 537 = Tech_Saracens
normalizeTech 538 = Tech_Turks
normalizeTech 539 = Tech_Vikings
normalizeTech 540 = Tech_Mongols
normalizeTech 541 = Tech_Celts
normalizeTech 542 = Tech_Spanish
normalizeTech 543 = Tech_Aztecs
normalizeTech 544 = Tech_Mayans
normalizeTech 545 = Tech_Huns
normalizeTech 546 = Tech_Koreans
normalizeTech 547 = Tech_Italians
normalizeTech 548 = Tech_Indians
normalizeTech 549 = Tech_Incas
normalizeTech 550 = Tech_Magyars
normalizeTech 551 = Tech_Slavs
normalizeTech 555 = Tech_EnableSheep
normalizeTech 556 = Tech_EnableLlamas
normalizeTech 557 = Tech_EnableCows
normalizeTech 558 = Tech_EnableTurkeys
normalizeTech 563 = Tech_EliteOrganGun
normalizeTech 565 = Tech_EliteCamelArcher
normalizeTech 567 = Tech_EliteGbeto
normalizeTech 569 = Tech_EliteShotelWarrior
normalizeTech 572 = Tech_Carrack
normalizeTech 573 = Tech_Arquebus
normalizeTech 574 = Tech_RoyalHeirs
normalizeTech 575 = Tech_TorsionEngines
normalizeTech 576 = Tech_Tigui
normalizeTech 577 = Tech_Farimba
normalizeTech 578 = Tech_Kasbah
normalizeTech 579 = Tech_MaghrabiCamels
normalizeTech 580 = Tech_Portuguese
normalizeTech 581 = Tech_Ethiopians
normalizeTech 582 = Tech_Malians
normalizeTech 583 = Tech_Berbers
normalizeTech 597 = Tech_EliteCaravel
normalizeTech 599 = Tech_EliteGenitour
normalizeTech 600 = Tech_FreeCartography
normalizeTech 602 = Tech_Arson
normalizeTech 608 = Tech_Arrowslits
normalizeTech 615 = Tech_EliteBallistaElephant
normalizeTech 617 = Tech_EliteKarambitWarrior
normalizeTech 619 = Tech_EliteArambai
normalizeTech 621 = Tech_EliteRattanArcher
normalizeTech 622 = Tech_TuskSwords
normalizeTech 623 = Tech_DoubleCrossbow
normalizeTech 624 = Tech_Thalassocracy
normalizeTech 625 = Tech_ForcedLevy
normalizeTech 626 = Tech_Howdah
normalizeTech 627 = Tech_ManipurCavalry
normalizeTech 628 = Tech_Chatras
normalizeTech 629 = Tech_PaperMoney
normalizeTech 631 = Tech_EliteBattleElephant
normalizeTech 650 = Tech_Khmer
normalizeTech 651 = Tech_Malay
normalizeTech 652 = Tech_Burmese
normalizeTech 653 = Tech_Vietnamese
normalizeTech 655 = Tech_ImperialSkirmisher
normalizeTech 658 = Tech_SetMaximumPopulationNoHouses
normalizeTech 665 = Tech_DisableVietnameseVision
normalizeTech n = Tech_Unknown n