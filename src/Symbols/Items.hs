{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Symbols.Items
  ( AdaptiveControlUnit
  , AIExpansionServer
  , AILimiter
  , AlcladAluminumSheet
  , AlienDNACapsule
  , AlienPowerMatrix
  , AlienProtein
  , AlienRemains
  , AluminumCasing
  , AluminumIngot
  , AluminumScrap
  , AssemblyDirectorSystem
  , AutomatedWiring
  , BaconAgaric
  , BallisticWarpDrive
  , Battery
  , Bauxite
  , Beacon
  , BerylNut
  , BiochemicalSculptor
  , Biomass
  , BlackPowder
  , BladeRunners
  , BoomBox
  , Cable
  , CateriumIngot
  , CateriumOre
  , Chainsaw
  , CircuitBoard
  , Coal
  , ColorCartridge
  , CompactedCoal
  , Computer
  , Concrete
  , CoolingSystem
  , CopperIngot
  , CopperOre
  , CopperPowder
  , CopperSheet
  , CrudeOil
  , CrystalOscillator
  , Cup
  , DarkMatterCrystal
  , Diamonds
  , ElectromagneticControlRod
  , EmptyCanister
  , EmptyFluidTank
  , EncasedIndustrialBeam
  , EncasedPlutoniumCell
  , EncasedUraniumCell
  , Fabric
  , FicsitCoupon
  , FicsiteIngot
  , FicsiteTrigon
  , Ficsonium
  , FicsoniumFuelRod
  , FlowerPetals
  , FusedModularFrame
  , GasMask
  , HardDrive
  , HazmatSuit
  , HeatSink
  , HeavyModularFrame
  , HighSpeedConnector
  , Hoverpack
  , HubParts
  , IronIngot
  , IronOre
  , IronPlate
  , IronRod
  , Jetpack
  , Leaves
  , Limestone
  , MagneticFieldGenerator
  , MedicinalInhaler
  , MercerSphere
  , Miner
  , ModularEngine
  , ModularFrame
  , Motor
  , Mycelia
  , NeuralQuantumProcessor
  , NobeliskDetonator
  , NonFissileUranium
  , NuclearPasta
  , ObjectScanner
  , PackagedAluminaSolution
  , PackagedFuel
  , PackagedHeavyOilResidue
  , PackagedIonizedFuel
  , PackagedLiquidBiofuel
  , PackagedNitricAcid
  , PackagedNitrogenGas
  , PackagedOil
  , PackagedRocketFuel
  , PackagedSulfuricAcid
  , PackagedTurbofuel
  , PackagedWater
  , Paleberry
  , Parachute
  , PetroleumCoke
  , Plastic
  , PlutoniumFuelRod
  , PlutoniumPellet
  , PlutoniumWaste
  , PolymerResin
  , PowerShard
  , PowerSlug
  , PressureConversionCube
  , QuartzCrystal
  , Quickwire
  , RadioControlUnit
  , RawQuartz
  , ReanimatedSAM
  , RebarGun
  , ReinforcedIronPlate
  , Rifle
  , Rotor
  , Rubber
  , SAM
  , SAMFluctuator
  , Screws
  , Silica
  , SingularityCell
  , SmartPlating
  , SmokelessPowder
  , SolidBiofuel
  , Somersloop
  , Stator
  , Statues
  , SteelBeam
  , SteelIngot
  , SteelPipe
  , Sulfur
  , Supercomputer
  , SuperpositionOscillator
  , ThermalPropulsionRocket
  , TimeCrystal
  , TurboMotor
  , Uranium
  , UraniumFuelRod
  , UraniumWaste
  , VersatileFramework
  , Vines
  , Wire
  , Wood
  , XenoBasher
  , XenoZapper
  , Zipline
  ) where


type AdaptiveControlUnit = "AdaptiveControlUnit"
type AIExpansionServer = "AIExpansionServer"
type AILimiter = "AILimiter"
type AlcladAluminumSheet = "AlcladAluminumSheet"
type AlienDNACapsule = "AlienDNACapsule"
type AlienPowerMatrix = "AlienPowerMatrix"
type AlienProtein = "AlienProtein"
type AlienRemains = "AlienRemains"
type AluminumCasing = "AluminumCasing"
type AluminumIngot = "AluminumIngot"
type AluminumScrap = "AluminumScrap"
type AssemblyDirectorSystem = "AssemblyDirectorSystem"
type AutomatedWiring = "AutomatedWiring"

type BaconAgaric = "BaconAgaric"
type BallisticWarpDrive = "BallisticWarpDrive"
type Battery = "Battery"
type Bauxite = "Bauxite"
type Beacon = "Beacon"
type BerylNut = "BerylNut"
type BiochemicalSculptor = "BiochemicalSculptor"
type Biomass = "Biomass"
type BlackPowder = "BlackPowder"
type BladeRunners = "BladeRunners"
type BoomBox = "BoomBox"

type Cable = "Cable"
type CateriumIngot = "CateriumIngot"
type CateriumOre = "CateriumOre"
type Chainsaw = "Chainsaw"
type CircuitBoard = "CircuitBoard"
type Coal = "Coal"
type ColorCartridge = "ColorCartridge"
type CompactedCoal = "CompactedCoal"
type Computer = "Computer"
type Concrete = "Concrete"
type CoolingSystem = "CoolingSystem"
type CopperIngot = "CopperIngot"
type CopperOre = "CopperOre"
type CopperPowder = "CopperPowder"
type CopperSheet = "CopperSheet"
type CrudeOil = "CrudeOil"
type CrystalOscillator = "CrystalOscillator"
type Cup = "Cup"

type DarkMatterCrystal = "DarkMatterCrystal"
type Diamonds = "Diamonds"

type ElectromagneticControlRod = "ElectromagneticControlRod"
type EmptyCanister = "EmptyCanister"
type EmptyFluidTank = "EmptyFluidTank"
type EncasedIndustrialBeam = "EncasedIndustrialBeam"
type EncasedPlutoniumCell = "EncasedPlutoniumCell"
type EncasedUraniumCell = "EncasedUraniumCell"

type Fabric = "Fabric"
type FicsitCoupon = "FicsitCoupon"
type FicsiteIngot = "FicsiteIngot"
type FicsiteTrigon = "FicsiteTrigon"
type Ficsonium = "Ficsonium"
type FicsoniumFuelRod = "FicsoniumFuelRod"
type FlowerPetals = "FlowerPetals"
type FusedModularFrame = "FusedModularFrame"

type GasMask = "GasMask"

type HardDrive = "HardDrive"
type HazmatSuit = "HazmatSuit"
type HeatSink = "HeatSink"
type HeavyModularFrame = "HeavyModularFrame"
type HighSpeedConnector = "HighSpeedConnector"
type Hoverpack = "Hoverpack"
type HubParts = "HubParts"

type IronIngot = "IronIngot"
type IronOre = "IronOre"
type IronPlate = "IronPlate"
type IronRod = "IronRod"

type Jetpack = "Jetpack"

type Leaves = "Leaves"
type Limestone = "Limestone"

type MagneticFieldGenerator = "MagneticFieldGenerator"
type MedicinalInhaler = "MedicinalInhaler"
type MercerSphere = "MercerSphere"
type Miner = "Miner"
type ModularEngine = "ModularEngine"
type ModularFrame = "ModularFrame"
type Motor = "Motor"
type Mycelia = "Mycelia"

type NeuralQuantumProcessor = "NeuralQuantumProcessor"
type NobeliskDetonator = "NobeliskDetonator"
type NonFissileUranium = "NonFissileUranium"
type NuclearPasta = "NuclearPasta"

type ObjectScanner = "ObjectScanner"

type PackagedAluminaSolution = "PackagedAluminaSolution"
type PackagedFuel = "PackagedFuel"
type PackagedHeavyOilResidue = "PackagedHeavyOilResidue"
type PackagedIonizedFuel = "PackagedIonizedFuel"
type PackagedLiquidBiofuel = "PackagedLiquidBiofuel"
type PackagedNitricAcid = "PackagedNitricAcid"
type PackagedNitrogenGas = "PackagedNitrogenGas"
type PackagedOil = "PackagedOil"
type PackagedRocketFuel = "PackagedRocketFuel"
type PackagedSulfuricAcid = "PackagedSulfuricAcid"
type PackagedTurbofuel = "PackagedTurbofuel"
type PackagedWater = "PackagedWater"
type Paleberry = "Paleberry"
type Parachute = "Parachute"
type PetroleumCoke = "PetroleumCoke"
type Plastic = "Plastic"
type PlutoniumFuelRod = "PlutoniumFuelRod"
type PlutoniumPellet = "PlutoniumPellet"
type PlutoniumWaste = "PlutoniumWaste"
type PolymerResin = "PolymerResin"
type PowerShard = "PowerShard"
type PowerSlug = "PowerSlug"
type PressureConversionCube = "PressureConversionCube"

type QuartzCrystal = "QuartzCrystal"
type Quickwire = "Quickwire"

type RadioControlUnit = "RadioControlUnit"
type RawQuartz = "RawQuartz"
type ReanimatedSAM = "ReanimatedSAM"
type RebarGun = "RebarGun"
type ReinforcedIronPlate = "ReinforcedIronPlate"
type Rifle = "Rifle"
type Rotor = "Rotor"
type Rubber = "Rubber"

type SAM = "SAM"
type SAMFluctuator = "SAMFluctuator"
type Screws = "Screws"
type Silica = "Silica"
type SingularityCell = "SingularityCell"
type SmartPlating = "SmartPlating"
type SmokelessPowder = "SmokelessPowder"
type SolidBiofuel = "SolidBiofuel"
type Somersloop = "Somersloop"
type Stator = "Stator"
type Statues = "Statues"
type SteelBeam = "SteelBeam"
type SteelIngot = "SteelIngot"
type SteelPipe = "SteelPipe"
type Sulfur = "Sulfur"
type Supercomputer = "Supercomputer"
type SuperpositionOscillator = "SuperpositionOscillator"

type ThermalPropulsionRocket = "ThermalPropulsionRocket"
type TimeCrystal = "TimeCrystal"
type TurboMotor = "TurboMotor"

type Uranium = "Uranium"
type UraniumFuelRod = "UraniumFuelRod"
type UraniumWaste = "UraniumWaste"

type VersatileFramework = "VersatileFramework"
type Vines = "Vines"

type Wire = "Wire"
type Wood = "Wood"

type XenoBasher = "XenoBasher"
type XenoZapper = "XenoZapper"

type Zipline = "Zipline"
