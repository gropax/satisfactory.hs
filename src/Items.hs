{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}

module Items
  ( Item(..)
  , ItemId(..), mkItemId
  , ItemRegistry, lookupItem
  , allItems
  , adaptiveControlUnit
  , aiExpansionServer
  , aiLimiter
  , alcladAluminumSheet
  , alienDNACapsule
  , alienPowerMatrix
  , alienProtein
  , alienRemains
  , aluminumCasing
  , aluminumIngot
  , aluminumScrap
  , assemblyDirectorSystem
  , automatedWiring
  , baconAgaric
  , ballisticWarpDrive
  , battery
  , bauxite
  , beacon
  , berylNut
  , biochemicalSculptor
  , biomass
  , blackPowder
  , bladeRunners
  , boomBox
  , cable
  , cateriumIngot
  , cateriumOre
  , chainsaw
  , circuitBoard
  , coal
  , colorCartridge
  , compactedCoal
  , computer
  , concrete
  , coolingSystem
  , copperIngot
  , copperOre
  , copperPowder
  , copperSheet
  , crudeOil
  , crystalOscillator
  , cup
  , darkMatterCrystal
  , diamonds
  , electromagneticControlRod
  , emptyCanister
  , emptyFluidTank
  , encasedIndustrialBeam
  , encasedPlutoniumCell
  , encasedUraniumCell
  , fabric
  , ficsitCoupon
  , ficsiteIngot
  , ficsiteTrigon
  , ficsonium
  , ficsoniumFuelRod
  , flowerPetals
  , fusedModularFrame
  , gasMask
  , hardDrive
  , hazmatSuit
  , heatSink
  , heavyModularFrame
  , highSpeedConnector
  , hoverpack
  , hubParts
  , ironIngot
  , ironOre
  , ironPlate
  , ironRod
  , jetpack
  , leaves
  , limestone
  , magneticFieldGenerator
  , medicinalInhaler
  , mercerSphere
  , miner
  , modularEngine
  , modularFrame
  , motor
  , mycelia
  , neuralQuantumProcessor
  , nobeliskDetonator
  , nonFissileUranium
  , nuclearPasta
  , objectScanner
  , packagedAluminaSolution
  , packagedFuel
  , packagedHeavyOilResidue
  , packagedIonizedFuel
  , packagedLiquidBiofuel
  , packagedNitricAcid
  , packagedNitrogenGas
  , packagedOil
  , packagedRocketFuel
  , packagedSulfuricAcid
  , packagedTurbofuel
  , packagedWater
  , paleberry
  , parachute
  , petroleumCoke
  , plastic
  , plutoniumFuelRod
  , plutoniumPellet
  , plutoniumWaste
  , polymerResin
  , powerShard
  , powerSlug
  , pressureConversionCube
  , quartzCrystal
  , quickwire
  , radioControlUnit
  , rawQuartz
  , reanimatedSAM
  , rebarGun
  , reinforcedIronPlate
  , rifle
  , rotor
  , rubber
  , sAM
  , sAMFluctuator
  , screws
  , silica
  , singularityCell
  , smartPlating
  , smokelessPowder
  , solidBiofuel
  , somersloop
  , stator
  , statues
  , steelBeam
  , steelIngot
  , steelPipe
  , sulfur
  , supercomputer
  , superpositionOscillator
  , thermalPropulsionRocket
  , timeCrystal
  , turboMotor
  , uranium
  , uraniumFuelRod
  , uraniumWaste
  , versatileFramework
  , vines
  , wire
  , wood
  , xenoBasher
  , xenoZapper
  , zipline
  ) where

import GHC.Generics (Generic)
import GHC.TypeLits
import Data.Proxy
import Symbols.Items
import qualified Data.Map.Strict as M


newtype ItemId = ItemId String
  deriving (Eq, Ord, Show, Generic)

mkItemId :: forall (s :: Symbol). KnownSymbol s => ItemId
mkItemId = ItemId (symbolVal (Proxy @s))


data Item = Item
  { itemId     :: ItemId
  , itemName   :: String
  , stackSize  :: Int
  , sinkPoints :: Int
  } deriving (Eq, Ord, Show)


type ItemRegistry = M.Map ItemId Item

itemRegistry :: ItemRegistry
itemRegistry = M.fromList [ (itemId i, i) | i <- allItems ]

lookupItem :: ItemId -> Maybe Item
lookupItem = flip M.lookup $ itemRegistry


allItems :: [Item]
allItems =
  [ adaptiveControlUnit
  , aiExpansionServer
  , aiLimiter
  , alcladAluminumSheet
  , alienDNACapsule
  , alienPowerMatrix
  , alienProtein
  , alienRemains
  , aluminumCasing
  , aluminumIngot
  , aluminumScrap
  , assemblyDirectorSystem
  , automatedWiring
  , baconAgaric
  , ballisticWarpDrive
  , battery
  , bauxite
  , beacon
  , berylNut
  , biochemicalSculptor
  , biomass
  , blackPowder
  , bladeRunners
  , boomBox
  , cable
  , cateriumIngot
  , cateriumOre
  , chainsaw
  , circuitBoard
  , coal
  , colorCartridge
  , compactedCoal
  , computer
  , concrete
  , coolingSystem
  , copperIngot
  , copperOre
  , copperPowder
  , copperSheet
  , crudeOil
  , crystalOscillator
  , cup
  , darkMatterCrystal
  , diamonds
  , electromagneticControlRod
  , emptyCanister
  , emptyFluidTank
  , encasedIndustrialBeam
  , encasedPlutoniumCell
  , encasedUraniumCell
  , fabric
  , ficsitCoupon
  , ficsiteIngot
  , ficsiteTrigon
  , ficsonium
  , ficsoniumFuelRod
  , flowerPetals
  , fusedModularFrame
  , gasMask
  , hardDrive
  , hazmatSuit
  , heatSink
  , heavyModularFrame
  , highSpeedConnector
  , hoverpack
  , hubParts
  , ironIngot
  , ironOre
  , ironPlate
  , ironRod
  , jetpack
  , leaves
  , limestone
  , magneticFieldGenerator
  , medicinalInhaler
  , mercerSphere
  , miner
  , modularEngine
  , modularFrame
  , motor
  , mycelia
  , neuralQuantumProcessor
  , nobeliskDetonator
  , nonFissileUranium
  , nuclearPasta
  , objectScanner
  , packagedAluminaSolution
  , packagedFuel
  , packagedHeavyOilResidue
  , packagedIonizedFuel
  , packagedLiquidBiofuel
  , packagedNitricAcid
  , packagedNitrogenGas
  , packagedOil
  , packagedRocketFuel
  , packagedSulfuricAcid
  , packagedTurbofuel
  , packagedWater
  , paleberry
  , parachute
  , petroleumCoke
  , plastic
  , plutoniumFuelRod
  , plutoniumPellet
  , plutoniumWaste
  , polymerResin
  , powerShard
  , powerSlug
  , pressureConversionCube
  , quartzCrystal
  , quickwire
  , radioControlUnit
  , rawQuartz
  , reanimatedSAM
  , rebarGun
  , reinforcedIronPlate
  , rifle
  , rotor
  , rubber
  , sAM
  , sAMFluctuator
  , screws
  , silica
  , singularityCell
  , smartPlating
  , smokelessPowder
  , solidBiofuel
  , somersloop
  , stator
  , statues
  , steelBeam
  , steelIngot
  , steelPipe
  , sulfur
  , supercomputer
  , superpositionOscillator
  , thermalPropulsionRocket
  , timeCrystal
  , turboMotor
  , uranium
  , uraniumFuelRod
  , uraniumWaste
  , versatileFramework
  , vines
  , wire
  , wood
  , xenoBasher
  , xenoZapper
  , zipline
  ]


adaptiveControlUnit :: Item
adaptiveControlUnit = Item
  { itemId = mkItemId @AdaptiveControlUnit
  , itemName = "Adaptive Control Unit"
  , stackSize = 50
  , sinkPoints = 76_368
  }

aiExpansionServer :: Item
aiExpansionServer = Item
  { itemId = mkItemId @AIExpansionServer
  , itemName = "AI Expansion Server"
  , stackSize = 50
  , sinkPoints = 597_652
  }

aiLimiter :: Item
aiLimiter = Item
  { itemId = mkItemId @AILimiter
  , itemName = "AI Limiter"
  , stackSize = 100
  , sinkPoints = 920
  }

alcladAluminumSheet :: Item
alcladAluminumSheet = Item
  { itemId = mkItemId @AlcladAluminumSheet
  , itemName = "Alclad Aluminum Sheet"
  , stackSize = 200
  , sinkPoints = 266
  }

alienDNACapsule :: Item
alienDNACapsule = Item
  { itemId = mkItemId @AlienDNACapsule
  , itemName = "Alien DNA Capsule"
  , stackSize = 50
  , sinkPoints = 0
  }

alienPowerMatrix :: Item
alienPowerMatrix = Item
  { itemId = mkItemId @AlienPowerMatrix
  , itemName = "Alien Power Matrix"
  , stackSize = 50
  , sinkPoints = 210
  }

alienProtein :: Item
alienProtein = Item
  { itemId = mkItemId @AlienProtein
  , itemName = "Alien Protein"
  , stackSize = 100
  , sinkPoints = 0
  }

-- FIXME
alienRemains :: Item
alienRemains = Item
  { itemId = mkItemId @AlienRemains
  , itemName = "Alien Remains"
  , stackSize = 50
  , sinkPoints = 0
  }

aluminumCasing :: Item
aluminumCasing = Item
  { itemId = mkItemId @AluminumCasing
  , itemName = "Aluminum Casing"
  , stackSize = 200
  , sinkPoints = 393
  }

aluminumIngot :: Item
aluminumIngot = Item
  { itemId = mkItemId @AluminumIngot
  , itemName = "Aluminum Ingot"
  , stackSize = 100
  , sinkPoints = 131
  }

aluminumScrap :: Item
aluminumScrap = Item
  { itemId = mkItemId @AluminumScrap
  , itemName = "Aluminum Scrap"
  , stackSize = 500
  , sinkPoints = 27
  }

assemblyDirectorSystem :: Item
assemblyDirectorSystem = Item
  { itemId = mkItemId @AssemblyDirectorSystem
  , itemName = "Assembly Director System"
  , stackSize = 50
  , sinkPoints = 500_176
  }

automatedWiring :: Item
automatedWiring = Item
  { itemId = mkItemId @AutomatedWiring
  , itemName = "Automated Wiring"
  , stackSize = 50
  , sinkPoints = 1_440
  }

-- CHECKPOINT

baconAgaric :: Item
baconAgaric = Item
  { itemId = mkItemId @BaconAgaric
  , itemName = "Bacon Agaric"
  , stackSize = 100
  , sinkPoints = 0
  }

ballisticWarpDrive :: Item
ballisticWarpDrive = Item
  { itemId = mkItemId @BallisticWarpDrive
  , itemName = "Ballistic Warp Drive"
  , stackSize = 100
  , sinkPoints = 0
  }

battery :: Item
battery = Item
  { itemId = mkItemId @Battery
  , itemName = "Battery"
  , stackSize = 100
  , sinkPoints = 0
  }

bauxite :: Item
bauxite = Item
  { itemId = mkItemId @Bauxite
  , itemName = "Bauxite"
  , stackSize = 100
  , sinkPoints = 0
  }

beacon :: Item
beacon = Item
  { itemId = mkItemId @Beacon
  , itemName = "Beacon"
  , stackSize = 100
  , sinkPoints = 0
  }

berylNut :: Item
berylNut = Item
  { itemId = mkItemId @BerylNut
  , itemName = "Beryl Nut"
  , stackSize = 100
  , sinkPoints = 0
  }

biochemicalSculptor :: Item
biochemicalSculptor = Item
  { itemId = mkItemId @BiochemicalSculptor
  , itemName = "Biochemical Sculptor"
  , stackSize = 100
  , sinkPoints = 0
  }

biomass :: Item
biomass = Item
  { itemId = mkItemId @Biomass
  , itemName = "Biomass"
  , stackSize = 100
  , sinkPoints = 0
  }

blackPowder :: Item
blackPowder = Item
  { itemId = mkItemId @BlackPowder
  , itemName = "Black Powder"
  , stackSize = 100
  , sinkPoints = 0
  }

bladeRunners :: Item
bladeRunners = Item
  { itemId = mkItemId @BladeRunners
  , itemName = "Blade Runners"
  , stackSize = 100
  , sinkPoints = 0
  }

boomBox :: Item
boomBox = Item
  { itemId = mkItemId @BoomBox
  , itemName = "Boom Box"
  , stackSize = 100
  , sinkPoints = 0
  }

cable :: Item
cable = Item
  { itemId = mkItemId @Cable
  , itemName = "Cable"
  , stackSize = 100
  , sinkPoints = 0
  }

cateriumIngot :: Item
cateriumIngot = Item
  { itemId = mkItemId @CateriumIngot
  , itemName = "Caterium Ingot"
  , stackSize = 100
  , sinkPoints = 0
  }

cateriumOre :: Item
cateriumOre = Item
  { itemId = mkItemId @CateriumOre
  , itemName = "Caterium Ore"
  , stackSize = 100
  , sinkPoints = 0
  }

chainsaw :: Item
chainsaw = Item
  { itemId = mkItemId @Chainsaw
  , itemName = "Chainsaw"
  , stackSize = 100
  , sinkPoints = 0
  }

circuitBoard :: Item
circuitBoard = Item
  { itemId = mkItemId @CircuitBoard
  , itemName = "Circuit Board"
  , stackSize = 100
  , sinkPoints = 0
  }

coal :: Item
coal = Item
  { itemId = mkItemId @Coal
  , itemName = "Coal"
  , stackSize = 100
  , sinkPoints = 0
  }

colorCartridge :: Item
colorCartridge = Item
  { itemId = mkItemId @ColorCartridge
  , itemName = "Color Cartridge"
  , stackSize = 100
  , sinkPoints = 0
  }

compactedCoal :: Item
compactedCoal = Item
  { itemId = mkItemId @CompactedCoal
  , itemName = "Compacted Coal"
  , stackSize = 100
  , sinkPoints = 0
  }

computer :: Item
computer = Item
  { itemId = mkItemId @Computer
  , itemName = "Computer"
  , stackSize = 100
  , sinkPoints = 0
  }

concrete :: Item
concrete = Item
  { itemId = mkItemId @Concrete
  , itemName = "Concrete"
  , stackSize = 100
  , sinkPoints = 0
  }

coolingSystem :: Item
coolingSystem = Item
  { itemId = mkItemId @CoolingSystem
  , itemName = "Cooling System"
  , stackSize = 100
  , sinkPoints = 0
  }

copperIngot :: Item
copperIngot = Item
  { itemId = mkItemId @CopperIngot
  , itemName = "Copper Ingot"
  , stackSize = 100
  , sinkPoints = 0
  }

copperOre :: Item
copperOre = Item
  { itemId = mkItemId @CopperOre
  , itemName = "Copper Ore"
  , stackSize = 100
  , sinkPoints = 0
  }

copperPowder :: Item
copperPowder = Item
  { itemId = mkItemId @CopperPowder
  , itemName = "Copper Powder"
  , stackSize = 100
  , sinkPoints = 0
  }

copperSheet :: Item
copperSheet = Item
  { itemId = mkItemId @CopperSheet
  , itemName = "Copper Sheet"
  , stackSize = 100
  , sinkPoints = 0
  }

crudeOil :: Item
crudeOil = Item
  { itemId = mkItemId @CrudeOil
  , itemName = "Crude Oil"
  , stackSize = 100
  , sinkPoints = 0
  }

crystalOscillator :: Item
crystalOscillator = Item
  { itemId = mkItemId @CrystalOscillator
  , itemName = "Crystal Oscillator"
  , stackSize = 100
  , sinkPoints = 0
  }

cup :: Item
cup = Item
  { itemId = mkItemId @Cup
  , itemName = "Cup"
  , stackSize = 100
  , sinkPoints = 0
  }

darkMatterCrystal :: Item
darkMatterCrystal = Item
  { itemId = mkItemId @DarkMatterCrystal
  , itemName = "Dark Matter Crystal"
  , stackSize = 100
  , sinkPoints = 0
  }

diamonds :: Item
diamonds = Item
  { itemId = mkItemId @Diamonds
  , itemName = "Diamonds"
  , stackSize = 100
  , sinkPoints = 0
  }

electromagneticControlRod :: Item
electromagneticControlRod = Item
  { itemId = mkItemId @ElectromagneticControlRod
  , itemName = "Electromagnetic Control Rod"
  , stackSize = 100
  , sinkPoints = 0
  }

emptyCanister :: Item
emptyCanister = Item
  { itemId = mkItemId @EmptyCanister
  , itemName = "Empty Canister"
  , stackSize = 100
  , sinkPoints = 0
  }

emptyFluidTank :: Item
emptyFluidTank = Item
  { itemId = mkItemId @EmptyFluidTank
  , itemName = "Empty Fluid Tank"
  , stackSize = 100
  , sinkPoints = 0
  }

encasedIndustrialBeam :: Item
encasedIndustrialBeam = Item
  { itemId = mkItemId @EncasedIndustrialBeam
  , itemName = "Encased Industrial Beam"
  , stackSize = 100
  , sinkPoints = 0
  }

encasedPlutoniumCell :: Item
encasedPlutoniumCell = Item
  { itemId = mkItemId @EncasedPlutoniumCell
  , itemName = "Encased Plutonium Cell"
  , stackSize = 100
  , sinkPoints = 0
  }

encasedUraniumCell :: Item
encasedUraniumCell = Item
  { itemId = mkItemId @EncasedUraniumCell
  , itemName = "Encased Uranium Cell"
  , stackSize = 100
  , sinkPoints = 0
  }

fabric :: Item
fabric = Item
  { itemId = mkItemId @Fabric
  , itemName = "Fabric"
  , stackSize = 100
  , sinkPoints = 0
  }

ficsitCoupon :: Item
ficsitCoupon = Item
  { itemId = mkItemId @FicsitCoupon
  , itemName = "FICSIT Coupon"
  , stackSize = 100
  , sinkPoints = 0
  }

ficsiteIngot :: Item
ficsiteIngot = Item
  { itemId = mkItemId @FicsiteIngot
  , itemName = "Ficsite Ingot"
  , stackSize = 100
  , sinkPoints = 0
  }

ficsiteTrigon :: Item
ficsiteTrigon = Item
  { itemId = mkItemId @FicsiteTrigon
  , itemName = "Ficsite Trigon"
  , stackSize = 100
  , sinkPoints = 0
  }

ficsonium :: Item
ficsonium = Item
  { itemId = mkItemId @Ficsonium
  , itemName = "Ficsonium"
  , stackSize = 100
  , sinkPoints = 0
  }

ficsoniumFuelRod :: Item
ficsoniumFuelRod = Item
  { itemId = mkItemId @FicsoniumFuelRod
  , itemName = "Ficsonium Fuel Rod"
  , stackSize = 100
  , sinkPoints = 0
  }

flowerPetals :: Item
flowerPetals = Item
  { itemId = mkItemId @FlowerPetals
  , itemName = "Flower Petals"
  , stackSize = 100
  , sinkPoints = 0
  }

fusedModularFrame :: Item
fusedModularFrame = Item
  { itemId = mkItemId @FusedModularFrame
  , itemName = "Fused Modular Frame"
  , stackSize = 100
  , sinkPoints = 0
  }

gasMask :: Item
gasMask = Item
  { itemId = mkItemId @GasMask
  , itemName = "Gas Mask"
  , stackSize = 100
  , sinkPoints = 0
  }

hardDrive :: Item
hardDrive = Item
  { itemId = mkItemId @HardDrive
  , itemName = "Hard Drive"
  , stackSize = 100
  , sinkPoints = 0
  }

hazmatSuit :: Item
hazmatSuit = Item
  { itemId = mkItemId @HazmatSuit
  , itemName = "Hazmat Suit"
  , stackSize = 100
  , sinkPoints = 0
  }

heatSink :: Item
heatSink = Item
  { itemId = mkItemId @HeatSink
  , itemName = "Heat Sink"
  , stackSize = 100
  , sinkPoints = 0
  }

heavyModularFrame :: Item
heavyModularFrame = Item
  { itemId = mkItemId @HeavyModularFrame
  , itemName = "Heavy Modular Frame"
  , stackSize = 100
  , sinkPoints = 0
  }

highSpeedConnector :: Item
highSpeedConnector = Item
  { itemId = mkItemId @HighSpeedConnector
  , itemName = "High-Speed Connector"
  , stackSize = 100
  , sinkPoints = 0
  }

hoverpack :: Item
hoverpack = Item
  { itemId = mkItemId @Hoverpack
  , itemName = "Hoverpack"
  , stackSize = 100
  , sinkPoints = 0
  }

hubParts :: Item
hubParts = Item
  { itemId = mkItemId @HubParts
  , itemName = "HUB Parts"
  , stackSize = 100
  , sinkPoints = 0
  }

ironIngot :: Item
ironIngot = Item
  { itemId = mkItemId @IronIngot
  , itemName = "Iron Ingot"
  , stackSize = 100
  , sinkPoints = 0
  }

ironOre :: Item
ironOre = Item
  { itemId = mkItemId @IronOre
  , itemName = "Iron Ore"
  , stackSize = 100
  , sinkPoints = 0
  }

ironPlate :: Item
ironPlate = Item
  { itemId = mkItemId @IronPlate
  , itemName = "Iron Plate"
  , stackSize = 100
  , sinkPoints = 0
  }

ironRod :: Item
ironRod = Item
  { itemId = mkItemId @IronRod
  , itemName = "Iron Rod"
  , stackSize = 100
  , sinkPoints = 0
  }

jetpack :: Item
jetpack = Item
  { itemId = mkItemId @Jetpack
  , itemName = "Jetpack"
  , stackSize = 100
  , sinkPoints = 0
  }

leaves :: Item
leaves = Item
  { itemId = mkItemId @Leaves
  , itemName = "Leaves"
  , stackSize = 100
  , sinkPoints = 0
  }

limestone :: Item
limestone = Item
  { itemId = mkItemId @Limestone
  , itemName = "Limestone"
  , stackSize = 100
  , sinkPoints = 0
  }

magneticFieldGenerator :: Item
magneticFieldGenerator = Item
  { itemId = mkItemId @MagneticFieldGenerator
  , itemName = "Magnetic Field Generator"
  , stackSize = 100
  , sinkPoints = 0
  }

medicinalInhaler :: Item
medicinalInhaler = Item
  { itemId = mkItemId @MedicinalInhaler
  , itemName = "Medicinal Inhaler"
  , stackSize = 100
  , sinkPoints = 0
  }

mercerSphere :: Item
mercerSphere = Item
  { itemId = mkItemId @MercerSphere
  , itemName = "Mercer Sphere"
  , stackSize = 100
  , sinkPoints = 0
  }

miner :: Item
miner = Item
  { itemId = mkItemId @Miner
  , itemName = "Miner"
  , stackSize = 100
  , sinkPoints = 0
  }

modularEngine :: Item
modularEngine = Item
  { itemId = mkItemId @ModularEngine
  , itemName = "Modular Engine"
  , stackSize = 100
  , sinkPoints = 0
  }

modularFrame :: Item
modularFrame = Item
  { itemId = mkItemId @ModularFrame
  , itemName = "Modular Frame"
  , stackSize = 100
  , sinkPoints = 0
  }

motor :: Item
motor = Item
  { itemId = mkItemId @Motor
  , itemName = "Motor"
  , stackSize = 100
  , sinkPoints = 0
  }

mycelia :: Item
mycelia = Item
  { itemId = mkItemId @Mycelia
  , itemName = "Mycelia"
  , stackSize = 100
  , sinkPoints = 0
  }

neuralQuantumProcessor :: Item
neuralQuantumProcessor = Item
  { itemId = mkItemId @NeuralQuantumProcessor
  , itemName = "Neural-Quantum Processor"
  , stackSize = 100
  , sinkPoints = 0
  }

nobeliskDetonator :: Item
nobeliskDetonator = Item
  { itemId = mkItemId @NobeliskDetonator
  , itemName = "Nobelisk Detonator"
  , stackSize = 100
  , sinkPoints = 0
  }

nonFissileUranium :: Item
nonFissileUranium = Item
  { itemId = mkItemId @NonFissileUranium
  , itemName = "Non-Fissile Uranium"
  , stackSize = 100
  , sinkPoints = 0
  }

nuclearPasta :: Item
nuclearPasta = Item
  { itemId = mkItemId @NuclearPasta
  , itemName = "Nuclear Pasta"
  , stackSize = 100
  , sinkPoints = 0
  }

objectScanner :: Item
objectScanner = Item
  { itemId = mkItemId @ObjectScanner
  , itemName = "Object Scanner"
  , stackSize = 100
  , sinkPoints = 0
  }

packagedAluminaSolution :: Item
packagedAluminaSolution = Item
  { itemId = mkItemId @PackagedAluminaSolution
  , itemName = "Packaged Alumina Solution"
  , stackSize = 100
  , sinkPoints = 0
  }

packagedFuel :: Item
packagedFuel = Item
  { itemId = mkItemId @PackagedFuel
  , itemName = "Packaged Fuel"
  , stackSize = 100
  , sinkPoints = 0
  }

packagedHeavyOilResidue :: Item
packagedHeavyOilResidue = Item
  { itemId = mkItemId @PackagedHeavyOilResidue
  , itemName = "Packaged Heavy Oil Residue"
  , stackSize = 100
  , sinkPoints = 0
  }

packagedIonizedFuel :: Item
packagedIonizedFuel = Item
  { itemId = mkItemId @PackagedIonizedFuel
  , itemName = "Packaged Ionized Fuel"
  , stackSize = 100
  , sinkPoints = 0
  }

packagedLiquidBiofuel :: Item
packagedLiquidBiofuel = Item
  { itemId = mkItemId @PackagedLiquidBiofuel
  , itemName = "Packaged Liquid Biofuel"
  , stackSize = 100
  , sinkPoints = 0
  }

packagedNitricAcid :: Item
packagedNitricAcid = Item
  { itemId = mkItemId @PackagedNitricAcid
  , itemName = "Packaged Nitric Acid"
  , stackSize = 100
  , sinkPoints = 0
  }

packagedNitrogenGas :: Item
packagedNitrogenGas = Item
  { itemId = mkItemId @PackagedNitrogenGas
  , itemName = "Packaged Nitrogen Gas"
  , stackSize = 100
  , sinkPoints = 0
  }

packagedOil :: Item
packagedOil = Item
  { itemId = mkItemId @PackagedOil
  , itemName = "Packaged Oil"
  , stackSize = 100
  , sinkPoints = 0
  }

packagedRocketFuel :: Item
packagedRocketFuel = Item
  { itemId = mkItemId @PackagedRocketFuel
  , itemName = "Packaged Rocket Fuel"
  , stackSize = 100
  , sinkPoints = 0
  }

packagedSulfuricAcid :: Item
packagedSulfuricAcid = Item
  { itemId = mkItemId @PackagedSulfuricAcid
  , itemName = "Packaged Sulfuric Acid"
  , stackSize = 100
  , sinkPoints = 0
  }

packagedTurbofuel :: Item
packagedTurbofuel = Item
  { itemId = mkItemId @PackagedTurbofuel
  , itemName = "Packaged Turbofuel"
  , stackSize = 100
  , sinkPoints = 0
  }

packagedWater :: Item
packagedWater = Item
  { itemId = mkItemId @PackagedWater
  , itemName = "Packaged Water"
  , stackSize = 100
  , sinkPoints = 0
  }

paleberry :: Item
paleberry = Item
  { itemId = mkItemId @Paleberry
  , itemName = "Paleberry"
  , stackSize = 100
  , sinkPoints = 0
  }

parachute :: Item
parachute = Item
  { itemId = mkItemId @Parachute
  , itemName = "Parachute"
  , stackSize = 100
  , sinkPoints = 0
  }

petroleumCoke :: Item
petroleumCoke = Item
  { itemId = mkItemId @PetroleumCoke
  , itemName = "Petroleum Coke"
  , stackSize = 100
  , sinkPoints = 0
  }

plastic :: Item
plastic = Item
  { itemId = mkItemId @Plastic
  , itemName = "Plastic"
  , stackSize = 100
  , sinkPoints = 0
  }

plutoniumFuelRod :: Item
plutoniumFuelRod = Item
  { itemId = mkItemId @PlutoniumFuelRod
  , itemName = "Plutonium Fuel Rod"
  , stackSize = 100
  , sinkPoints = 0
  }

plutoniumPellet :: Item
plutoniumPellet = Item
  { itemId = mkItemId @PlutoniumPellet
  , itemName = "Plutonium Pellet"
  , stackSize = 100
  , sinkPoints = 0
  }

plutoniumWaste :: Item
plutoniumWaste = Item
  { itemId = mkItemId @PlutoniumWaste
  , itemName = "Plutonium Waste"
  , stackSize = 100
  , sinkPoints = 0
  }

polymerResin :: Item
polymerResin = Item
  { itemId = mkItemId @PolymerResin
  , itemName = "Polymer Resin"
  , stackSize = 100
  , sinkPoints = 0
  }

powerShard :: Item
powerShard = Item
  { itemId = mkItemId @PowerShard
  , itemName = "Power Shard"
  , stackSize = 100
  , sinkPoints = 0
  }

powerSlug :: Item
powerSlug = Item
  { itemId = mkItemId @PowerSlug
  , itemName = "Power Slug"
  , stackSize = 100
  , sinkPoints = 0
  }

pressureConversionCube :: Item
pressureConversionCube = Item
  { itemId = mkItemId @PressureConversionCube
  , itemName = "Pressure Conversion Cube"
  , stackSize = 100
  , sinkPoints = 0
  }

quartzCrystal :: Item
quartzCrystal = Item
  { itemId = mkItemId @QuartzCrystal
  , itemName = "Quartz Crystal"
  , stackSize = 100
  , sinkPoints = 0
  }

quickwire :: Item
quickwire = Item
  { itemId = mkItemId @Quickwire
  , itemName = "Quickwire"
  , stackSize = 100
  , sinkPoints = 0
  }

radioControlUnit :: Item
radioControlUnit = Item
  { itemId = mkItemId @RadioControlUnit
  , itemName = "Radio Control Unit"
  , stackSize = 100
  , sinkPoints = 0
  }

rawQuartz :: Item
rawQuartz = Item
  { itemId = mkItemId @RawQuartz
  , itemName = "Raw Quartz"
  , stackSize = 100
  , sinkPoints = 0
  }

reanimatedSAM :: Item
reanimatedSAM = Item
  { itemId = mkItemId @ReanimatedSAM
  , itemName = "Reanimated SAM"
  , stackSize = 100
  , sinkPoints = 0
  }

rebarGun :: Item
rebarGun = Item
  { itemId = mkItemId @RebarGun
  , itemName = "Rebar Gun"
  , stackSize = 100
  , sinkPoints = 0
  }

reinforcedIronPlate :: Item
reinforcedIronPlate = Item
  { itemId = mkItemId @ReinforcedIronPlate
  , itemName = "Reinforced Iron Plate"
  , stackSize = 100
  , sinkPoints = 0
  }

rifle :: Item
rifle = Item
  { itemId = mkItemId @Rifle
  , itemName = "Rifle"
  , stackSize = 100
  , sinkPoints = 0
  }

rotor :: Item
rotor = Item
  { itemId = mkItemId @Rotor
  , itemName = "Rotor"
  , stackSize = 100
  , sinkPoints = 0
  }

rubber :: Item
rubber = Item
  { itemId = mkItemId @Rubber
  , itemName = "Rubber"
  , stackSize = 100
  , sinkPoints = 0
  }

sAM :: Item
sAM = Item
  { itemId = mkItemId @SAM
  , itemName = "SAM"
  , stackSize = 100
  , sinkPoints = 0
  }

sAMFluctuator :: Item
sAMFluctuator = Item
  { itemId = mkItemId @SAMFluctuator
  , itemName = "SAM Fluctuator"
  , stackSize = 100
  , sinkPoints = 0
  }

screws :: Item
screws = Item
  { itemId = mkItemId @Screws
  , itemName = "Screws"
  , stackSize = 100
  , sinkPoints = 0
  }

silica :: Item
silica = Item
  { itemId = mkItemId @Silica
  , itemName = "Silica"
  , stackSize = 100
  , sinkPoints = 0
  }

singularityCell :: Item
singularityCell = Item
  { itemId = mkItemId @SingularityCell
  , itemName = "Singularity Cell"
  , stackSize = 100
  , sinkPoints = 0
  }

smartPlating :: Item
smartPlating = Item
  { itemId = mkItemId @SmartPlating
  , itemName = "Smart Plating"
  , stackSize = 100
  , sinkPoints = 0
  }

smokelessPowder :: Item
smokelessPowder = Item
  { itemId = mkItemId @SmokelessPowder
  , itemName = "Smokeless Powder"
  , stackSize = 100
  , sinkPoints = 0
  }

solidBiofuel :: Item
solidBiofuel = Item
  { itemId = mkItemId @SolidBiofuel
  , itemName = "Solid Biofuel"
  , stackSize = 100
  , sinkPoints = 0
  }

somersloop :: Item
somersloop = Item
  { itemId = mkItemId @Somersloop
  , itemName = "Somersloop"
  , stackSize = 100
  , sinkPoints = 0
  }

stator :: Item
stator = Item
  { itemId = mkItemId @Stator
  , itemName = "Stator"
  , stackSize = 100
  , sinkPoints = 0
  }

statues :: Item
statues = Item
  { itemId = mkItemId @Statues
  , itemName = "Statues"
  , stackSize = 100
  , sinkPoints = 0
  }

steelBeam :: Item
steelBeam = Item
  { itemId = mkItemId @SteelBeam
  , itemName = "Steel Beam"
  , stackSize = 100
  , sinkPoints = 0
  }

steelIngot :: Item
steelIngot = Item
  { itemId = mkItemId @SteelIngot
  , itemName = "Steel Ingot"
  , stackSize = 100
  , sinkPoints = 0
  }

steelPipe :: Item
steelPipe = Item
  { itemId = mkItemId @SteelPipe
  , itemName = "Steel Pipe"
  , stackSize = 100
  , sinkPoints = 0
  }

sulfur :: Item
sulfur = Item
  { itemId = mkItemId @Sulfur
  , itemName = "Sulfur"
  , stackSize = 100
  , sinkPoints = 0
  }

supercomputer :: Item
supercomputer = Item
  { itemId = mkItemId @Supercomputer
  , itemName = "Supercomputer"
  , stackSize = 100
  , sinkPoints = 0
  }

superpositionOscillator :: Item
superpositionOscillator = Item
  { itemId = mkItemId @SuperpositionOscillator
  , itemName = "Superposition Oscillator"
  , stackSize = 100
  , sinkPoints = 0
  }

thermalPropulsionRocket :: Item
thermalPropulsionRocket = Item
  { itemId = mkItemId @ThermalPropulsionRocket
  , itemName = "Thermal Propulsion Rocket"
  , stackSize = 100
  , sinkPoints = 0
  }

timeCrystal :: Item
timeCrystal = Item
  { itemId = mkItemId @TimeCrystal
  , itemName = "Time Crystal"
  , stackSize = 100
  , sinkPoints = 0
  }

turboMotor :: Item
turboMotor = Item
  { itemId = mkItemId @TurboMotor
  , itemName = "Turbo Motor"
  , stackSize = 100
  , sinkPoints = 0
  }

uranium :: Item
uranium = Item
  { itemId = mkItemId @Uranium
  , itemName = "Uranium"
  , stackSize = 100
  , sinkPoints = 0
  }

uraniumFuelRod :: Item
uraniumFuelRod = Item
  { itemId = mkItemId @UraniumFuelRod
  , itemName = "Uranium Fuel Rod"
  , stackSize = 100
  , sinkPoints = 0
  }

uraniumWaste :: Item
uraniumWaste = Item
  { itemId = mkItemId @UraniumWaste
  , itemName = "Uranium Waste"
  , stackSize = 100
  , sinkPoints = 0
  }

versatileFramework :: Item
versatileFramework = Item
  { itemId = mkItemId @VersatileFramework
  , itemName = "Versatile Framework"
  , stackSize = 100
  , sinkPoints = 0
  }

vines :: Item
vines = Item
  { itemId = mkItemId @Vines
  , itemName = "Vines"
  , stackSize = 100
  , sinkPoints = 0
  }

wire :: Item
wire = Item
  { itemId = mkItemId @Wire
  , itemName = "Wire"
  , stackSize = 100
  , sinkPoints = 0
  }

wood :: Item
wood = Item
  { itemId = mkItemId @Wood
  , itemName = "Wood"
  , stackSize = 100
  , sinkPoints = 0
  }

xenoBasher :: Item
xenoBasher = Item
  { itemId = mkItemId @XenoBasher
  , itemName = "Xeno-Basher"
  , stackSize = 100
  , sinkPoints = 0
  }

xenoZapper :: Item
xenoZapper = Item
  { itemId = mkItemId @XenoZapper
  , itemName = "Xeno-Zapper"
  , stackSize = 100
  , sinkPoints = 0
  }

zipline :: Item
zipline = Item
  { itemId = mkItemId @Zipline
  , itemName = "Zipline"
  , stackSize = 100
  , sinkPoints = 0
  }

