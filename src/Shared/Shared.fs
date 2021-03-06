namespace Shared

open System

type Counter = { Value: int }

type ProductDimension = {
    Heigth: float
    Width: float
    Depth: float option
}

type ProductColor =
    | Red
    | Black
    | White
    | Gray
    | Blue
    | Green
    | NotSupportedByStore

type Brand =
    | Toshiba
    | Sony
    | Microsoft
    | Intel
    | AMD
    | Nintendo
    | Bose
    | Asus
    | Apple
    | NotSupportedByStore

type SupportedLanguage =
    | English
    | French
    | NotSupportedByStore

type CommonProductInformation  = {
    Name:           string
    Weight:         float
    ShippingWeight: float
    AverageReviews: float
    Dimensions:     ProductDimension
    Price:          float
    Color:          ProductColor
    Brand:          Brand
}

type DeviceDisplay =
    | StandardDefinition    of int
    | EnhanceDefinition     of int
    | HighDefinition        of int
    | UltraHighDefinition   of int

type OperatingSystem =
    | Windows7
    | Windows8
    | Windows10
    | MacOS
    | Linux
    | XboxOne
    | Playstation4
    | Switch

type CableConnection =
    | USB1
    | USB2
    | USB3
    | USBC
    | HDMI
    | MiniHDMI
    | PowerAdapter

type Device = {
    ProductDetails:     CommonProductInformation
    ModelNumber:        string
    IsWireless:         bool
    SupportedOS:        OperatingSystem list
    HardwareInterfaces: CableConnection list
    Resolution:         DeviceDisplay
}

type Keyboard = {
    DeviceDefinition:   Device
    IsMechanical:       bool
    IsGamingKeyboard:   bool
    KeyCount:           int
}

type FightingPad = {
    DeviceDescription:      Device;
    AreBatteriesRequired:   bool
    HasProgrammableButtons: bool
}

type DeviceInput =
    | Keyboard  of Keyboard
    | Gamepad   of FightingPad

type IntelProcessorSeries =
    | IntelCorei3
    | IntelCorei5
    | IntelCorei7
    | IntelCorei9

type AmdProcessorSeries =
    | Ryden
    | Athlon
    | AthlonII
    | ASeries
    | ESeries
    | FSeries

type ProcessorSeries =
    | Intel     of IntelProcessorSeries
    | AMD       of AmdProcessorSeries

type DDR =
    | DDR2
    | DDR3
    | DDR4

type CPU = {
    Details:            CommonProductInformation
    CoreCount:          int
    Series:             ProcessorSeries
    ProcessorSpeed:     float
    OverclockedSpeed:   float
    Wattage:            int
    YearModel:          DateTime
}

type Computer = {
    Details:                CommonProductInformation
    Resolution:             DeviceDisplay
    Cpu:                    CPU
    Ram:                    int
    CacheMemory:            int option
    DdrRam:                 DDR option
    RunningOperatingSystem: OperatingSystem
    DeviceInputs:           DeviceInput list
}

type GameConsole = {
    Hardware:               Computer
    SupportedResolutions:   DeviceDisplay list
    Inputs:                 CableConnection list
    IsHandHandledDevice:    bool
    MaxControllerSupported: int
}

type BookCategory =
    | Fantasy
    | ``Computer Science``
    | ``Graphic Novel``

type BookFormat =
    | Paperback
    | Hardcover
    | Pdf
    | KindleVersion

type Book = {
    AuthorName:     string
    Format:         BookFormat
    Summary:        string
    Details:        CommonProductInformation
    Category:       BookCategory
    PageCount:      int
    ISBN:           string
    Language:       SupportedLanguage
    ReleasedDate:   DateTime
}

type HeadphoneFit =
    | ``In ear``
    | ``On ear``
    | ``Over ear``

type HeadphoneProduct = {
    Details:                CommonProductInformation
    Fit:                    HeadphoneFit
    BatteryLife:            int option
    ReleaseDate:            DateTime;
    AreWireless:            bool
    IsNoiseCancelActive:    bool
}

// Removed by default here; not necessary.
type StoreProduct =
    | Book                  of novel:       Book * SkuId: string
    | WirelessHeadphones    of headphones:  HeadphoneProduct * SkuId: string
    | Television            of television:  Device * SkuId: string
    | Laptop                of laptop:      Computer * SkuId: string
    | GameConsole           of console:     GameConsole * SkuId: string
with
    member x.ProductPrice =
        match x with
        | Book (b, _) -> b.Details.Price
        | WirelessHeadphones (wh, _) -> wh.Details.Price
        | Television (t, _) -> t.ProductDetails.Price
        | Laptop (l, _) -> l.Details.Price
        | GameConsole (gc, _) -> gc.Hardware.Details.Price


type Product = {
    Id: int
    Item: StoreProduct
}

type TestSerializeProduct = { Id: int ; Value : StoreProduct array }