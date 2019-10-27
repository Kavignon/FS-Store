module StoreProductDomain

open System
open FSharp.Data

type Counter = { Value: int }

module StoreProductDomain 
    type ProductDb = XmlProvider<"./CommerceDb.xml"> // Move the data to Azure.
    let productsFromDatabase = ProductDb.GetSample()
    
    type ProductDimension = {
        Heigth: decimal
        Width: decimal
        Depth: decimal option
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
        Price:          decimal
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
        KeyCount:           byte
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
        CoreCount:          byte
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
        MaxControllerSupported: byte
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
        BatteryLife:            sbyte option
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

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let (|Suffix|_|) (p:string) (s:string) =
    if s.EndsWith(p) then
        Some("")
    else
        None

type GeneratedTypeFromStore =
    | Headphones                of value: ProductDb.Headphone
    | ReadingMaterial           of value: ProductDb.Book
    | Computer                  of value: ProductDb.Computer

let bookCategory (book: ProductDb.Book)=
    match book.Category.Value with
    | Prefix "Fan" _ -> Some Fantasy
    | Prefix "Com" _ & Suffix "Sci" _ -> Some ``Computer Science``
    | _ -> None

type DbProductUtils =
    static member getFitFromHeadphones (h: ProductDb.Headphone) =
        match h.Fit.Value with
            | Prefix "In" _ -> ``In ear``
            | Prefix "On" _ -> ``On ear``
            | _ -> ``Over ear``

    static member getColorFromDbProduct colorString =
       match colorString with
        | "Red" -> Some Red
        | "Black" -> Some Black
        | "White" -> Some White
        | "Gray" -> Some Gray
        | "Blue" -> Some Blue
        | "Green" -> Some Green
        | _ -> None
        |> Option.defaultValue ProductColor.NotSupportedByStore

    static member getBrandFromDbProduct brandString =
        match brandString with
        | "Toshiba" -> Some Toshiba
        | "Sony" -> Some Sony
        | "Microsoft" -> Some Microsoft
        | "Intel" -> Some Brand.Intel
        | "AMD" -> Some Brand.AMD
        | "Nintendo" -> Some Nintendo
        | "Bose" -> Some Bose
        | "Asus" -> Some Asus
        | "Apple" -> Some Apple
        | _ -> None
        |> Option.defaultValue Brand.NotSupportedByStore

// TODO: Refactor implementation here since a lot can be abstracted through a function to create the record.
let getProductInfoFromProvider providerType =
        match providerType with        
        | Headphones dbHeadphones ->
            {
                Name = dbHeadphones.Model.Name
                Weight = dbHeadphones.Weigth.Value |> float
                ShippingWeight = dbHeadphones.Weigth.Value |> float // Must be updated to ShipingWeight
                AverageReviews = 4.2 // Update xml with AverageReview field
                Dimensions = {
                    Heigth = dbHeadphones.Heigth.Value
                    Width = dbHeadphones.Width.Value
                    Depth = Some (dbHeadphones.Depth.Value)
                }
                Price = dbHeadphones.Price.Value
                Color = dbHeadphones.Color.Value |> DbProductUtils.getColorFromDbProduct
                Brand = dbHeadphones.Manufacturer.Name |> DbProductUtils.getBrandFromDbProduct
            }
        | ReadingMaterial dbBook ->
            {
                Name = dbBook.Name.Value
                Weight = dbBook.ShippingWeight.Value |> float // Add weight field for books...
                ShippingWeight = dbBook.ShippingWeight.Value |> float
                AverageReviews = dbBook.ReviewAverage.Value |> float
                Dimensions = {
                    // Add dimensions to book definition
                    Heigth = 1.00m
                    Width = 1.00m
                    Depth = Some 1.00m
                }
                Price = dbBook.Price.Value
                Color = Red // Provide book color in definition
                Brand = Toshiba //Waiting for up book publisher companies in definition
            }
        | Computer computerDb ->
            {
                Name = computerDb.Model.Series + " " + computerDb.Model.Number
                Weight = computerDb.Weight.Value |> float
                ShippingWeight = computerDb.Weight.Value |> float // Add shipping weight field for computers...
                AverageReviews = 4.5 // reviews
                Dimensions = {
                    // Add dimensions to book definition
                    Heigth = computerDb.Height.Value |> decimal
                    Width = computerDb.Height.Value |> decimal
                    Depth = Some (computerDb.Height.Value |> decimal)
                }
                Price = computerDb.Price.Value
                Color = computerDb.Color.Value |>DbProductUtils.getColorFromDbProduct // Provide book color in definition
                Brand = computerDb.Manufacturer |> DbProductUtils.getBrandFromDbProduct
            }

let getStoreHeadphones storeProductList =
    Array.map(fun h ->
        let headphonesInfo = Headphones(h) |> getProductInfoFromProvider
        let product = {
            Details = headphonesInfo
            Fit = DbProductUtils.getFitFromHeadphones h
            BatteryLife = Some(h.BatteryLife.Value |> sbyte)
            ReleaseDate = h.ReleaseDate.Value
            AreWireless = h.IsWireless.Value
            IsNoiseCancelActive = h.IsNoiseCancelled.Value
        }
        WirelessHeadphones(product, "WireLessId")
    )
    >> Array.append storeProductList

let getStoreBooks storeProductList =
    Array.map(fun b ->
        let bookInfo = ReadingMaterial(b) |> getProductInfoFromProvider
        let product = {
            Details = bookInfo
            AuthorName = b.AuthorName.Value
            Format = KindleVersion // TODO: Missing format from XML document
            ISBN = "" // TODO: Missing ISBN from XML document
            Summary = "" // TODO: Missing Summary from XML document
            PageCount = b.PageCount.Value
            Language = English
            Category = Fantasy // TODO: Missing function to convert from the XML document
            ReleasedDate = DateTime.Now // TODO: Missing release date from XML document
        }
        Book(product, "SomeBookId")
    )
    >> Array.append storeProductList

let getStoreComputers storeProductList =
    Array.map(fun computer ->
        let computerInfo = Computer(computer) |> getProductInfoFromProvider
        let computerCpu = {
            Details = computerInfo // Need function to retrieve it from XmlProvider<...>.Computer
            CoreCount = 4uy // Need function to retrieve it from XmlProvider<...>.Computer
            Series = Intel(IntelCorei7) // Need function to retrieve it from XmlProvider<...>.Computer
            ProcessorSpeed = 3.2 // Need function to retrieve it from XmlProvider<...>.Computer
            OverclockedSpeed = 5.2 // Need function to retrieve it from XmlProvider<...>.Computer
            Wattage = 80 // Need function to retrieve it from XmlProvider<...>.Computer
            YearModel = DateTime.Today // Need function to retrieve it from XmlProvider<...>.Computer + Should be int, not DateTime
        }
        let product = {
            Details = computerInfo
            Resolution = HighDefinition 1920
            Cpu = computerCpu
            Ram = 8192 // Need conversion for RAM
            CacheMemory = None // Need to specify field in computer + function to retrieve it from the generated type
            DdrRam = None
            RunningOperatingSystem = Windows10
            DeviceInputs = []
        }
        Laptop(product, "LaptopId")
    )
    >> Array.append storeProductList

// TODO: Make this more readable for beginners.
let loadStoreProducts (storeItems: ProductDb.Items)  =
    [||]
    |> getStoreHeadphones <| storeItems.Headphones
    |> getStoreBooks <| storeItems.Books
    |> getStoreComputers <| storeItems.Computers