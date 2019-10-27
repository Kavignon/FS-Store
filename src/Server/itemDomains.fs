module ItemDomain

open Common
open Shared
open System

type GeneratedTypeFromStore =
    | Headphones                of value: ProductDb.Headphone
    | ReadingMaterial           of value: ProductDb.Book
    | Computer                  of value: ProductDb.Computer

let bookCategory (book: ProductDb.Book)=
    match book.Category.Value with
    | Prefix "Fan" _ -> Some Fantasy
    | Prefix "Comp" _ & Suffix "Sciences" _ -> Some ``Computer Science``
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


let getProductInfoFromProvider providerType =
        match providerType with
        | Headphones dbHeadphones ->
            {
                Name = dbHeadphones.Model.Name
                Weight = dbHeadphones.Weigth.Value |> float
                ShippingWeight = dbHeadphones.Weigth.Value |> float // Must be updated to ShipingWeight
                AverageReviews = 4.2 // Update xml with AverageReview field
                Dimensions = {
                    Heigth = float dbHeadphones.Heigth.Value
                    Width = float dbHeadphones.Width.Value
                    Depth = Some (float dbHeadphones.Depth.Value)
                }
                Price = float dbHeadphones.Price.Value
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
                    Heigth = 1.00
                    Width = 1.00
                    Depth = Some 1.00
                }
                Price = float dbBook.Price.Value
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
                    Heigth = float computerDb.Height.Value
                    Width = float computerDb.Height.Value
                    Depth = Some (float computerDb.Height.Value)
                }
                Price = float computerDb.Price.Value
                Color = computerDb.Color.Value |>DbProductUtils.getColorFromDbProduct // Provide book color in definition
                Brand = computerDb.Manufacturer |> DbProductUtils.getBrandFromDbProduct
            }


let getStoreHeadphones headphones storeProductList =
    headphones
    |> Array.map(fun h ->
        let headphonesInfo = Headphones(h) |> getProductInfoFromProvider
        let product = {
            Details = headphonesInfo
            Fit = DbProductUtils.getFitFromHeadphones h
            BatteryLife = Some(h.BatteryLife.Value)
            ReleaseDate = h.ReleaseDate.Value
            AreWireless = h.IsWireless.Value
            IsNoiseCancelActive = h.IsNoiseCancelled.Value
        }
        WirelessHeadphones(product, "WireLessId")
    )
    |> Array.append storeProductList

let getStoreBooks books storeProductList =
    books
    |> Array.map(fun b ->
        let bookInfo = ReadingMaterial(b) |> getProductInfoFromProvider
        let product = {
            Details = bookInfo
            AuthorName = b.AuthorName.Value
            Format = KindleVersion // Missing format from XML document
            ISBN = "" // Missing ISBN from XML document
            Summary = "" // Missing Summary from XML document
            PageCount = b.PageCount.Value
            Language = English
            Category = Fantasy // Missing function to convert from the XML document
            ReleasedDate = DateTime.Now // Missing release date from XML document
        }
        Book(product, "SomeBookId")
    )
    |> Array.append storeProductList

let getStoreComputers computers storeProductList =
    computers
    |> Array.map(fun computer ->
        let computerInfo = Computer(computer) |> getProductInfoFromProvider
        let computerCpu = {
            Details = computerInfo // Need function to retrieve it from XmlProvider<...>.Computer
            CoreCount = 4 // Need function to retrieve it from XmlProvider<...>.Computer
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
    |> Array.append storeProductList

let loadStoreProducts (storeItems: ProductDb.Items)  =
    [||]
    |> getStoreHeadphones storeItems.Headphones
    |> getStoreBooks storeItems.Books
    |> getStoreComputers storeItems.Computers