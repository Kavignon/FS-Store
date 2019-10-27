module Database

open Shared
open FSharp.Data
open LiteDB
open LiteDB.FSharp
open System.IO

open ItemDomain

type ProductDb = XmlProvider<"./CommerceDb.xml">
let productsFromDatabase = ProductDb.GetSample() // TODO: Look for sample product feeds (should be XML)

let mapper = FSharpBsonMapper()

// TODO: have a serialized version of the models...
let private initializeDb () = 
    printfn "initializing db"
    let connectionString = ConnectionString("simple.db")
    connectionString.Mode <- FileMode.Exclusive
    use db = new LiteDatabase(connectionString, mapper)
    // let fileItems = loadStoreProducts productsFromDatabase
    // let products = fileItems |> Array.mapi(fun i item -> { Id = i; Item = item })
    printfn "Got store products collection"
    let storeProducts = db.GetCollection<TestSerializeProduct>("store_products")
    // TODO: Talk about thoth -> took a while to find decimal error (SAFE Template)
    // Issue during mentorship program - basic .net type but we spent too much time on this
    // A Can we improve error message and what's your recommendation 
    // Provide good support for the errors
    let t1 = { Id =1 ; Value = loadStoreProducts productsFromDatabase  }
    let t2 = { Id =2 ; Value = loadStoreProducts productsFromDatabase}
    let products = [ t1 ; t2]
    printfn "Inserting products %A" products
    storeProducts.Insert(products) |> ignore

// (DEMO) Here db will never be initialized without hydrating the NoSQL db with the data from the file.
let getDb () =
    let connectionString = ConnectionString("simple.db")
    connectionString.Mode <- FileMode.Exclusive
    if not <| File.Exists("simple.db") then
        initializeDb ()
    new LiteDatabase(connectionString, mapper)

let getStoreItems() =
    use db = getDb ()
    let collection = db.GetCollection<TestSerializeProduct>("store_products")
    if isNull collection then printfn "Collection is null"
    else printfn "Collection is not null"
    let seq = collection.FindAll()
    printfn "%A" (Seq.length seq) 
    seq |> Seq.toArray 