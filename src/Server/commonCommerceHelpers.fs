module Common

open System
open FSharp.Data

type ProductDb = XmlProvider<"./CommerceDb.xml"> // Move the data to Azure.
let productsFromDatabase = ProductDb.GetSample()

[<Literal>]
let BlackColor = "Black"

[<Literal>]
let WhiteColor = "White"

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let (|Suffix|_|) (p:string) (s:string) =
    if s.EndsWith(p) then
        Some(String.Empty)
    else
        None