namespace FsMal

module Env =
    open System.Collections
    open Error
    open Types
    open Utils

    type Data = Generic.Dictionary<string, Form>
    type Env = { parent: Env option;data: Data }

    let makeEmpty () = { parent = None;data = Data() }

    let set env key value =
        env.data.[key] <- value
        env

    let fork env = { parent = Some env;data = Data() }

    let rec find env key =
        match env.data.GetOption key with
        | Some _ -> Some env
        | None ->
            match env.parent with
            | Some env' -> find env' key
            | None -> None

    let tryGet env key =
        option {
            let! env' = find env key
            return! env'.data.GetOption key
        }

    let get env key =
        match tryGet env key with
        | None -> raise (symbolNotFound key)
        | Some v -> v
