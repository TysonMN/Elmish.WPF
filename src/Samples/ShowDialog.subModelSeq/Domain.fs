module Domain
open System

type FieldId = FieldId of Guid

type MessageType = { 
    ID: int
    Name: string
}

type FieldData = { // Leaf data
    Id: FieldId
    Name: string
    Type: string
    IsGml: bool
    IsCml: bool
    IsCmlChangeField: bool
    IsCmlEntity: bool
}

type Field =  { 
    Data: FieldData
    Fields: Field list
}
    with 
        member this.IsEmpty = this.Data.Type = ""

type Model = { 
    MsgType: MessageType
    DummyRoot: Field
}
    with 
        member this.ParentStruct = this.DummyRoot.Fields.Head
        member this.IsEmpty = this.ParentStruct.IsEmpty
