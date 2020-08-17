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

//type Field =  { 
//    Data: FieldData
//    Fields: Field list
//}
//    with 
//        member this.IsEmpty = this.Data.Type = ""

type Model = { 
    MsgType: MessageType
    DummyRoot: RoseTree<FieldData>
}
    with 
        member this.ParentStruct = this.DummyRoot.Children.Head
        member this.IsEmpty = this.ParentStruct.Data.Type = ""
