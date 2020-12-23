namespace Calculator

module Calculator =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout

    type Operation = 
        | Unknown
        | Add
        | Subtract
    
    type State = { Num1 : string 
                   Num2 : string
                   Oper : Operation
                   Disp : string}
    let init = { Num1 = "0"
                 Num2 = "0"
                 Oper = Unknown
                 Disp = "0" }

    type Msg = 
        | Zero
        | One
        | Two
        | Three
        | Four
        | Five
        | Six 
        | Seven 
        | Eight 
        | Nine 
        | Reset 
        | Add 
        | Subtract
        | Evaluate

    let update (msg: Msg) (state: State) : State =
        match msg with
        | Zero -> 
            { state with Disp = if state.Disp = "0" then "0" else state.Disp + "0"}
        | One -> 
            { state with Disp = if state.Disp = "0" then "1" else state.Disp + "1"}
        | Two -> 
            { state with Disp = if state.Disp = "0" then "2" else state.Disp + "2"}
        | Three -> 
            { state with Disp = if state.Disp = "0" then "3" else state.Disp + "3"}
        | Four -> 
            { state with Disp = if state.Disp = "0" then "4" else state.Disp + "4"}
        | Five -> 
            { state with Disp = if state.Disp = "0" then "5" else state.Disp + "5"}
        | Six -> 
            { state with Disp = if state.Disp = "0" then "6" else state.Disp + "6"}
        | Seven -> 
            { state with Disp = if state.Disp = "0" then "7" else state.Disp + "7"}
        | Eight -> 
            { state with Disp = if state.Disp = "0" then "8" else state.Disp + "8"}
        | Nine -> 
            { state with Disp = if state.Disp = "0" then "9" else state.Disp + "9"}
        | Reset -> 
            init
        | Add -> 
            init
            // { state with Disp = "add" }
        | Subtract -> 
            init
        | Evaluate -> 
            init
    
    let view (state: State) (dispatch) =
        DockPanel.create [
            DockPanel.children [
                DockPanel.create [
                    DockPanel.dock Dock.Top
                    DockPanel.height 100.0
                    DockPanel.children [
                        TextBlock.create [
                            TextBlock.fontSize 60.0
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.horizontalAlignment HorizontalAlignment.Center    
                            TextBlock.text (string state.Disp)
                        ]
                    ]
                ]
                DockPanel.create [
                    DockPanel.dock Dock.Bottom
                    DockPanel.height 400.0
                    DockPanel.children [
                        DockPanel.create [
                            DockPanel.dock Dock.Top
                            DockPanel.horizontalAlignment HorizontalAlignment.Center
                            DockPanel.height 100.0
                            DockPanel.children [
                                Button.create [
                                    Button.onClick (fun _ -> dispatch One)
                                    Button.content "1"
                                    Button.fontSize 50.0
                                    Button.width 95.0
                                    Button.height 95.0
                                    Button.margin (2.5, 2.5)
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.horizontalAlignment HorizontalAlignment.Center  
                                ]
                                Button.create [
                                    Button.onClick (fun _ -> dispatch Two)
                                    Button.content "2"
                                    Button.fontSize 50.0
                                    Button.width 95.0
                                    Button.height 95.0
                                    Button.margin (2.5, 2.5)
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.horizontalAlignment HorizontalAlignment.Center 
                                ]
                                Button.create [
                                    Button.onClick (fun _ -> dispatch Three)
                                    Button.content "3"
                                    Button.fontSize 50.0
                                    Button.width 95.0
                                    Button.height 95.0
                                    Button.margin (2.5, 2.5)
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.horizontalAlignment HorizontalAlignment.Center 
                                ]
                                Button.create [
                                    Button.onClick (fun _ -> dispatch Evaluate)
                                    Button.content "="
                                    Button.fontSize 50.0
                                    Button.width 95.0
                                    Button.height 95.0
                                    Button.margin (2.5, 2.5)
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.horizontalAlignment HorizontalAlignment.Center 
                                ]
                            ]
                        ]
                        DockPanel.create [
                            DockPanel.dock Dock.Top
                            DockPanel.horizontalAlignment HorizontalAlignment.Center
                            DockPanel.height 100.0
                            DockPanel.children [
                                Button.create [
                                    Button.onClick (fun _ -> dispatch Four)
                                    Button.content "4"
                                    Button.fontSize 50.0
                                    Button.width 95.0
                                    Button.height 95.0
                                    Button.margin (2.5, 2.5)
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.horizontalAlignment HorizontalAlignment.Center  
                                ]
                                Button.create [
                                    Button.onClick (fun _ -> dispatch Five)
                                    Button.content "5"
                                    Button.fontSize 50.0
                                    Button.width 95.0
                                    Button.height 95.0
                                    Button.margin (2.5, 2.5)
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.horizontalAlignment HorizontalAlignment.Center 
                                ]
                                Button.create [
                                    Button.onClick (fun _ -> dispatch Six)
                                    Button.content "6"
                                    Button.fontSize 50.0
                                    Button.width 95.0
                                    Button.height 95.0
                                    Button.margin (2.5, 2.5)
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.horizontalAlignment HorizontalAlignment.Center 
                                ]
                                Button.create [
                                    Button.onClick (fun _ -> dispatch Add)
                                    Button.content "+"
                                    Button.fontSize 50.0
                                    Button.width 95.0
                                    Button.height 95.0
                                    Button.margin (2.5, 2.5)
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.horizontalAlignment HorizontalAlignment.Center 
                                ]
                            ]
                        ]
                        DockPanel.create [
                            DockPanel.dock Dock.Top
                            DockPanel.horizontalAlignment HorizontalAlignment.Center
                            DockPanel.height 100.0
                            DockPanel.children [
                                Button.create [
                                    Button.onClick (fun _ -> dispatch Seven)
                                    Button.content "7"
                                    Button.fontSize 50.0
                                    Button.width 95.0
                                    Button.height 95.0
                                    Button.margin (2.5, 2.5)
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.horizontalAlignment HorizontalAlignment.Center  
                                ]
                                Button.create [
                                    Button.onClick (fun _ -> dispatch Eight)
                                    Button.content "8"
                                    Button.fontSize 50.0
                                    Button.width 95.0
                                    Button.height 95.0
                                    Button.margin (2.5, 2.5)
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.horizontalAlignment HorizontalAlignment.Center 
                                ]
                                Button.create [
                                    Button.onClick (fun _ -> dispatch Nine)
                                    Button.content "9"
                                    Button.fontSize 50.0
                                    Button.width 95.0
                                    Button.height 95.0
                                    Button.margin (2.5, 2.5)
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.horizontalAlignment HorizontalAlignment.Center 
                                ]
                                Button.create [
                                    Button.onClick (fun _ -> dispatch Subtract)
                                    Button.content "-"
                                    Button.fontSize 50.0
                                    Button.width 95.0
                                    Button.height 95.0
                                    Button.margin (2.5, 2.5)
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.horizontalAlignment HorizontalAlignment.Center 
                                ]
                            ]
                        ]
                        DockPanel.create [
                            DockPanel.dock Dock.Top
                            DockPanel.horizontalAlignment HorizontalAlignment.Center
                            DockPanel.height 100.0
                            DockPanel.children [
                                Button.create [
                                    Button.onClick (fun _ -> dispatch Zero)
                                    Button.content "0"
                                    Button.fontSize 50.0
                                    Button.width 195.0
                                    Button.height 95.0
                                    Button.margin (2.5, 2.5)
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.horizontalAlignment HorizontalAlignment.Center  
                                ]
                                Button.create [
                                    Button.onClick (fun _ -> dispatch Reset)
                                    Button.content "C"
                                    Button.fontSize 50.0
                                    Button.width 195.0
                                    Button.height 95.0
                                    Button.margin (2.5, 2.5)
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.horizontalAlignment HorizontalAlignment.Center 
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]       

// 1|2|3|=
// 4|5|6|+
// 7|8|9|-
//  0 | c

        // Button.create [
        //             Button.dock Dock.Bottom
        //             Button.onClick (fun _ -> dispatch Reset)
        //             Button.content "reset"
        //         ]                
        //         Button.create [
        //             Button.dock Dock.Bottom
        //             Button.onClick (fun _ -> dispatch Decrement)
        //             Button.content "-"
        //         ]
        //         Button.create [
        //             Button.dock Dock.Bottom
        //             Button.onClick (fun _ -> dispatch Increment)
        //             Button.content "+"
        //         ]
        //         TextBlock.create [
        //             TextBlock.dock Dock.Top
        //             TextBlock.fontSize 48.0
        //             TextBlock.verticalAlignment VerticalAlignment.Center
        //             TextBlock.horizontalAlignment HorizontalAlignment.Center
        //             TextBlock.text (string state.count)
        //         ]

