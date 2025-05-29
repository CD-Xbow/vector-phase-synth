XIncludeFile "CurveGadget.pbi"

Dim CurvePoint.POINT(1)

OpenWindow(0, 0, 0, 430, 340, "CurveGadget Demo", #PB_Window_SystemMenu|#PB_Window_ScreenCentered)

No1 = CurveGadget(#PB_Any, 10, 10, 200, 100)
No2 = CurveGadget(#PB_Any, 10, 120, 200, 100)
No3 = CurveGadget(#PB_Any, 220, 120, 200, 100)
No4 = CurveGadget(#PB_Any, 10, 230, 200, 100)
No5 = CurveGadget(#PB_Any, 220, 230, 200, 100, 7)

CurveGadgetSetAttribute(No1, #CurveGadget_MinXSpacing, 20) ; test by Danilo

CurveGadgetSetColor(No2, #PB_Gadget_FrontColor, $FF0000)
CurveGadgetSetColor(No2, #PB_Gadget_BackColor, $00FFFF)

CurveGadgetSetAttribute(No2, #CurveGadget_Points, 8)
CurveGadgetSetAttribute(No2, #CurveGadget_PointRadius, 3)
CurveGadgetSetAttribute(No2, #CurveGadget_CatchRadius, 10)
CurveGadgetSetAttribute(No2, #CurveGadget_X_Maximum, 50)
CurveGadgetSetAttribute(No2, #CurveGadget_Y_Maximum, 20)

CurveGadgetSetAttribute(No3, #CurveGadget_Points, 2)

CurveGadgetSetAttribute(No4, #CurveGadget_Y_Only, #True)
CurveGadgetSetAttribute(No4, #CurveGadget_DrawLines, #False)
CurveGadgetSetAttribute(No4, #CurveGadget_DrawBars, #True)


CurveGadgetGetState(No5, CurvePoint())
CurvePoint(0)\x + 10    ; not possible
CurvePoint(1)\y + 10
CurvePoint(3)\y - 10
CurvePoint(4)\x + 10
CurvePoint(6)\y + 20
CurvePoint(6)\x - 20    ; not possible
CurveGadgetSetState(No5, CurvePoint())
CurveGadgetDisable(No5, #True)

Exit = #False
Repeat

  Event = WaitWindowEvent()

  Select Event
    Case #PB_Event_Gadget
      Select EventGadget()
        Case No1
          If CurveGadgetEvent(No1) = 1
            If CurveGadgetGetState(No1, CurvePoint())
              For i = 0 To ArraySize(CurvePoint())
                Debug "1: P" + Str(i + 1) + " (" + Str(CurvePoint(i)\x) + "/" + Str(CurvePoint(i)\y) + ")"
              Next i
            EndIf
          EndIf
        Case No2
          If CurveGadgetEvent(No2) = 1
            If CurveGadgetGetState(No2, CurvePoint())
              For i = 0 To ArraySize(CurvePoint())
                Debug "2: P" + Str(i + 1) + " (" + Str(CurvePoint(i)\x) + "/" + Str(CurvePoint(i)\y) + ")"
              Next i
            EndIf
          EndIf
        Case No3 : CurveGadgetEvent(No3)
        Case No4 : CurveGadgetEvent(No4)
        Case No5 : CurveGadgetEvent(No5)
      EndSelect
    Case #PB_Event_CloseWindow
      Exit = #True
  EndSelect

Until Exit
; IDE Options = PureBasic 6.02 LTS (Windows - x64)
; CursorPosition = 72
; EnableXP
; DPIAware