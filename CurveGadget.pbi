; Program: CurveGadget.pbi
; Author: intratec, modified by Demivec, Little John and Danilo
; version: 13
;
; History:
;
; 13 (infratec) bugfix for the middle point in a vertical row
; 12 (Danilo) added #CurveGadget_MinXSpacing
; 11 (infratec) added CurveGadgetSetState()
; 10 (infratec) added #CurveGadget_Y_Only, #CurveGadget_DrawLines and #CurveGadget_DrawBars
;  9 (infratec) bugfix for placing points on top of each other
;  8 (Demivec) code optimization
;    (infratec) set catchRadius when pointRadius is changed
;    renamed everything from filterBoxGadget to CurveGadget
;    small cosmetic changes
;  7 (infratec) added attribute #FilterBox_Points
;  6 (infratec) added attribute #FilterBox_PointRadius and #FilterBox_CatchRadius
;  5 (infratec) added filterBoxGadgetSetAttribut for scaling the result
;    make it impossible to place illegal X values (hopefully)
;  4 (Little John) changed cursor on mouse over
;    (Demivec) simplified code
;  3 (infratec) added filterBoxGadgetSetColor
;  2 (Demivec) changed cursor on pick up
;    fixed mouse move event
;    more flexible with #FilterBoxPoint... defines
;  1 (infratec) initial version

;EnableExplicit

CompilerIf Defined(PB_Gadget_DisableColor, #PB_Constant) = 0
  #PB_Gadget_DisableColor = 20
CompilerEndIf

Enumeration
  #CurveGadget_Points
  #CurveGadget_PointRadius
  #CurveGadget_CatchRadius
  #CurveGadget_X_Maximum
  #CurveGadget_Y_Maximum
  #CurveGadget_Y_Only
  #CurveGadget_DrawLines
  #CurveGadget_DrawBars
  #CurveGadget_MinXSpacing
EndEnumeration

Structure CurveGadgetStr
  List dataPoint.Point()
  width.i
  height.i
  disabled.i
  activePoint.i
  frontColor.i
  backColor.i
  disableColor.i
  xScaleMax.f
  yScaleMax.f
  pointRadius.i
  catchRadius.i
  points.i
  yOnly.i
  drawLines.i
  drawBars.i
  minXSpacing.i
EndStructure


Procedure CurveGadgetDraw(gadgetNo)
  Protected *CurveGadget.CurveGadgetStr
  Protected i, x1, y1
 
  If IsGadget(gadgetNo)
    *CurveGadget = GetGadgetData(gadgetNo)
    With *CurveGadget
      If StartDrawing(CanvasOutput(gadgetNo))
          If \disabled
            Box(0, 0, \width, \height, \disableColor)
          Else
            Box(0, 0, \width, \height, \backColor)
          EndIf
          ResetList(\dataPoint())
          NextElement(\dataPoint())
          i = 0
          While i < \points
            x1 = \dataPoint()\x
            y1 = \dataPoint()\y
            Circle(x1, y1, \pointRadius, \frontColor)
            NextElement(\dataPoint())
            If \drawLines
              LineXY(x1, y1, \dataPoint()\x, \dataPoint()\y, \frontColor)
            EndIf
            If \drawBars
              Box(x1 - \pointRadius, y1, \pointRadius * 2 + 1, \height - y1, \frontColor)
            EndIf
            i + 1
          Wend
        StopDrawing()
      EndIf
    EndWith
  EndIf
EndProcedure


Procedure CurveGadgetSetAttribute(gadgetNo, attribute, value)
  Protected *CurveGadget.CurveGadgetStr
  Protected i.i
  
  If IsGadget(gadgetNo)
    *CurveGadget = GetGadgetData(gadgetNo)
    With *CurveGadget
      Select attribute
        Case #CurveGadget_Points
          If value < 2 : value = 2 : EndIf
          \points = value
          ClearList(\dataPoint())
          AddElement(\dataPoint())
          \dataPoint()\x = 0
          \dataPoint()\y = \height / 2
          For i = 1 To \points - 2
            AddElement(\dataPoint())
            \dataPoint()\x = ((0.0 + \width - \pointRadius) / (\points - 1)) * i
            \dataPoint()\y = \height / 2
          Next
          AddElement(\dataPoint())
          \dataPoint()\x = \width - 1
          \dataPoint()\y = \height / 2
        Case #CurveGadget_PointRadius
          \pointRadius = value
          If \catchRadius < value : \catchRadius = value : EndIf
        Case #CurveGadget_CatchRadius : \catchRadius = value
        Case #CurveGadget_X_Maximum : \xScaleMax = value
        Case #CurveGadget_Y_Maximum : \yScaleMax = value
        Case #CurveGadget_Y_Only : \yOnly = value
        Case #CurveGadget_DrawLines
          If value
            \drawLines = #True
            \drawBars = #False
          Else
            \drawLines = #False
          EndIf
        Case #CurveGadget_DrawBars
          If value
            \drawBars = #True
            \drawLines = #False
          Else
            \drawBars = #False
          EndIf
        Case #CurveGadget_MinXSpacing : \minXSpacing = value
      EndSelect
    EndWith
    CurveGadgetDraw(gadgetNo)
  EndIf
EndProcedure


Procedure CurveGadget(gadgetNo, x, y, width, height, points = 5, fgc = $0000FF, bgc = $FFFFFF, dc = $E0E0E0)
  Protected *CurveGadget.CurveGadgetStr
  Protected i
 
  If gadgetNo = #PB_Any Or IsGadget(gadgetNo) = 0
    gadgetNo = CanvasGadget(#PB_Any, x, y, width, height, #PB_Canvas_Border)
    If gadgetNo
      *CurveGadget = AllocateMemory(SizeOf(CurveGadgetStr))
      SetGadgetData(gadgetNo, *CurveGadget)
     
      ;gadget output will be smaller if borders are drawn
      StartDrawing(CanvasOutput(gadgetNo))
        width = OutputWidth()
        height = OutputHeight()
      StopDrawing()
      With *CurveGadget
        \width = width
        \height = height
        \disabled = #False
        \activePoint = -1
        \frontColor = fgc
        \backColor = bgc
        \disableColor = dc
        \xScaleMax = width
        \yScaleMax = height
        \pointRadius = 5
        \catchRadius = 5
        \points = points
        \yOnly = #False
        \drawLines = #True
        \drawBars = #False
        \minXSpacing = 0
        
        NewList \dataPoint.POINT()
        CurveGadgetSetAttribute(gadgetNo, #CurveGadget_Points, points)
      EndWith
    EndIf
  EndIf
 
  CurveGadgetDraw(gadgetNo)
 
  ProcedureReturn gadgetNo
EndProcedure


Procedure CurveGadgetPointCheck(*CurveGadget.CurveGadgetStr, xPos, yPos)
  ;returns the number of the point at (xPos, yPos) for the given filterBox structure
  ;returns -1 if no point present at (xPos, yPos)
  Protected i
  
  With *CurveGadget
    i = 0
    ForEach \dataPoint()
      If (\dataPoint()\x - \catchRadius) < xPos And (\dataPoint()\x + \catchRadius) > xPos
        If (\dataPoint()\y - \catchRadius) < yPos And (\dataPoint()\y + \catchRadius) > yPos
          ProcedureReturn i ;found a point
        EndIf
      EndIf
      i + 1
    Next
  EndWith
 
  ProcedureReturn -1 ;no point at (xPos, yPos)
EndProcedure


Procedure CurveGadgetEvent(gadgetNo)
  
  Enumeration
    #TwoPointsBefore
    #PointBefore
    #Point
    #PointAfter
    #TwoPointsAfter
  EndEnumeration
  
  Protected *CurveGadget.CurveGadgetStr
  Protected result
  Protected i, xPos, yPos
  
  *CurveGadget = GetGadgetData(gadgetNo)
  
  With *CurveGadget
    If Not \disabled
      xPos = GetGadgetAttribute(gadgetNo, #PB_Canvas_MouseX)
      yPos = GetGadgetAttribute(gadgetNo, #PB_Canvas_MouseY)
      
      If xPos < 0 : xPos = 0 : EndIf
      If xPos > \width - 1: xPos = \width - 1: EndIf
      If yPos < 0 : yPos = 0 : EndIf
      If yPos > \height - 1 : yPos = \height - 1 : EndIf
      
      Select EventType()
        Case #PB_EventType_LeftButtonDown
          If \activePoint = -1
            \activePoint = CurveGadgetPointCheck(*CurveGadget, xPos, yPos)
          EndIf
          
        Case #PB_EventType_MouseMove
          If \activePoint <> -1
            
            Protected Dim pointArray.POINT(4)
            
            i = 0
            ForEach \dataPoint()
              If i = \activePoint - 2
                pointArray(#TwoPointsBefore)\x = \dataPoint()\x
                pointArray(#TwoPointsBefore)\y = \dataPoint()\y
              ElseIf i = \activePoint - 1
                pointArray(#PointBefore)\x = \dataPoint()\x
                pointArray(#PointBefore)\y = \dataPoint()\y
              ElseIf i = \activePoint
                pointArray(#Point)\x = \dataPoint()\x
                pointArray(#Point)\y = \dataPoint()\y
              ElseIf i = \activePoint + 1
                pointArray(#PointAfter)\x = \dataPoint()\x
                pointArray(#PointAfter)\y = \dataPoint()\y
              ElseIf i = \activePoint + 2
                pointArray(#TwoPointsAfter)\x = \dataPoint()\x
                pointArray(#TwoPointsAfter)\y = \dataPoint()\y
                Break
              EndIf
              i + 1
            Next
            
            If \activePoint > 0 And \activePoint < \points - 1
              
              ; the y-stuff, when a point is in the middle of a vertical line
              If \activePoint > 1 And \activePoint < \points - 2
                If pointArray(#PointBefore)\x = pointArray(#Point)\x And pointArray(#PointAfter)\x = pointArray(#Point)\x
                  If yPos > pointArray(#PointAfter)\y - \pointRadius : yPos = pointArray(#PointAfter)\y - \pointRadius : EndIf
                  If yPos < pointArray(#PointBefore)\y + \pointRadius : yPos = pointArray(#PointBefore)\y + \pointRadius : EndIf
                EndIf
              EndIf
              
              
              If xPos < \pointRadius: xPos = \pointRadius : EndIf
              If xPos > \width - \pointRadius: xPos = \width - \pointRadius : EndIf
              
              If xPos < pointArray(#PointBefore)\x + \minXSpacing
                xPos = pointArray(#PointBefore)\x + \minXSpacing
                
                If yPos > pointArray(#PointBefore)\y - \pointRadius And yPos < pointArray(#PointBefore)\y + \pointRadius
                  xPos + \pointRadius
                EndIf
                
                If \activePoint > 2
                  If xPos = pointArray(#TwoPointsBefore)\x
                    If pointArray(#TwoPointsBefore)\y - pointArray(#PointBefore)\y > 0
                      If yPos > pointArray(#PointBefore)\y
                        xPos + \pointRadius
                      EndIf
                    Else
                      If yPos < pointArray(#PointBefore)\y
                        xPos + \pointRadius
                      EndIf
                    EndIf
                  EndIf
                EndIf
              EndIf
              
              If xPos > pointArray(#PointAfter)\x - \minXSpacing
                xPos = pointArray(#PointAfter)\x - \minXSpacing
                
                If yPos < pointArray(#PointAfter)\y + \pointRadius And yPos > pointArray(#PointAfter)\y - \pointRadius
                  xPos - \pointRadius
                EndIf
                
                If \activePoint < \points - 2
                  If xPos = pointArray(#TwoPointsAfter)\x
                    If pointArray(#TwoPointsAfter)\y - pointArray(#PointAfter)\y > 0
                      If yPos > pointArray(#PointAfter)\y
                        xPos - \pointRadius
                      EndIf
                    Else
                      If yPos < pointArray(#PointAfter)\y
                        xPos - \pointRadius
                      EndIf
                    EndIf
                  EndIf
                EndIf
              EndIf
              
            Else
              If \activePoint = 0
                xPos = 0
              Else
                xPos = \width
              EndIf
            EndIf
            
            i = 0
            ForEach \dataPoint()
              If i = \activePoint
                If Not \yOnly
                  \dataPoint()\x = xPos
                EndIf
                \dataPoint()\y = yPos
                Break
              EndIf
              i + 1
            Next
            CurveGadgetDraw(gadgetNo)
            
          Else
            If CurveGadgetPointCheck(*CurveGadget, xPos, yPos) >= 0
              SetGadgetAttribute(gadgetNo, #PB_Canvas_Cursor, #PB_Cursor_Hand)
            Else
              SetGadgetAttribute(gadgetNo, #PB_Canvas_Cursor, #PB_Cursor_Default)
            EndIf
          EndIf
          
        Case #PB_EventType_LeftButtonUp
          If \activePoint <> -1
            \activePoint = -1
            result = 1
          EndIf
      EndSelect
    Else
      SetGadgetAttribute(gadgetNo, #PB_Canvas_Cursor, #PB_Cursor_Denied)
    EndIf
  EndWith
  
  ProcedureReturn result
EndProcedure


Procedure CurveGadgetDisable(gadgetNo, state)
  Protected *CurveGadget.CurveGadgetStr
 
  *CurveGadget = GetGadgetData(gadgetNo)
  If state
    If Not *CurveGadget\disabled
      *CurveGadget\disabled = #True
      CurveGadgetDraw(gadgetNo)
    EndIf
  Else
    If *CurveGadget\disabled
      *CurveGadget\disabled = #False
      CurveGadgetDraw(gadgetNo)
    EndIf
  EndIf
EndProcedure


Procedure CurveGadgetGetState(gadgetNo, Array dataPoint.POINT(1))
  Protected *CurveGadget.CurveGadgetStr
  Protected result, i
  
  result = #False
  
  If IsGadget(gadgetNo)
    *CurveGadget = GetGadgetData(gadgetNo)
    With *CurveGadget
      Dim dataPoint(\points - 1)
      i = 0
      ForEach \dataPoint()
        dataPoint(i)\x = \xScaleMax * \dataPoint()\x / \width
        dataPoint(i)\y = \yScaleMax - (\yScaleMax * \dataPoint()\y / \height)
        i + 1
      Next
    EndWith
    result = #True
  EndIf
  
  ProcedureReturn result
EndProcedure


Procedure CurveGadgetSetState(gadgetNo, Array dataPoint.POINT(1))
  Protected *CurveGadget.CurveGadgetStr
  Protected i.i
  
  If IsGadget(gadgetNo)
    *CurveGadget = GetGadgetData(gadgetNo)
    With *CurveGadget
      LSize = ListSize(\dataPoint()) - 1
      If LSize = ArraySize(dataPoint())
        i = 0
        ForEach \dataPoint()
          If i > 0 And i < LSize
            \dataPoint()\x = dataPoint(i)\x
          EndIf
          \dataPoint()\y = \height - dataPoint(i)\y
          i + 1
        Next
      EndIf
    EndWith
    CurveGadgetDraw(gadgetNo)
  EndIf
  
EndProcedure


Procedure CurveGadgetSetColor(gadgetNo, attribute, value)
  Protected *CurveGadget.CurveGadgetStr
 
  If IsGadget(gadgetNo)
    *CurveGadget = GetGadgetData(gadgetNo)
    With *CurveGadget
      Select attribute
        Case #PB_Gadget_FrontColor : \frontColor = value
        Case #PB_Gadget_BackColor : \backColor = value
        Case #PB_Gadget_DisableColor : \disableColor = value
      EndSelect
    EndWith
    CurveGadgetDraw(gadgetNo)
  EndIf
EndProcedure
; IDE Options = PureBasic 6.02 LTS (Windows - x64)
; CursorPosition = 433
; FirstLine = 428
; Folding = --
; EnableXP
; DPIAware