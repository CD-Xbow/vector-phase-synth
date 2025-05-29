;******Phase-Warper: A 2 Oscillator Vector Phase Synthesizer**********
;v1.01 created 2013 by DoctorNash (Jim Singh)
;Thanks to Einander for the dial (knobs) gadget, and midi code
;******************************************************

XIncludeFile "PortAudio.pb"

#SampleRate = 44100

#PI2 = 6.28318530717958

Global Pee.l ;tracks phase of the first oscillator
Global PeeLFO.l ;tracks phase of the second oscillator - even though it is called LFO

Global TheInitializer.l=0

Global Kata.s

Global PDVal.f

Global NumSamplePoints.l
Global LFOSamplePoints.l

Global Osc1FreqVal.d = 1200
Global Osc2FreqVal.d = 1200
Global OscMixLevel.d = 50


Enumeration  
  #GADGET_MainWin
  #WIN_MAIN
  #PlayPauseButton
  #MIDIDevice
  #Trackbar_Attack
  #Trackbar_Decay
  #Trackbar_Sustain
  #Trackbar_Release
  #Text_Attack
  #Text_Sustain
  #Text_Decay
  #Text_Release
  ;for the phase distored wave:
  #GADGET_FrameAmpEG
  #GADGET_FrameOsc1
  #GADGET_FrameOsc2
  #Trackbar_Osc1Freq
  #Trackbar_Osc2Freq
  #Trackbar_OscMix
  ;for phase vector algorithm:
  #Trackbar_Vee
  #Trackbar_Dee
  #Trackbar_Vee1
  #Trackbar_Dee1
  #GADGET_CheckGroup ;for first oscillator ADSR XY Matrix
  #GADGET_CheckGroup1 ;for second oscillator ADSR XY Matrix
  #XYFirst
  #XYSecond
  ;midi status display:
  #Text_MIDIStatus
EndEnumeration

Enumeration
  #Off
  #On
EndEnumeration
;
Structure KNOB
  Canv.I
  Size.L
  MinValue.L
  MaxValue.L
  Value.L
  xCenter.L
  yCenter.L
  RGB1.L
  RGB2.L
  LightRGB.L
  BkRGB.I
  Ang.F
  Info.L 
EndStructure
;
Define Instances=6,Index,Ev,Catch,Offx
Global Dim _KNOB.KNOB(Instances),_BkRGB=$55
;
Structure Pointf
  X.F : Y.F
EndStructure

;switches code
Global SwitchPos.l
Global OldSwitchPos.l
;end switches

;pushbutton1 code
Global Pressed.l=0
;pushbutton 2 code
Global Push2Pressed.l = 0
;pushbutton 3 code
Global Push3Pressed.l = 0

#PI2 = 6.28318530717958

Global State.i

Global SampleCounter.l

Global ForcedDecayCounter.l=0
Global Groucho.d=0
Global InitializeSampleCounter.l=0

;midi stuff
Global Chromatic$
Chromatic$="C C#D EbE F F#G AbA BbB C "
Global NoteVal.l
Global Freq.d

Global OldFreq.d=100

;ADSR parameters
Global Attack.d
Global Decay.d
Global Sustain.d
Global Release.d

;these are for the dials of ADSR

Global AttackRaw.d=240 ;this is 40 degrees on the dial, or val 12 out of 100. multiply val by 20 to get range upto 2000 seconds
Global DecayRaw.d = 1000 ;this is 70 degrees on the dial, or val 20 out of 100. multiply val by 50 to get range upto 5000 seconds
Global SustainRaw.d = 50 ;this is 180 degrees on the dial, or val 50 out of 100
Global ReleaseRaw.d = 3000; this is 215 degrees on the dial, or val 60 out of 100. multiply val by 50 to get range upto 5000 seconds

Global AttackSamples.d
Global DecaySamples.d
Global SustainLevel.d
Global ReleaseSamples.d

Global ADSRLevel.d
Global UptoV.d ;the ADSR level we have reached when Release strikes
Global UptoP.l ;samplecount we have reached when the Release strikes
Global UptoDeeVal.d ;the Dee value we have reached when the Release strikes for first oscillator
Global UptoVeeVal.d ;the Vee value we have reached when the Release strikes for first oscillator
Global UptoDeeVal1.d ;the Dee value we have reached when the Release strikes for second oscillator
Global UptoVeeVal1.d ;the Vee value we have reached when the Release strikes for second oscillator

;----------------now we do the XY 2D pads-----------------------

;dee and vee for the two oscillators
Global Dee.d = 50
Global Vee.d = 50
Global Dee1.d = 50
Global Vee1.d = 50

;these are for the Dee Vee ADSR XY matrix for the first oscillator
;dee goes from 0 To 100
Global AttackStartDee.d = 95
Global AttackEndDee.d = 70
Global SustainDee.d = 5
Global ReleaseDee.d =90

;vee goes from 0 to 500
Global AttackStartVee.d = 20
Global AttackEndVee.d = 500
Global SustainVee.d = 200
Global ReleaseVee.d = 400

;these are for the Dee Vee ADSR XY matrix for the second oscillator
;dee goes from 0 To 100
Global AttackStartDee1.d = 95
Global AttackEndDee1.d = 70
Global SustainDee1.d = 5
Global ReleaseDee1.d =90

;vee goes from 0 to 500
Global AttackStartVee1.d = 20
Global AttackEndVee1.d = 500
Global SustainVee1.d = 200
Global ReleaseVee1.d = 400

;these are for the Dee Vee ADSR XY matrix for the first oscillator
Global Osc1Point1X.d 
Global Osc1Point1Y.d 
Global Osc1Point2X.d 
Global Osc1Point2Y.d 
Global Osc1Point3X.d 
Global Osc1Point3Y.d 
Global Osc1Point4X.d 
Global Osc1Point4Y.d 

Global GroupTheDots.l = 0

;to match the Dee and Vee initial settings in the XY matrix, x axis is vee, y axis is dee. multiply y axis by 2 and divide x axis by 2.5
Osc1Point1X.d = AttackStartVee/2.5
Osc1Point1Y.d = AttackStartDee*2
Osc1Point2X.d = AttackEndVee/2.5
Osc1Point2Y.d = AttackEndDee*2
Osc1Point3X.d = SustainVee/2.5
Osc1Point3Y.d = SustainDee*2
Osc1Point4X.d = ReleaseVee/2.5
Osc1Point4Y.d = ReleaseDee*2

Global XYPointID.s = "Unpressed";this is so we know which point in the XY ADSR has been pressed so that when we drag the point it keeps up with the cursor

;these are for the Dee Vee ADSR XY matrix for the second oscillator
Global Osc2Point1X.d 
Global Osc2Point1Y.d 
Global Osc2Point2X.d 
Global Osc2Point2Y.d 
Global Osc2Point3X.d 
Global Osc2Point3Y.d 
Global Osc2Point4X.d 
Global Osc2Point4Y.d 

Global GroupTheDots1.l = 0

;to match the Dee and Vee initial settings in the XY matrix, x axis is vee, y axis is dee. multiply y axis by 2 and divide x axis by 2.5
Osc2Point1X.d = AttackStartVee1/2.5
Osc2Point1Y.d = AttackStartDee1*2
Osc2Point2X.d = AttackEndVee1/2.5
Osc2Point2Y.d = AttackEndDee1*2
Osc2Point3X.d = SustainVee1/2.5
Osc2Point3Y.d = SustainDee1*2
Osc2Point4X.d = ReleaseVee1/2.5
Osc2Point4Y.d = ReleaseDee1*2

Global XYPointID1.s = "Unpressed";this is so we know which point in the XY ADSR has been pressed so that when we drag the point it keeps up with the cursor

Declare.f PhaseDistorter(SampleNum.l, xval.f, aval.f)
Declare.f CalculateTheWaveform(SamplePoint.l)

;------------------------------------------------------------------
Macro MMk
  Abs(GetAsyncKeyState_(#VK_LBUTTON) +GetAsyncKeyState_(#VK_RBUTTON)*2+GetAsyncKeyState_(#VK_MBUTTON)*3)/$8000   
EndMacro
;
Macro CMMx(Canv) 
  GetGadgetAttribute(Canv, #PB_Canvas_MouseX)
EndMacro
;
Macro CMMy(Canv) 
  GetGadgetAttribute(Canv, #PB_Canvas_MouseY)
EndMacro
;
Procedure Distance(X1, Y1, X2, Y2)
  ProcedureReturn Sqr((X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2))
EndProcedure 
;
Procedure DrawArc(X, Y, Ang1.F,Ang2.F, Radius.F, Color=#Red)
  Ang1=Radian(Ang1):Ang2=Radian(Ang2)
  Protected Stp.F = #PI /(Radius*4)
  If Ang2<Ang1:Swap Ang1,Ang2:EndIf
  Repeat
    Box(X+Cos(Ang1) * Radius,Y+Sin(Ang1) * Radius, 2, 2, Color)
    Ang1 + Stp
  Until Ang1 > Ang2
  ;
EndProcedure
;
Procedure Lim(A,B,C)
  If A<B :ProcedureReturn B
  ElseIf A>C :ProcedureReturn C
  EndIf
  ProcedureReturn A
EndProcedure     
;
Procedure AngLine(X.D,Y.D,Ang.D,LineSize.D,RGB=0) ;- AngLine(x,y,Ang,LineSize) - Draw Line with Len LineSize From x y with Angle Ang
  LineXY(X,Y,X+Cos(Radian(Ang))*LineSize ,Y+Sin(Radian(Ang))*LineSize,RGB)
EndProcedure 
;
Macro ConiCGradient(X,Y,RGB1,RGB2,Ang)
  DrawingMode(#PB_2DDrawing_Gradient)     
  FrontColor(RGB1)
  BackColor(RGB2)
  ConicalGradient(X, Y,Ang)     
EndMacro
;
Procedure AngleEndPoint(X.D,Y.D,Ang.D,LineSize.D,*P.PointF) ; Ret circular end PointF for Line, Angle, Size
  *P\X= X+Cos(Radian(Ang))*LineSize
  *P\Y= Y+Sin(Radian(Ang))*LineSize
EndProcedure
;
Procedure DrawPie(X,Y, Ang1,Ang2,Radius,RGB)
  Protected Pf.Pointf
  Angline(X,Y,Ang1,Radius,RGB)
  Angline(X,Y,Ang2,Radius,RGB)
  Drawarc(X,Y,Ang1,Ang2,Radius,RGB)
  Angleendpoint(X,Y,(Ang1+Ang2)/2,Radius/2,@Pf)
  FillArea(Pf\X,Pf\Y,-1,RGB)
EndProcedure
;
Procedure.F GetAngle(X1.F,Y1.F,X2,Y2)  ; Ret Angle (Float)   
  Protected.F A = X2-X1 , B = Y2-Y1 , C = Sqr(A*A+B*B)
  Protected Ang.F = Degree(ACos(A/C))
  If Y1 > Y2  : ProcedureReturn 360-Ang  : EndIf
  ProcedureReturn Ang
EndProcedure
;
Procedure SpinKNOB(Index)
  With _KNOB(Index)
    Protected RGB,Radius=\Size/2-1
    Protected J,P.Pointf,R=Radius*0.8
    StartDrawing(CanvasOutput(\Canv))

    Box(0,0,OutputWidth(),OutputHeight(),\BkRGB)
 
    CONICGradient(\xCenter,\yCenter,$333333,$888888,90)
    Circle(\xCenter,\yCenter,Radius)
    DrawingMode(0)
    For J=0 To 7
      Angline(\xCenter,\yCenter,J*45,Radius*0.93,$Aaaaaa)
    Next
    RGB=Point(P\X,P\Y)
    Angleendpoint(\xCenter,\yCenter,\Ang-90,Radius*0.84,P.Pointf)
    Circle(\xCenter,\yCenter,Radius*0.85,$121212)
    If \Value>\MinValue
      If index<>0 And index <>1
        Drawpie(\xCenter,\yCenter,-90,\Ang-90,Radius*0.85,$00FF00)
      Else
        If \Value>=1200
          Drawpie(\xCenter,\yCenter,270,\Ang-90,Radius*0.85,$00FF00)
        Else
          Drawpie(\xCenter,\yCenter,-90,\Ang-90,Radius*0.85,$00FF00)
        EndIf  
      EndIf  
    EndIf     
    LineXY(\xCenter,\yCenter,\xCenter,\yCenter-Radius,0)
    LineXY(\xCenter,\yCenter,P\X,P\Y,0)
    ConiCGradient(\xCenter,\yCenter,\RGB1,\RGB2,-\Ang+90)
    Circle(\xCenter,\yCenter,Radius*0.75)
    ConiCGradient(\xCenter,\yCenter,\RGB2,\RGB1,-\Ang+90)
    Circle(\xCenter,\yCenter,Radius*0.70)
    DrawingMode(#PB_2DDrawing_Outlined)
    Circle(\xCenter,\yCenter,Radius*0.75,$676767)
    StopDrawing()
  EndWith
EndProcedure
;
Procedure.F RotaF(X.F ,Min.F ,Max.F )
  Protected A.F
  If X>=Min And X<=Max :ProcedureReturn X :EndIf
  If X <-Min  :A=-1 :EndIf
  If X >=Min
    ProcedureReturn Mod((X -Min -A), (1+Max -Min )) + A+Min
  EndIf
  ProcedureReturn Mod((1+X -Min -A), (1+Max -Min )) + A+Max
EndProcedure
;
Procedure Proportion(X.F, Min,Max,A.F,Z.F)
  If X = Min : ProcedureReturn A: EndIf   
  If X = Max : ProcedureReturn Z: EndIf
  Protected B.F=(Max-Min) / (X - Min)
  ProcedureReturn Lim(A + (Z-A) / B,A,Z)
EndProcedure
;
Procedure KNOBSettings(Instances)
  Protected Index,Offx,X=50,Y=100
  For Index=0 To Instances
    With _KNOB(Index)
      ; ----------------global dial settings here                                         
      \RGB1=$Bfbfbf
      \RGB2=$454545                                           
      \LightRGB=$FfAa                                         
      \BkRGB=_BkRGB
      ; -----------------------------------
      If Index = 0
        \Size = 80
        X = 40: Y = 50
      \MinValue=0     
      \MaxValue=2400  
      EndIf
      If INDEX = 1
        \Size = 80
        X = 40: Y = 170
      \MinValue=0     
      \MaxValue=2400  
      EndIf  
      If INDEX = 2
        \size = 80
        X = 40: Y = 290
      \MinValue=0     
      \MaxValue=100
      \Ang = 180
      \Value = Proportion(\Ang,0,360,\MinValue,\MaxValue)
      EndIf 
      If INDEX = 3
        \size = 80
        X = 240: Y = 50
      \MinValue=1     
      \MaxValue=100 
      \Ang = 40
      \Value = Proportion(\Ang,0,360,\MinValue,\MaxValue)
      ;AttackRaw = \Value * 20
      EndIf 
      If Index = 4
        \size = 80
        X = 340: Y = 50
      \MinValue=1     
      \MaxValue=100 
      \Ang = 70
      \Value = Proportion(\Ang,0,360,\MinValue,\MaxValue)
      ;DecayRaw = \Value * 50
      EndIf
      If INDEX = 5
        \size = 80
        X = 440: Y = 50
      \MinValue=0     
      \MaxValue=100
      \Ang = 180
      \Value = Proportion(\Ang,0,360,\MinValue,\MaxValue)
      ;SustainRaw = \Value
      EndIf  
      If INDEX = 6
       \size = 80 
        X = 540: Y = 50
      \MinValue=1     
      \MaxValue=100
      \Ang = 215
      \Value = Proportion(\Ang,0,360,\MinValue,\MaxValue)
      ;ReleaseRaw = \Value * 50
      EndIf 

      \Canv=CanvasGadget(#PB_Any, X+Offx/2,Y,\Size,\Size,#PB_Canvas_Keyboard)
      X+\Size+Offx+6
      \xCenter=\Size/2.0
      \yCenter=\Size/2.0
      \Info=TextGadget(#PB_Any,GadgetX(\Canv)+\xCenter,GadgetY(\Canv)-35,50,30,"")
      SetGadgetColor(\Info,#PB_Gadget_FrontColor,#White)
      SetGadgetColor(\Info,#PB_Gadget_BackColor,\BkRGB)
      SpinKNOB(Index)
      If index<>2
        SetGadgetText(\Info,Str(\Value))
      Else
           If LoadFont(1, "Arial", 10)
           SetGadgetFont(\Info, FontID(1))   ; Set the loaded Arial 10 font as the font for the label of the Mix dial
           EndIf
        SetGadgetText(\Info,"Mix")
      EndIf  
    EndWith 
  Next
EndProcedure

Procedure.i MIDI_GetInputDevices()
 
  Protected mic.MIDIINCAPS, Num.i, i.i
 
  Num = midiInGetNumDevs_()
  For i = 0 To Num - 1
    If midiInGetDevCaps_(i, @mic, SizeOf(MIDIINCAPS)) = 0
      AddGadgetItem(#MIDIDevice, -1, PeekS(@mic\szPname))
    EndIf
  Next i
 
  ProcedureReturn Num
 
EndProcedure


Procedure.s MIDI_Note(Note);returns note's name
   ProcedureReturn Trim(Mid(Chromatic$, (Note % 12) * 2 + 1, 2)) + Str(Note / 12)
EndProcedure


Procedure MidiInCallback(hMidiIn.i, wMsg.i, dwInstance.l, dwParam1.l, dwParam2.l)
 
  Protected Note.i, Velocity.i, Status.i
  Static KeyCounter.i
  
  Select wMsg    ; process MIDI in events
    Case #MIM_CLOSE
      KeyCounter = 0
      State = #Off
      kata = "Off"
    Case #MM_MIM_DATA
      Status = dwParam1 & $FF
      If Status = $90
        Note = (dwParam1 >> 8) & $FF
        Velocity = (dwParam1 >> 16) & $FF
        If Velocity
          Freq = Pow(2, (Note - 69) / 12) * 440
          If KeyCounter = 0
            State = #On
            kata = MIDI_Note(Note)
          EndIf
          KeyCounter + 1
        Else
          If KeyCounter : KeyCounter - 1 : EndIf
          If KeyCounter = 0
          State = #Off
          kata = "Off"
          TheInitializer = 0
          InitializeSampleCounter = 0
          OldFreq = Freq
          EndIf
        EndIf
      EndIf 
      
      If Status = $80
        If KeyCounter : KeyCounter - 1 : EndIf
        If KeyCounter = 0
          State = #Off 
          kata = "Off"
          TheInitializer = 0
          InitializeSampleCounter = 0
          OldFreq = Freq
        EndIf
      EndIf
        SetGadgetText(#Text_MIDIStatus, KATA)

  EndSelect
EndProcedure
;end midi stuff


Procedure Error(err)
  MessageRequester("PortAudio", PeekS(Pa_GetErrorText(err)))
  End
EndProcedure


 Procedure.f CalculateTheWaveform(SamplePoint.l)
   
  Protected N.l
  Protected WaveformVal.f
  
  Protected OffsetOsc1Val.f
  Protected OffsetOsc2Val.f
  
  ;now, the thing is that this: y = 2^(x/1200) is the multiplicative factor for 1 octave frequency offset of the played key when x = 1200 cents
  ;the offset dials go from 0 to 2400, centred at 1200. so x = dialval-1200
  OffsetOsc1Val = Pow(2, ((Osc1FreqVal-1200)/1200))
  OffsetOsc2Val = Pow(2, ((Osc2FreqVal-1200)/1200))
      
If Freq>0 
   
  If ForcedDecayCounter<=1000
  NumSamplePoints = #SampleRate/(OldFreq*OffsetOsc1Val)
  LFOSamplePoints = #SampleRate/(OldFreq*OffsetOsc2Val)
  Else  
  NumSamplePoints = #SampleRate/(Freq*OffsetOsc1Val)
  LFOSamplePoints = #SampleRate/(Freq*OffsetOsc2Val)
  EndIf

  Pee = Pee+1
  PeeLFO = PeeLFO + 1
  
    If Pee = NumSamplePoints
      Pee = 0
      If Pressed = 1 ;the sync pushbutton is pressed. we want the second oscillator to reset when the first does
        PeeLFO = 0
      EndIf  
    EndIf
    If Pressed = 0 ;the sync pushbutton is not pressed
      If PeeLFO = LFOSamplePoints
        PeeLFO = 0
      EndIf  
    EndIf
    
    WaveformVal = PhaseDistorter(SamplePoint, Pee/NumSamplePoints, PeeLFO/LFOSamplePoints)
          
EndIf       
ProcedureReturn WaveformVal
     
EndProcedure  


Procedure.f PhaseDistorter(SampleNum.l, xval.f, aval.f)
  
;vector phase distortion synthesis:

If xval>= 0 And xval<(dee/100)
  xval = (vee/100)*(xval/(dee/100))
ElseIf xval>= (dee/100) And xval<=1
  xval = (1-(vee/100))*((xval-(dee/100))/(1-(dee/100)))+(vee/100)
EndIf  


If aval>= 0 And aval<(dee1/100)
  aval = (vee1/100)*(aval/(dee1/100))
ElseIf aval>= (dee1/100) And aval<=1
  aval = (1-(vee1/100))*((aval-(dee1/100))/(1-(dee1/100)))+(vee1/100)
EndIf  

;righto, for Additive, we want a straight mix between the two oscillators, oscmixlevel goes from 0 to 100. so apply the oscmixlevel/100 to one oscillator
;and 1-(oscmixlevel/100) to the other oscillator. like so:

;*****Additive:
If SwitchPos = 1
  PDVal = ((OscMixLevel/100)*Sin(#PI2*XVAL)) + ((1-(OscMixLevel/100))*Sin(#PI2*aval))
EndIf  

;for FM, the Mix dial controls the magnitude of the FM. The /15 was settled on by trial and error:

;*****FM:
If SwitchPos = 2
  PDVal = Sin((#PI2*XVAL) + (OscMixLevel/15)*Sin(#PI2*aval))
EndIf  

;for AM, the Mix dial controls a couple of paramters of AM, depending on which half of the dial the setting is on:

;*****AM:
If SwitchPos = 3
  If OscMixLevel<50
    TheFactor = (OscMixLevel*2/100)
    PDVal = ((TheFactor+0.5*Sin(#PI2*XVAL))*Sin(#PI2*aval))*(1/(TheFactor+0.5))
  Else
    TheFactor = (OscMixLevel-50)*2/100
    PDVal = ((1+(TheFactor)*Sin(#PI2*XVAL))*Sin(#PI2*aval))*(1/(1+TheFactor))
  EndIf
EndIf

;*****Alternative AM algorithm:
If SwitchPos = 3
  If OscMixLevel<25
    TheFactor = (OscMixLevel*4/100)
    PDVal = ((TheFactor+0.5*Sin(#PI2*XVAL))*Sin(#PI2*aval))*(1/(TheFactor+0.5))
  ElseIf OscMixLevel>=25 And OscMixLevel<50
    TheFactor = (OscMixLevel-25)*4/100
    PDVal = ((1+(TheFactor)*Sin(#PI2*XVAL))*Sin(#PI2*aval))*(1/(1+TheFactor))
  Else
    TheFactor = (OscMixLevel-50)*2*5/100
    If (1+TheFactor*PDVal)<>0
      PDVal = ((1+TheFactor*PDVal)*Sin(#PI2*xval))*(1/(1+TheFactor*PDVal))
    EndIf  
    PDVal = ((1+TheFactor*PDVal)*Sin(#PI2*aval))
  EndIf
EndIf
;*****End Alternative AM algorithm

ProcedureReturn PDVal

EndProcedure


Global Stream


Procedure LevelADSR(SampleTracker.l)
  
If SampleTracker>2147483640
SampleCounter = 0
UptoV = 0
EndIf  
 
If State = #Off
    If (ReleaseSamples+UptoP-SampleTracker)>=0
      ADSRLevel = (UptoV*(ReleaseSamples+UptoP-SampleTracker))/ReleaseSamples
      Dee = UptoDeeVal + ((ReleaseDee - UptoDeeVal)*(SampleTracker-UptoP)/ReleaseSamples)
      Vee = UptoVeeVal + ((ReleaseVee - UptoVeeVal)*(SampleTracker-UptoP)/ReleaseSamples)
      Dee1 = UptoDeeVal1 + ((ReleaseDee1 - UptoDeeVal1)*(SampleTracker-UptoP)/ReleaseSamples)
      Vee1 = UptoVeeVal1 + ((ReleaseVee1 - UptoVeeVal1)*(SampleTracker-UptoP)/ReleaseSamples)
    Else
      ADSRLevel = 0
      Dee = ReleaseDee
      Vee = ReleaseVee
      Dee1 = ReleaseDee1
      Vee1 = ReleaseVee1
    EndIf 
    Groucho = ADSRLevel
EndIf
 
 If State = #On
   If TheInitializer = 0
     TheInitializer = 1
     ForcedDecayCounter = -1
            
      Attack = (0.0994856*Pow(1.00517037535,AttackRaw))/1000 ;could have made the Attack dial linear response, but prefer log as it gives finer control over shorter attacks
      Decay = DecayRaw / 1000
      Sustain = SustainRaw / 100
      Release = (0.9982977*Pow(1.00170523, ReleaseRaw))/1000 ;could have made the Release dial linear response, but prefer log as it gives finer control over shorter release
      
      AttackSamples = Attack*#SampleRate
      DecaySamples = Decay*#SampleRate
      SustainLevel = Sustain
      ReleaseSamples = Release*#SampleRate
            
    EndIf
    
    ForcedDecayCounter = ForcedDecayCounter+1
    If ForcedDecayCounter<=1000
      ADSRLevel = (1-Pow((ForcedDecayCounter/1000),1))*Groucho
    Else
      ;got to set the following only the FIRST TIME ForcedDecayCounter increments above 1000 samples:
      If InitializeSampleCounter = 0 
      Pee = -1
      PeeLFO = -1
      InitializeSampleCounter = 1  
      SampleCounter = 0
      SampleTracker = 0
      EndIf
      ;let us calculate the actual ADSR level now shall we
          If SampleTracker<=AttackSamples
            ADSRLevel = SampleTracker/AttackSamples
            Dee = AttackStartDee + ((AttackEndDee-AttackStartDee)*(SampleTracker)/AttackSamples)
            Vee = AttackStartVee + ((AttackEndVee-AttackStartVee)*(SampleTracker)/AttackSamples)
            Dee1 = AttackStartDee1 + ((AttackEndDee1-AttackStartDee1)*(SampleTracker)/AttackSamples)
            Vee1 = AttackStartVee1 + ((AttackEndVee1-AttackStartVee1)*(SampleTracker)/AttackSamples)
          ElseIf SampleTracker>AttackSamples And SampleTracker<=(AttackSamples+DecaySamples)
            ADSRLevel = SustainLevel + (((AttackSamples+DecaySamples-SampleTracker)*(1-SustainLevel))/DecaySamples)
            Dee = AttackEndDee + ((SustainDee-AttackEndDee)*(SampleTracker-AttackSamples)/DecaySamples)
            Vee = AttackEndVee + ((SustainVee-AttackEndVee)*(SampleTracker-AttackSamples)/DecaySamples)
            Dee1 = AttackEndDee1 + ((SustainDee1-AttackEndDee1)*(SampleTracker-AttackSamples)/DecaySamples)
            Vee1 = AttackEndVee1 + ((SustainVee1-AttackEndVee1)*(SampleTracker-AttackSamples)/DecaySamples)
          ElseIf SampleTracker>(AttackSamples+DecaySamples)
            ADSRLevel = SustainLevel
            Dee = SustainDee
            Vee = SustainVee
            Dee1 = SustainDee1
            Vee1 = SustainVee1
          EndIf
    EndIf      
    UptoV = ADSRLevel
    UptoP = SampleTracker
    UptoDeeVal = Dee
    UptoVeeVal = Vee
    UptoDeeVal1 = Dee1
    UptoVeeVal1 = Vee1
    
EndIf   

EndProcedure


ProcedureC PaStreamCallback(*in, *output.Float, frameCount, *timeInfo.PaStreamCallbackTimeInfo, statusFlags, *userData)

  While frameCount
   LevelADSR(SampleCounter)
   If SampleCounter<100
     *output\f = (CalculateTheWaveform(SampleCounter))*ADSRLevel*Pow((SampleCounter/100),5)
   Else
     *output\f = (CalculateTheWaveform(SampleCounter))*ADSRLevel
   EndIf  
      
   SampleCounter = SampleCounter+1  
   *output + 4
   frameCount - 1
Wend
 
EndProcedure


Define err.i

err = Pa_Initialize()
If err <> #paNoError : Error(err) : EndIf

Define op.PaStreamParameters
op\channelCount = 1
op\device = Pa_GetDefaultOutputDevice()
op\sampleFormat = #paFloat32
op\suggestedLatency = 75/1000

err = Pa_OpenStream(@Stream, #Null, @op, #SampleRate, #paframesperbufferunspecified, #paNoFlag, @PaStreamCallback(), 0)
If err <> #paNoError : Error(err) : EndIf

Define Indev.i
InDev = 0

Define hMi.i
hMi = 0

OpenWindow(#GADGET_MainWin, 0, 0, 665, 465, "Phase-Warper", #PB_Window_SystemMenu|#PB_Window_ScreenCentered)

    If CreateMenu(1000, WindowID(#GADGET_MainWin))    ;menu creation
      MenuTitle("?")
      MenuItem(1, "About")
    EndIf
    
  ComboBoxGadget(#MIDIDevice, 32, 405, 130, 20)
  If MIDI_GetInputDevices()
    SetGadgetState(#MIDIDevice, 0)
    InDev = 0
  Else
    InDev = -1
  EndIf    

CanvasGadget(#XYFirst, 237, 230, 200, 200)   
CanvasGadget(#XYSecond, 443, 230, 200, 200)

    If StartDrawing(CanvasOutput(#XYFirst))
        Box(0, 0, 200, 200, $000000)
        Circle(Osc1Point1X, Osc1Point1Y, 5, $00FF00)
        Circle(Osc1Point2X, Osc1Point2Y, 5, $FFFFFF)
        Circle(Osc1Point3X, Osc1Point3Y, 5, $FFFFFF)
        Circle(Osc1Point4X, Osc1Point4Y, 5, $0000FF)
        LineXY(Osc1Point1X, Osc1Point1Y, Osc1Point2X, Osc1Point2Y, $00FF00) 
        LineXY(Osc1Point2X, Osc1Point2Y, Osc1Point3X, Osc1Point3Y, $00CCFF) 
        LineXY(Osc1Point3X, Osc1Point3Y, Osc1Point4X, Osc1Point4Y, $0000FF) 
        StopDrawing()
    EndIf
      
    If StartDrawing(CanvasOutput(#XYSecond))
        Box(0, 0, 200, 200, $000000)
        Circle(Osc2Point1X, Osc2Point1Y, 5, $00FF00)
        Circle(Osc2Point2X, Osc2Point2Y, 5, $FFFFFF)
        Circle(Osc2Point3X, Osc2Point3Y, 5, $FFFFFF)
        Circle(Osc2Point4X, Osc2Point4Y, 5, $0000FF)
        LineXY(Osc2Point1X, Osc2Point1Y, Osc2Point2X, Osc2Point2Y, $00FF00) 
        LineXY(Osc2Point2X, Osc2Point2Y, Osc2Point3X, Osc2Point3Y, $00CCFF) 
        LineXY(Osc2Point3X, Osc2Point3Y, Osc2Point4X, Osc2Point4Y, $0000FF) 
        StopDrawing()
    EndIf

;frames around objects
Frame3DGadget(#PB_Any, 25, 5, 195, 371, "OSC")
Frame3DGadget(#PB_Any, 230, 5, 420, 155, "ADSR")
Frame3DGadget(#PB_Any, 230, 170, 420, 267, "WARP")
Frame3DGadget(#PB_Any, 25, 385, 195, 52, "MIDI")

;switch code - labelling
TextGadget(50, 170, 290, 30, 20, "-Add")
SetGadgetColor(50, #PB_Gadget_BackColor, _BkRGB)
SetGadgetColor(50, #PB_Gadget_FrontColor, $FFFFFF)
If LoadFont(0, "Arial", 10)
SetGadgetFont(50, FontID(0))   ; Set the loaded Arial 10 font as new standard
EndIf

TextGadget(51, 170, 320, 25, 20, "-FM")
SetGadgetColor(51, #PB_Gadget_BackColor, _BkRGB)
SetGadgetColor(51, #PB_Gadget_FrontColor, $FFFFFF)
SetGadgetFont(51, FontID(0))   ; Set the loaded Arial 10 font as new standard

TextGadget(52, 170, 350, 25, 20, "-AM")
SetGadgetColor(52, #PB_Gadget_BackColor, _BkRGB)
SetGadgetColor(52, #PB_Gadget_FrontColor, $FFFFFF)
SetGadgetFont(52, FontID(0))   ; Set the loaded Arial 10 font as new standard

;ADSR labels
TextGadget(62, 260, 135, 60, 20, "Attack")
SetGadgetColor(62, #PB_Gadget_BackColor, _BkRGB)
SetGadgetColor(62, #PB_Gadget_FrontColor, $FFFFFF)
SetGadgetFont(62, FontID(0))   ; Set the loaded Arial 10 font as new standard

TextGadget(63, 360, 135, 60, 20, "Decay")
SetGadgetColor(63, #PB_Gadget_BackColor, _BkRGB)
SetGadgetColor(63, #PB_Gadget_FrontColor, $FFFFFF)
SetGadgetFont(63, FontID(0))   ; Set the loaded Arial 10 font as new standard

TextGadget(64, 460, 135, 60, 20, "Sustain")
SetGadgetColor(64, #PB_Gadget_BackColor, _BkRGB)
SetGadgetColor(64, #PB_Gadget_FrontColor, $FFFFFF)
SetGadgetFont(64, FontID(0))   ; Set the loaded Arial 10 font as new standard

TextGadget(65, 560, 135, 60, 20, "Release")
SetGadgetColor(65, #PB_Gadget_BackColor, _BkRGB)
SetGadgetColor(65, #PB_Gadget_FrontColor, $FFFFFF)
SetGadgetFont(65, FontID(0))   ; Set the loaded Arial 10 font as new standard

TextGadget(66, 180, 142, 30, 20, "Sync")
SetGadgetColor(66, #PB_Gadget_BackColor, _BkRGB)
SetGadgetColor(66, #PB_Gadget_FrontColor, $FFFFFF)
SetGadgetFont(66, FontID(0))   ; Set the loaded Arial 10 font as new standard

TextGadget(67, 360, 197, 50, 20, "Group")
SetGadgetColor(67, #PB_Gadget_BackColor, _BkRGB)
SetGadgetColor(67, #PB_Gadget_FrontColor, $FFFFFF)
SetGadgetFont(67, FontID(0))   ; Set the loaded Arial 10 font as new standard

TextGadget(68, 565, 197, 50, 20, "Group")
SetGadgetColor(68, #PB_Gadget_BackColor, _BkRGB)
SetGadgetColor(68, #PB_Gadget_FrontColor, $FFFFFF)
SetGadgetFont(68, FontID(0))   ; Set the loaded Arial 10 font as new standard

TextGadget(181, 130, 80, 50, 20, "Osc 1")
SetGadgetColor(181, #PB_Gadget_BackColor, _BkRGB)
SetGadgetColor(181, #PB_Gadget_FrontColor, $FFFFFF)
SetGadgetFont(181, FontID(0))   ; Set the loaded Arial 10 font as new standard

TextGadget(182, 130, 200, 50, 20, "Osc 2")
SetGadgetColor(182, #PB_Gadget_BackColor, _BkRGB)
SetGadgetColor(182, #PB_Gadget_FrontColor, $FFFFFF)
SetGadgetFont(182, FontID(0))   ; Set the loaded Arial 10 font as new standard

TextGadget(#Text_MIDIStatus, 170, 407, 45, 20, "Off")
SetGadgetColor(#Text_MIDIStatus, #PB_Gadget_BackColor, _BkRGB)
SetGadgetColor(#Text_MIDIStatus, #PB_Gadget_FrontColor, $FFFFFF)
SetGadgetFont(#Text_MIDIStatus, FontID(0))   ; Set the loaded Arial 10 font as new standard

;switch code:

  CanvasGadget(0, 140, 285, 20, 85, #PB_Canvas_Border) ;1st switch
  
  ;draw switch 1 background and toggle
  StartDrawing(CanvasOutput(0))
  OldSwitchPos = 1
  SwitchPos = 1
  Box(0, 0, 20, 85, $EDEDED) ;(x, y, width, height, colour). background color of switch 1
  Box(0, 0, 20, 30, $000000) ;actual switch object rendering of switch 1
      For Height = 0 To 30 Step 2
        Line(0, Height, 20, 1, RGB($99, $99, $99))
      Next Height
  StopDrawing()
            
;knobs code
SetWindowColor(#GADGET_MainWin,_BkRGB)
KNOBSettings(Instances)  

;pushbutton1:
  CanvasGadget(200, 132, 130, 41, 41)
  
  If StartDrawing(CanvasOutput(200))
            Box(0, 0, 41, 41, _BkRGB)
            DrawingMode(#PB_2DDrawing_Gradient)      
            BackColor($FFFFFF)
            FrontColor($223388)
            CircularGradient(20, 20, 28)
            Circle(20, 20, 20)
            StopDrawing()
  EndIf
  
  ;pushbutton2:
  CanvasGadget(201, 310, 185, 41, 41)
  
  If StartDrawing(CanvasOutput(201))
            Box(0, 0, 41, 41, _BkRGB)
            DrawingMode(#PB_2DDrawing_Gradient)      
            BackColor($FFFFFF)
            FrontColor($223388)
            CircularGradient(20, 20, 28)
            Circle(20, 20, 20)
            StopDrawing()
  EndIf
          
  ;pushbutton3:
  CanvasGadget(202, 515, 185, 41, 41)
  
  If StartDrawing(CanvasOutput(202))
            Box(0, 0, 41, 41, _BkRGB)
            DrawingMode(#PB_2DDrawing_Gradient)      
            BackColor($FFFFFF)
            FrontColor($223388)
            CircularGradient(20, 20, 28)
            Circle(20, 20, 20)
            StopDrawing()
  EndIf
  
 ;---------MIDI STUFF for NOTE ON/OFF DISPLAY-------------
  If midiInOpen_(@hMi, InDev, @MidiInCallback(), 0, #CALLBACK_FUNCTION) = #MMSYSERR_NOERROR
    If midiInStart_(hMi) = #MMSYSERR_NOERROR
      SetGadgetText(#Text_MIDIStatus, "Off")
    EndIf
  EndIf
 ;----------END MIDI STUFF--------------------
 
  State = #Off
  Pa_StartStream(Stream)
   
Repeat
  Ev = WaitWindowEvent(1)
  
 If Ev = #PB_Event_Menu And EventMenu() = 1
   MessageRequester("About", "Phase-Warper v1.01 " + Chr(169) + " Jim Singh" + Chr(13) + "Vector Phase Synthesizer")
 EndIf
  
  If Ev = #PB_Event_Gadget And EventGadget() = #MIDIDevice
            midiInStop_(hMi)
            midiInClose_(hMi)
            InDev = GetGadgetState(#MIDIDevice)
            midiInOpen_(@hMi, InDev, @MidiInCallback(), 0, #CALLBACK_FUNCTION)
            If midiInStart_(hMi) = #MMSYSERR_NOERROR: EndIf
  EndIf
  ;switches code
  
  If Ev = #PB_Event_Gadget And EventGadget() = 0 
        If EventType() = #PB_EventType_LeftButtonDown Or (EventType() = #PB_EventType_MouseMove And GetGadgetAttribute(0, #PB_Canvas_Buttons) & #PB_Canvas_LeftButton) 
          If StartDrawing(CanvasOutput(0))
            x = GetGadgetAttribute(0, #PB_Canvas_MouseX)
            y = GetGadgetAttribute(0, #PB_Canvas_MouseY)
            If SwitchPos = 1
              If y>30 And y<45
                SwitchPos = 2
                Box(0, 0, 20, 85, $EDEDED)
                Box(0, 25, 20, 30, $000000)
                For Height = 25 To 55 Step 2
                Line(0, Height, 20, 1, RGB($99, $99, $99))
                Next Height
              ElseIf y>45
                SwitchPos = 3
                Box(0, 0, 20, 85, $EDEDED)
                Box(0, 50, 20, 30, $000000)
                For Height = 50 To 80 Step 2
                Line(0, Height, 20, 1, RGB($99, $99, $99))
                Next Height
              EndIf
            ElseIf SwitchPos = 2
              If y<25
                SwitchPos = 1
                Box(0, 0, 20, 85, $EDEDED)
                Box(0, 0, 20, 30, $000000)
                For Height = 0 To 30 Step 2
                Line(0, Height, 20, 1, RGB($99, $99, $99))
                Next Height
              ElseIf y>55
                SwitchPos = 3
                Box(0, 0, 20, 85, $EDEDED)
                Box(0, 50, 20, 30, $000000)
                For Height = 50 To 80 Step 2
                Line(0, Height, 20, 1, RGB($99, $99, $99))
                Next Height
              EndIf
            ElseIf SwitchPos = 3
              If y>35 And y<50
                SwitchPos = 2
                Box(0, 0, 20, 85, $EDEDED)
                Box(0, 25, 20, 30, $000000)
                For Height = 25 To 55 Step 2
                Line(0, Height, 20, 1, RGB($99, $99, $99))
                Next Height
              ElseIf y<35
                SwitchPos = 1
                Box(0, 0, 20, 85, $EDEDED)
                Box(0, 0, 20, 30, $000000)
                For Height = 0 To 30 Step 2
                Line(0, Height, 20, 1, RGB($99, $99, $99))
                Next Height
              EndIf
            EndIf
            StopDrawing()
              If SwitchPos<>OldSwitchPos
                Debug "switch0"+SwitchPos
                OldSwitchPos = SwitchPos
              EndIf  
            EndIf
          EndIf
          EndIf
              
        ;knobs code:
        
  If EventType() = #PB_EventType_LeftButtonUp Or MMk=0 Or Index>Instances
    Catch=0
  EndIf
  If Catch
    With _KNOB(Index)
      \Ang=RotaF(GetAngle(WindowMouseX(#GADGET_MainWin),WindowMouseY(#GADGET_MainWin),GadgetX(Catch)+\xCenter,GadgetY(Catch)+\yCenter)-90,0,360)
      SpinKNOB(Index)
      \Value= Proportion(\Ang,0,360,\MinValue,\MaxValue)
      If Index = 0
        If \Value>1200
          SetGadgetText(\Info,Str(\Value-2400))
          Osc1FreqVal = \Value - 1200
        Else
          SetGadgetText(\Info,Str(\Value))
          Osc1FreqVal = \Value + 1200
        EndIf 
      ElseIf index = 1
        If \Value>1200
          SetGadgetText(\Info,Str(\Value-2400))
          Osc2FreqVal = \Value - 1200
        Else
          SetGadgetText(\Info,Str(\Value))
           Osc2FreqVal = \Value + 1200
        EndIf 
      ElseIf index = 2  
        OscMixLevel = \Value
      ElseIf index = 3
        SetGadgetText(\Info,Str(\Value))
        AttackRaw = \Value*20
      ElseIf index = 4
        SetGadgetText(\Info,Str(\Value))
        DecayRaw = \Value*50
      ElseIf index = 5
        SetGadgetText(\Info,Str(\Value))
        SustainRaw = \Value
      ElseIf index = 6
        SetGadgetText(\Info,Str(\Value))
        ReleaseRaw = \Value*50  
      EndIf  
    EndWith 
  Else
    For Index=0 To Instances
      With _Knob(Index)
        If GetDlgCtrlID_(WindowFromPoint_(DesktopMouseX()|DesktopMouseY()<<32))=\Canv 
          If EventType()=#PB_EventType_LeftButtonDown And Distance(CMMx(\Canv),CMMy(\Canv),\xCenter,\yCenter)<\Size/2-1
            Catch=\Canv
            Break
          EndIf
        EndIf
      EndWith  
    Next 
  EndIf
  
  ;and now for the pushbutton1 code:
  
  If Ev = #PB_Event_Gadget And EventGadget() = 200 
        If EventType() = #PB_EventType_LeftButtonDown
          If StartDrawing(CanvasOutput(200))
            DrawingMode(#PB_2DDrawing_Gradient)      
            If Pressed = 0
            BackColor($223388)
            FrontColor($FFFFFF)
            CircularGradient(22,22,51)
            Circle(20,20,20)
            Pressed = 1
            Else
            BackColor($FFFFFF)
            FrontColor($223388)
            CircularGradient(20, 20, 28)
            Circle(20, 20, 20)
            Pressed = 0
            EndIf
            StopDrawing()
          EndIf
        EndIf
  EndIf    
  
  If Ev = #PB_Event_Gadget And EventGadget() = 201 
        If EventType() = #PB_EventType_LeftButtonDown
          If StartDrawing(CanvasOutput(201))
            DrawingMode(#PB_2DDrawing_Gradient)      
            If Push2Pressed = 0
            BackColor($223388)
            FrontColor($FFFFFF)
            CircularGradient(22,22,51)
            Circle(20,20,20)
            Push2Pressed = 1
            GroupTheDots = 1
            Else
            BackColor($FFFFFF)
            FrontColor($223388)
            CircularGradient(20, 20, 28)
            Circle(20, 20, 20)
            Push2Pressed = 0
            GroupTheDots = 0
            EndIf
            StopDrawing()
          EndIf
        EndIf
  EndIf    
  
  If Ev = #PB_Event_Gadget And EventGadget() = 202 
        If EventType() = #PB_EventType_LeftButtonDown
          If StartDrawing(CanvasOutput(202))
            DrawingMode(#PB_2DDrawing_Gradient)      
            If Push3Pressed = 0
            BackColor($223388)
            FrontColor($FFFFFF)
            CircularGradient(22,22,51)
            Circle(20,20,20)
            Push3Pressed = 1
            GroupTheDots1 = 1
            Else
            BackColor($FFFFFF)
            FrontColor($223388)
            CircularGradient(20, 20, 28)
            Circle(20, 20, 20)
            Push3Pressed = 0
            GroupTheDots1 = 0
            EndIf
            StopDrawing()
          EndIf
        EndIf
 EndIf
      

 If Ev = #PB_Event_Gadget And EventGadget() = #XYFirst
           If EventType() = #PB_EventType_MouseMove And GetGadgetAttribute(#XYFirst, #PB_Canvas_Buttons) & #PB_Canvas_LeftButton
            If StartDrawing(CanvasOutput(#XYFirst))
            x = GetGadgetAttribute(#XYFirst, #PB_Canvas_MouseX)
            y = GetGadgetAttribute(#XYFirst, #PB_Canvas_MouseY)
            
              If XYPointID = "Unpressed"
                If x>Osc1Point1X-5 And x<Osc1Point1X+5 And y>Osc1Point1Y-5 And y<Osc1Point1Y+5 
                  XYPointID = "FirstPoint"
                ElseIf x>Osc1Point2X-5 And x<Osc1Point2X+5 And y>Osc1Point2Y-5 And y<Osc1Point2Y+5
                  XYPointID = "SecondPoint"
                ElseIf x>Osc1Point3X-5 And x<Osc1Point3X+5 And y>Osc1Point3Y-5 And y<Osc1Point3Y+5
                  XYPointID = "ThirdPoint"
                ElseIf x>Osc1Point4X-5 And x<Osc1Point4X+5 And y>Osc1Point4Y-5 And y<Osc1Point4Y+5
                  XYPointID = "FourthPoint"
                EndIf
              EndIf
               
                  If GroupTheDots = 0
                    If x>=0 And x<=200 And y>=0 And y<=200
                      If XYPointID = "FirstPoint"
                      Osc1Point1X = x
                      Osc1Point1Y = y
                      AttackStartVee = Osc1Point1X*2.5
                      AttackStartDee = Osc1Point1Y/2
                      EndIf
                      If XYPointID = "SecondPoint"  
                      Osc1Point2X = x
                      Osc1Point2Y = y
                      AttackEndVee = Osc1Point2X*2.5
                      AttackEndDee = Osc1Point2Y/2 
                      EndIf
                      If XYPointID = "ThirdPoint"
                      Osc1Point3X = x
                      Osc1Point3Y = y
                      SustainVee = Osc1Point3X*2.5
                      SustainDee = Osc1Point3Y/2
                      EndIf
                      If XYPointID = "FourthPoint"
                      Osc1Point4X = x
                      Osc1Point4Y = y
                      ReleaseVee = Osc1Point4X*2.5
                      ReleaseDee = Osc1Point4Y/2
                      EndIf
                    
                      Box(0, 0, 200, 200, $000000)
                      Circle(Osc1Point1X, Osc1Point1Y, 5, $00FF00)
                      Circle(Osc1Point2X, Osc1Point2Y, 5, $FFFFFF)
                      Circle(Osc1Point3X, Osc1Point3Y, 5, $FFFFFF)
                      Circle(Osc1Point4X, Osc1Point4Y, 5, $0000FF)
                      LineXY(Osc1Point1X, Osc1Point1Y, Osc1Point2X, Osc1Point2Y, $00FF00) 
                      LineXY(Osc1Point2X, Osc1Point2Y, Osc1Point3X, Osc1Point3Y, $00CCFF) 
                      LineXY(Osc1Point3X, Osc1Point3Y, Osc1Point4X, Osc1Point4Y, $0000FF) 
                    EndIf
                  EndIf
                  
                  If GroupTheDots = 1
                    If XYPointID = "FirstPoint"
                      TheHopx = (x-Osc1Point1X)
                      TheHopy = (y-Osc1Point1Y)
                    ElseIf XYPointID = "SecondPoint"
                      TheHopx = (x-Osc1Point2X)
                      TheHopy = (y-Osc1Point2Y)
                    ElseIf XYPointID = "ThirdPoint"
                      TheHopx = (x-Osc1Point3X)
                      TheHopy = (y-Osc1Point3Y)
                    ElseIf XYPointID = "FourthPoint"
                      TheHopx = (x-Osc1Point4X)
                      TheHopy = (y-Osc1Point4Y)
                    EndIf  
                    
                                         
                    If (Osc1Point1X + TheHopx)>=0 And (Osc1Point1X + TheHopx)<=200 And (Osc1Point1Y + TheHopy)>=0 And (Osc1Point1Y + TheHopy)<=200 And (Osc1Point2X + TheHopx)>=0 And (Osc1Point2X + TheHopx)<=200 And (Osc1Point2Y + TheHopy)>=0 And (Osc1Point2Y + TheHopy)<=200 And (Osc1Point3X + TheHopx)>=0 And (Osc1Point3X + TheHopx)<=200 And (Osc1Point3Y + TheHopy)>=0 And (Osc1Point3Y + TheHopy)<=200 And (Osc1Point4X + TheHopx)>=0 And (Osc1Point4X + TheHopx)<=200 And (Osc1Point4Y + TheHopy)>=0 And (Osc1Point4Y + TheHopy)<=200

                      Osc1Point1X = Osc1Point1X + TheHopx
                      Osc1Point1Y = Osc1Point1Y + TheHopy
                      Osc1Point2X = Osc1Point2X + TheHopx
                      Osc1Point2Y = Osc1Point2Y + TheHopy
                      Osc1Point3X = Osc1Point3X + TheHopx
                      Osc1Point3Y = Osc1Point3Y + TheHopy
                      Osc1Point4X = Osc1Point4X + TheHopx
                      Osc1Point4Y = Osc1Point4Y + TheHopy
                      
                      AttackStartVee = Osc1Point1X*2.5
                      AttackStartDee = Osc1Point1Y/2
                      AttackEndVee = Osc1Point2X*2.5
                      AttackEndDee = Osc1Point2Y/2
                      SustainVee = Osc1Point3X*2.5
                      SustainDee = Osc1Point3Y/2
                      ReleaseVee = Osc1Point4X*2.5
                      ReleaseDee = Osc1Point4Y/2
                      
                      Box(0, 0, 200, 200, $000000)
                      Circle(Osc1Point1X, Osc1Point1Y, 5, $00FF00)
                      Circle(Osc1Point2X, Osc1Point2Y, 5, $FFFFFF)
                      Circle(Osc1Point3X, Osc1Point3Y, 5, $FFFFFF)
                      Circle(Osc1Point4X, Osc1Point4Y, 5, $0000FF)
                      LineXY(Osc1Point1X, Osc1Point1Y, Osc1Point2X, Osc1Point2Y, $00FF00) 
                      LineXY(Osc1Point2X, Osc1Point2Y, Osc1Point3X, Osc1Point3Y, $00CCFF) 
                      LineXY(Osc1Point3X, Osc1Point3Y, Osc1Point4X, Osc1Point4Y, $0000FF) 
                    EndIf
                    
                  EndIf  
            StopDrawing()
            EndIf
          EndIf
          
          If EventType() = #PB_EventType_LeftButtonUp
            XYPointID = "Unpressed"
            XYPointID1 = "Unpressed"
            TheHopx = 0
            TheHopy = 0
          EndIf  
      
 EndIf     
      
 
If Ev = #PB_Event_Gadget And EventGadget() = #XYSecond
           If EventType() = #PB_EventType_MouseMove And GetGadgetAttribute(#XYSecond, #PB_Canvas_Buttons) & #PB_Canvas_LeftButton
            If StartDrawing(CanvasOutput(#XYSecond))
            x1 = GetGadgetAttribute(#XYSecond, #PB_Canvas_MouseX)
            y1 = GetGadgetAttribute(#XYSecond, #PB_Canvas_MouseY)
            
              If XYPointID1 = "Unpressed"
                If x1>Osc2Point1X-5 And x1<Osc2Point1X+5 And y1>Osc2Point1Y-5 And y1<Osc2Point1Y+5 
                  XYPointID1 = "FirstPoint"
                ElseIf x1>Osc2Point2X-5 And x1<Osc2Point2X+5 And y1>Osc2Point2Y-5 And y1<Osc2Point2Y+5
                  XYPointID1 = "SecondPoint"
                ElseIf x1>Osc2Point3X-5 And x1<Osc2Point3X+5 And y1>Osc2Point3Y-5 And y1<Osc2Point3Y+5
                  XYPointID1 = "ThirdPoint"
                ElseIf x1>Osc2Point4X-5 And x1<Osc2Point4X+5 And y1>Osc2Point4Y-5 And y1<Osc2Point4Y+5
                  XYPointID1 = "FourthPoint"
                EndIf
              EndIf
               
                  If GroupTheDots1 = 0
                    If x1>=0 And x1<=200 And y1>=0 And y1<=200
                      If XYPointID1 = "FirstPoint"
                      Osc2Point1X = x1
                      Osc2Point1Y = y1
                      AttackStartVee1 = Osc2Point1X*2.5
                      AttackStartDee1 = Osc2Point1Y/2
                      EndIf
                      If XYPointID1 = "SecondPoint"  
                      Osc2Point2X = x1
                      Osc2Point2Y = y1
                      AttackEndVee1 = Osc2Point2X*2.5
                      AttackEndDee1 = Osc2Point2Y/2 
                      EndIf
                      If XYPointID1 = "ThirdPoint"
                      Osc2Point3X = x1
                      Osc2Point3Y = y1
                      SustainVee1 = Osc2Point3X*2.5
                      SustainDee1 = Osc2Point3Y/2
                      EndIf
                      If XYPointID1 = "FourthPoint"
                      Osc2Point4X = x1
                      Osc2Point4Y = y1
                      ReleaseVee1 = Osc2Point4X*2.5
                      ReleaseDee1 = Osc2Point4Y/2
                      EndIf
                    
                      Box(0, 0, 200, 200, $000000)
                      Circle(Osc2Point1X, Osc2Point1Y, 5, $00FF00)
                      Circle(Osc2Point2X, Osc2Point2Y, 5, $FFFFFF)
                      Circle(Osc2Point3X, Osc2Point3Y, 5, $FFFFFF)
                      Circle(Osc2Point4X, Osc2Point4Y, 5, $0000FF)
                      LineXY(Osc2Point1X, Osc2Point1Y, Osc2Point2X, Osc2Point2Y, $00FF00) 
                      LineXY(Osc2Point2X, Osc2Point2Y, Osc2Point3X, Osc2Point3Y, $00CCFF) 
                      LineXY(Osc2Point3X, Osc2Point3Y, Osc2Point4X, Osc2Point4Y, $0000FF) 
                    EndIf
                  EndIf
                  
                  If GroupTheDots1 = 1
                    If XYPointID1 = "FirstPoint"
                      TheHopx1 = (x1-Osc2Point1X)
                      TheHopy1 = (y1-Osc2Point1Y)
                    ElseIf XYPointID1 = "SecondPoint"
                      TheHopx1 = (x1-Osc2Point2X)
                      TheHopy1 = (y1-Osc2Point2Y)
                    ElseIf XYPointID1 = "ThirdPoint"
                      TheHopx1 = (x1-Osc2Point3X)
                      TheHopy1 = (y1-Osc2Point3Y)
                    ElseIf XYPointID1 = "FourthPoint"
                      TheHopx1 = (x1-Osc2Point4X)
                      TheHopy1 = (y1-Osc2Point4Y)
                    EndIf  
                    
                                         
                    If (Osc2Point1X + TheHopx1)>=0 And (Osc2Point1X + TheHopx1)<=200 And (Osc2Point1Y + TheHopy1)>=0 And (Osc2Point1Y + TheHopy1)<=200 And (Osc2Point2X + TheHopx1)>=0 And (Osc2Point2X + TheHopx1)<=200 And (Osc2Point2Y + TheHopy1)>=0 And (Osc2Point2Y + TheHopy1)<=200 And (Osc2Point3X + TheHopx1)>=0 And (Osc2Point3X + TheHopx1)<=200 And (Osc2Point3Y + TheHopy1)>=0 And (Osc2Point3Y + TheHopy1)<=200 And (Osc2Point4X + TheHopx1)>=0 And (Osc2Point4X + TheHopx1)<=200 And (Osc2Point4Y + TheHopy1)>=0 And (Osc2Point4Y + TheHopy1)<=200

                      Osc2Point1X = Osc2Point1X + TheHopx1
                      Osc2Point1Y = Osc2Point1Y + TheHopy1
                      Osc2Point2X = Osc2Point2X + TheHopx1
                      Osc2Point2Y = Osc2Point2Y + TheHopy1
                      Osc2Point3X = Osc2Point3X + TheHopx1
                      Osc2Point3Y = Osc2Point3Y + TheHopy1
                      Osc2Point4X = Osc2Point4X + TheHopx1
                      Osc2Point4Y = Osc2Point4Y + TheHopy1
                      
                      AttackStartVee1 = Osc2Point1X*2.5
                      AttackStartDee1 = Osc2Point1Y/2
                      AttackEndVee1 = Osc2Point2X*2.5
                      AttackEndDee1 = Osc2Point2Y/2
                      SustainVee1 = Osc2Point3X*2.5
                      SustainDee1 = Osc2Point3Y/2
                      ReleaseVee1 = Osc2Point4X*2.5
                      ReleaseDee1 = Osc2Point4Y/2
                      
                      Box(0, 0, 200, 200, $000000)
                      Circle(Osc2Point1X, Osc2Point1Y, 5, $00FF00)
                      Circle(Osc2Point2X, Osc2Point2Y, 5, $FFFFFF)
                      Circle(Osc2Point3X, Osc2Point3Y, 5, $FFFFFF)
                      Circle(Osc2Point4X, Osc2Point4Y, 5, $0000FF)
                      LineXY(Osc2Point1X, Osc2Point1Y, Osc2Point2X, Osc2Point2Y, $00FF00) 
                      LineXY(Osc2Point2X, Osc2Point2Y, Osc2Point3X, Osc2Point3Y, $00CCFF) 
                      LineXY(Osc2Point3X, Osc2Point3Y, Osc2Point4X, Osc2Point4Y, $0000FF)   
                    EndIf
                    
                  EndIf  
            StopDrawing()
            EndIf
          EndIf
                    
          
          If EventType() = #PB_EventType_LeftButtonUp
            XYPointID = "Unpressed"
            XYPointID1 = "Unpressed"
            TheHopx1 = 0
            TheHopy1 = 0
          EndIf  
                            
EndIf
  
Until Ev = #PB_Event_CloseWindow
;
End

; IDE Options = PureBasic 6.20 (Windows - x64)
; CursorPosition = 1390
; FirstLine = 1357
; Folding = ----
; EnableXP
; DPIAware