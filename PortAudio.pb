; /*
;  * $Id: portaudio.h 1083 2006-08-23 07:30:49Z rossb $
;  * PortAudio Portable Real-Time Audio Library
;  * PortAudio API Header File
;  * Latest version available at: http://www.portaudio.com/
;  *
;  * Copyright (c) 1999-2002 Ross Bencina And Phil Burk
;  *
;  * Permission is hereby granted, free of charge, To any person obtaining
;  * a copy of this software And associated documentation files
;  * (the "Software"), To deal in the Software without restriction,
;  * including without limitation the rights To use, copy, modify, merge,
;  * publish, distribute, sublicense, And/Or sell copies of the Software,
;  * And To permit persons To whom the Software is furnished To do so,
;  * subject To the following conditions:
;  *
;  * The above copyright notice And this permission notice shall be
;  * included in all copies Or substantial portions of the Software.
;  *
;  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;  * EXPRESS Or IMPLIED, INCLUDING BUT Not LIMITED To THE WARRANTIES OF
;  * MERCHANTABILITY, FITNESS For A PARTICULAR PURPOSE And NONINFRINGEMENT.
;  * IN NO EVENT SHALL THE AUTHORS Or COPYRIGHT HOLDERS BE LIABLE For
;  * ANY CLAIM, DAMAGES Or OTHER LIABILITY, WHETHER IN AN ACTION OF
;  * CONTRACT, TORT Or OTHERWISE, ARISING FROM, OUT OF Or IN CONNECTION
;  * With THE SOFTWARE Or THE USE Or OTHER DEALINGS IN THE SOFTWARE.
;  */
; 
; /*
;  * The text above constitutes the entire PortAudio license; however, 
;  * the PortAudio community also makes the following non-binding requests:
;  *
;  * Any person wishing To distribute modifications To the Software is
;  * requested To send the modifications To the original developer so that
;  * they can be incorporated into the canonical version. It is also 
;  * requested that these non-binding requests be included along With the 
;  * license above.
;  */


Enumeration ; PaErrorCode
  #paNoError = 0

  #paNotInitialized = -10000
  #paUnanticipatedHostError
  #paInvalidChannelCount
  #paInvalidSampleRate
  #paInvalidDevice
  #paInvalidFlag
  #paSampleFormatNotSupported
  #paBadIODeviceCombination
  #paInsufficientMemory
  #paBufferTooBig
  #paBufferTooSmall
  #paNullCallback
  #paBadStreamPtr
  #paTimedOut
  #paInternalError
  #paDeviceUnavailable
  #paIncompatibleHostApiSpecificStreamInfo
  #paStreamIsStopped
  #paStreamIsNotStopped
  #paInputOverflowed
  #paOutputUnderflowed
  #paHostApiNotFound
  #paInvalidHostApi
  #paCanNotReadFromACallbackStream      ;/**< @todo review error code name */
  #paCanNotWriteToACallbackStream       ;/**< @todo review error code name */
  #paCanNotReadFromAnOutputOnlyStream   ;/**< @todo review error code name */
  #paCanNotWriteToAnInputOnlyStream     ;/**< @todo review error code name */
  #paIncompatibleStreamHostApi
  #paBadBufferPtr
EndEnumeration

#paNoDevice = (-1)
#paUseHostApiSpecificDeviceSpecification = (-2)

Enumeration ; PaHostApiTypeId
  #paInDevelopment=0 ; /* use While developing support For a new host API */
  #paDirectSound=1
  #paMME=2
  #paASIO=3
  #paSoundManager=4
  #paCoreAudio=5
  #paOSS=7
  #paALSA=8
  #paAL=9
  #paBeOS=10
  #paWDMKS=11
  #paJACK=12
  #paWASAPI=13
EndEnumeration

#paFloat32        = ($00000001)
#paInt32          = ($00000002)
#paInt24          = ($00000004)
#paInt16          = ($00000008)
#paInt8           = ($00000010)
#paUInt8          = ($00000020)
#paCustomFormat   = ($00010000)
#paNonInterleaved = ($80000000)

#paFormatIsSupported = (0)
#paFramesPerBufferUnspecified = (0)
#paNoFlag          = (0)
#paClipOff         = ($00000001)
#paDitherOff       = ($00000002)
#paNeverDropInput  = ($00000004)
#paPrimeOutputBuffersUsingStreamCallback = ($00000008)
#paPlatformSpecificFlags = ($FFFF0000)

#paInputUnderflow   = ($00000001)
#paInputOverflow    = ($00000002)
#paOutputUnderflow  = ($00000004)
#paOutputOverflow   = ($00000008)
#paPrimingOutput    = ($00000010)

Enumeration ; PaStreamCallbackResult
  #paContinue=0
  #paComplete=1
  #paAbort=2
EndEnumeration

Structure PaHostApiInfo
  structVersion.l
  type.l
 *name.b
  deviceCount.l
  defaultInputDevice.l
  defaultOutputDevice.l
EndStructure

Structure PaHostErrorInfo
  hostApiType.l   
  errorCode.l            
 *errorText.b          
EndStructure

Structure PaDeviceInfo
  structVersion.l
 *name.b
  hostApi.l
  maxInputChannels.l
  maxOutputChannels.l
  defaultLowInputLatency.d
  defaultLowOutputLatency.d
  defaultHighInputLatency.d
  defaultHighOutputLatency.d
  defaultSampleRate.d
EndStructure

Structure PaStreamParameters
  device.l
  channelCount.l
  sampleFormat.l
  suggestedLatency.d
 *hostApiSpecificStreamInfo
EndStructure

Structure PaStreamCallbackTimeInfo
  inputBufferAdcTime.d
  currentTime.d
  outputBufferDacTime.d
EndStructure

Structure PaStreamInfo
  structVersion.l
  inputLatency.d
  outputLatency.d
  sampleRate.d
EndStructure

PrototypeC PaStreamCallback(*input, *output, frameCount, *timeInfo, statusFlags, *userdata)
PrototypeC PaStreamFinishedCallback(*user_data)

ImportC "portaudio_x86.lib"
  Pa_GetVersion()
  Pa_GetVersionText()
  Pa_GetErrorText(errorCode)
  Pa_Initialize()
  Pa_Terminate()
  Pa_GetHostApiCount()
  Pa_GetDefaultHostApi()
  Pa_GetHostApiInfo(hostApi)
  Pa_HostApiTypeIdToHostApiIndex(type)
  Pa_HostApiDeviceIndexToDeviceIndex(hostApi, hostApiDeviceIndex)
  Pa_GetLastHostErrorInfo()
  Pa_GetDeviceCount()
  Pa_GetDefaultInputDevice()
  Pa_GetDefaultOutputDevice()
  Pa_GetDeviceInfo(device)
  Pa_IsFormatSupported(*inputParameters, *outputParameters, sampleRate.d)
  Pa_OpenStream(*stream.LONG, *inputParameters, *outputParameters, sampleRate.d, framesPerBuffer, streamFlags, *streamCallback, *user_data)
  Pa_OpenDefaultStream(*stream.LONG, numInputChannels, numOutputChannels, sampleFormat, sampleRate.d, framesPerBuffer, *streamCallback, *user_data)
  Pa_CloseStream(*stream)
  Pa_SetStreamFinishedCallback(*stream, *streamFinishedCallback)
  Pa_StartStream(*stream)
  Pa_StopStream(*stream)
  Pa_AbortStream(*stream)
  Pa_IsStreamStopped(*stream)
  Pa_IsStreamActive(*stream)
  Pa_GetStreamInfo(*stream)
  Pa_GetStreamTime.d(*stream)
  Pa_GetStreamCpuLoad.d(*stream)
  Pa_ReadStream(*stream, *buffer, frames)
  Pa_WriteStream(*stream, *buffer, frames)
  Pa_GetStreamReadAvailable(*stream)
  Pa_GetStreamWriteAvailable(*stream)
  Pa_GetSampleSize(format)
  Pa_Sleep(msec)
  PaAsio_ShowControlPanel(deviceIndex, systemSpecific)
EndImport



; IDE Options = PureBasic 4.40 Beta 5 (Windows - x86)
; EnableXP