{$ifdef ALS_INTERFACE}
type

  // Error code
TALSError = ( als_NoError = 0,
              als_ALLibraryNotLoaded,
              als_ALCanNotOpenPlaybackDevice,
              als_ALCanNotOpenCaptureDevice,
              als_ALTryToCreateContextOnNonExistentDevice,
              als_ALContextNotCreated,
              als_ALCanNotGenerateSource,
              als_ALCanNotGenerateBuffer,
              als_ALCanNotGenerateFilter,
              als_ALCanNotFillBuffer,
              als_ALCanNotAttachBufferToSource,
              als_ErrorWhileBufferingData,
              als_ErrorWhileQueuingBuffer,

              als_ALCanNotManageBFormat,
              als_ALCanNotOpenLoopbackDevice,
              als_ALContextCanNotLoopback,
              als_ALAttributesNotConfiguredForLoopback,
              //als_CanNotCreateTargetMixFile,
              als_FileWriteErrorWhileMixing,
              //als_BadMixTimeInterval,

              als_LibSndFileNotLoaded,
              als_FileNotOpened,
              als_UnsupportedChannelCount,
              als_UnSupportedFormat,
              als_FloatSampleNotSupported,
              als_FailToReadSample,

              als_CanNotCloseCaptureFile,
              als_FileWriteErrorWhileCapturing,
              als_ALErrorWhileCapturing,

              als_OutOfMemory
              );

const
  // error messages
  StrALS_NoError = 'No error';
  StrALS_ALLibraryNotLoaded = 'OpenAL: Library not loaded';
  StrALS_ALCanNotOpenPlaybackDevice = 'OpenAL: Can not open playback device';
  StrALS_ALCanNotOpenCaptureDevice = 'OpenAL: Can not open capture device';
  StrALS_ALTryToOpenAContextOnNonExistentDevice = 'OpenAL: Try to open a context on non existent device';
  StrALS_ALContextNotCreated = 'OpenAL: Context not created';
  StrALS_ALCanNotGenerateSource = 'OpenAl: Can not generate source';
  StrALS_ALCanNotGenerateBuffer = 'OpenAL: Can not generate buffer';
  StrALS_ALCanNotGenerateFilter = 'OpenAL: Can not generate filter';
  StrALS_ALCanNotFillBuffer = 'OpenAL: Can not fill buffer';
  StrALS_ALCanNotAttachBufferToSource = 'OpenAL: Can not attach buffer to source';
  StrALS_ErrorWhileBufferingData = 'OpenAL: Error while buffering data';
  StrALS_ErrorWhileQueuingBuffer = 'OpenAL: Error while queuing buffer';

  StrALS_ALCanNotManageBFormat = 'OpenAL: Can not manage Ambisonic B-Format buffer';
  StrALS_ALCanNotOpenLoopbackDevice = 'OpenAL: Can not open loopback device';
  StrALS_ALContextCanNotLoopback = 'OpenAL: Context can not loopback';
  StrALS_ALAttributesNotConfiguredForLoopback  = 'OpenAL: Ask for Loopback context but attributes not configured on loopback mode';
  //StrALS_CanNotCreateTargetMixFile = 'LibSndFile: Can not create target mix file';
  StrALS_FileWriteErrorWhileMixing = 'LibSndFile: File write error while mixing audio to file';
  //StrALS_BadMixTimeInterval = 'Loopback Context: Bad time interval';

  StrALS_LibSndFileNotLoaded = 'LibSndFile: Library not loaded';
  StrALS_FileNotOpened = 'LibSndFile: Can not open file';
  StrALS_UnsupportedChannelCount = 'LibSndFile: Unsupported channel count';
  StrALS_UnSupportedFormat = 'OpenAL: Unsupported format';
  StrALS_FloatSampleNotSupported = 'OpenAL: Float samples not supported';
  StrALS_FailToReadSample = 'LibSndFile: Failed to read sample';

  StrALS_CanNotCloseCaptureFile = 'LibSndFile: Failed to close the capture file';
  StrALS_FileWriteErrorWhileCapturing = 'LibSndFile: a file write error occurs when saving captured audio';
  StrALS_ALErrorWhileCapturing = 'OpenAL: An error occurs when capturing audio';

  StrALS_OutOfMemory = 'ALSound: Not enough memory';

  StrALS_Undefined = 'OALSSoundManager: Undefined error';
{$endif}

{$ifdef ALS_IMPLEMENTATION}

function GetStrError(aError: TALSError): string;
begin
  case aError of
    als_NoError: Result := StrALS_NoError;
    als_ALLibraryNotLoaded: Result := StrALS_ALLibraryNotLoaded;
    als_ALCanNotOpenPlaybackDevice: Result := StrALS_ALCanNotOpenPlaybackDevice;
    als_ALCanNotOpenCaptureDevice: Result := StrALS_ALCanNotOpenCaptureDevice;
    als_ALTryToCreateContextOnNonExistentDevice: Result := StrALS_ALTryToOpenAContextOnNonExistentDevice;
    als_ALContextNotCreated: Result := StrALS_ALContextNotCreated;
    als_ALCanNotGenerateSource: Result := StrALS_ALCanNotGenerateSource;
    als_ALCanNotGenerateBuffer: Result := StrALS_ALCanNotGenerateBuffer;
    als_ALCanNotGenerateFilter: Result := StrALS_ALCanNotGenerateFilter;
    als_ALCanNotFillBuffer: Result := StrALS_ALCanNotFillBuffer;
    als_ALCanNotAttachBufferToSource: Result := StrALS_ALCanNotAttachBufferToSource;
    als_ErrorWhileBufferingData: Result := StrALS_ErrorWhileBufferingData;
    als_ErrorWhileQueuingBuffer: Result := StrALS_ErrorWhileQueuingBuffer;

    als_ALCanNotManageBFormat: Result := StrALS_ALCanNotManageBFormat;
    als_ALCanNotOpenLoopbackDevice: Result := StrALS_ALCanNotOpenLoopbackDevice;
    als_ALContextCanNotLoopback: Result := StrALS_ALContextCanNotLoopback;
    als_ALAttributesNotConfiguredForLoopback: Result := StrALS_ALAttributesNotConfiguredForLoopback;
    //als_CanNotCreateTargetMixFile: Result := StrALS_CanNotCreateTargetMixFile;
    als_FileWriteErrorWhileMixing: Result := StrALS_FileWriteErrorWhileMixing;
    //als_BadMixTimeInterval: Result := StrALS_BadMixTimeInterval;

    als_LibSndFileNotLoaded: Result := StrALS_LibSndFileNotLoaded;
    als_FileNotOpened: Result := StrALS_FileNotOpened;
    als_UnsupportedChannelCount: Result := StrALS_UnsupportedChannelCount;
    als_UnSupportedFormat: Result := StrALS_UnSupportedFormat;
    als_FloatSampleNotSupported: Result := StrALS_FloatSampleNotSupported;
    als_FailToReadSample: Result := StrALS_FailToReadSample;

    als_CanNotCloseCaptureFile: Result := StrALS_CanNotCloseCaptureFile;
    als_FileWriteErrorWhileCapturing: Result := StrALS_FileWriteErrorWhileCapturing;
    als_ALErrorWhileCapturing: Result := StrALS_ALErrorWhileCapturing;

    als_OutOfMemory: Result := StrALS_OutOfMemory;
    else
      Result := StrALS_Undefined;
  end;
end;

{$endif}

