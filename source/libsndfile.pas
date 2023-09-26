{*
** Copyright (C) 1999-2016 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*}

{*
** sndfile.h -- system-wide definitions
**
** API documentation is in the doc/ directory of the source code tarball
** and at http://libsndfile.github.io/libsndfile/api.html.
*}

//
// FreePascal binding for LibSndFile written by lulu - 2022
//
{
  **************************************************************************
 *                                                                          *
 *  This file is part of ALSound. It is a pascal binding for LibSndFile     *
 *  library.
 *                                                                          *
 *  See the file LICENSE included in this distribution,                     *
 *  for details about the copyright.                                        *
 *                                                                          *
 *  This software is distributed in the hope of being useful                *
 *  for learning purposes about OpenAL-Soft and LibSndFile                  *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    *
 *                                                                          *
  **************************************************************************

  ALSound offer a simple and easy way to play, capture and mix sounds
  using OpenAL-Soft and LibSndFile libraries under FreePascal/Lazarus.


 written by Lulu - 2022 - https://github.com/Lulu04/ALSound
}
unit libsndfile;

{$mode objfpc}{$H+}
{$ModeSwitch AdvancedRecords}
{$PACKRECORDS C}

interface

uses
  DynLibs, ctypes
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF};

const
  {$IFDEF LINUX}
    {$IFDEF CPU386}
  LIBSNDFILE_LIBNAME = 'libsndfile.so';
    {$ENDIF}
    {$IFDEF CPUX86_64}
  LIBSNDFILE_LIBNAME = 'libsndfile.so';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF WINDOWS}
    {$IFDEF CPU386}
  LIBSNDFILE_LIBNAME = 'sndfile.dll';
    {$ENDIF}
    {$IFDEF CPUX86_64}
  LIBSNDFILE_LIBNAME = 'sndfile.dll';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF DARWIN}
    {$IFDEF CPUX86_64}
    LIBSNDFILE_LIBNAME = 'libsndfile.dylib';
    {$ELSE}
     {$error This Mac OS is not supported}
    {$ENDIF}
  {$ENDIF}



{* The following file types can be read and written.
** A file type would consist of a major type (ie SF_FORMAT_WAV) bitwise
** ORed with a minor type (ie SF_FORMAT_PCM). SF_FORMAT_TYPEMASK and
** SF_FORMAT_SUBMASK can be used to separate the major and minor file
** types.
*}

const
  // Major formats
  SF_FORMAT_WAV = $010000;  // Microsoft WAV format (little endian default)
  SF_FORMAT_AIFF = $020000;  // Apple/SGI AIFF format (big endian)
  SF_FORMAT_AU = $030000;  // Sun/NeXT AU format (big endian)
  SF_FORMAT_RAW = $040000;  // RAW PCM data
  SF_FORMAT_PAF = $050000;  // Ensoniq PARIS file format
  SF_FORMAT_SVX = $060000;  // Amiga IFF / SVX8 / SV16 format
  SF_FORMAT_NIST = $070000;  // Sphere NIST format
  SF_FORMAT_VOC = $080000;  // VOC files
  SF_FORMAT_IRCAM = $0A0000;  // Berkeley/IRCAM/CARL
  SF_FORMAT_W64 = $0B0000;  // Sonic Foundry's 64 bit RIFF/WAV
  SF_FORMAT_MAT4 = $0C0000;  // Matlab (tm) V4.2 / GNU Octave 2.0
  SF_FORMAT_MAT5 = $0D0000;  // Matlab (tm) V5.0 / GNU Octave 2.1
  SF_FORMAT_PVF = $0E0000;  // Portable Voice Format
  SF_FORMAT_XI = $0F0000;  // Fasttracker 2 Extended Instrument
  SF_FORMAT_HTK = $100000;  // HMM Tool Kit format
  SF_FORMAT_SDS = $110000;  // Midi Sample Dump Standard
  SF_FORMAT_AVR = $120000;  // Audio Visual Research
  SF_FORMAT_WAVEX = $130000;  // MS WAVE with WAVEFORMATEX
  SF_FORMAT_SD2 = $160000;  // Sound Designer 2
  SF_FORMAT_FLAC = $170000;  // FLAC lossless file format
  SF_FORMAT_CAF = $180000;  // Core Audio File format
  SF_FORMAT_WVE = $190000;  // Psion WVE format
  SF_FORMAT_OGG = $200000;  // Xiph OGG container
  SF_FORMAT_MPC2K = $210000;  // Akai MPC 2000 sampler
  SF_FORMAT_RF64 = $220000;  // RF64 WAV file
  SF_FORMAT_MPEG = $230000;  // MPEG-1/2 audio stream

  // Subtypes from here on.

  SF_FORMAT_PCM_S8 = $0001;  // Signed 8 bit data */
  SF_FORMAT_PCM_16 = $0002;  // Signed 16 bit data */
  SF_FORMAT_PCM_24 = $0003;  // Signed 24 bit data */
  SF_FORMAT_PCM_32 = $0004;  // Signed 32 bit data */

  SF_FORMAT_PCM_U8 = $0005;  // Unsigned 8 bit data (WAV and RAW only) */

  SF_FORMAT_FLOAT = $0006;  // 32 bit float data */
  SF_FORMAT_DOUBLE = $0007;  // 64 bit float data */

  SF_FORMAT_ULAW = $0010;  // U-Law encoded. */
  SF_FORMAT_ALAW = $0011;  // A-Law encoded. */
  SF_FORMAT_IMA_ADPCM = $0012;  // IMA ADPCM. */
  SF_FORMAT_MS_ADPCM = $0013;  // Microsoft ADPCM. */

  SF_FORMAT_GSM610 = $0020;  // GSM 6.10 encoding. */
  SF_FORMAT_VOX_ADPCM = $0021;  // OKI / Dialogix ADPCM */

  SF_FORMAT_NMS_ADPCM_16 = $0022;  // 16kbs NMS G721-variant encoding. */
  SF_FORMAT_NMS_ADPCM_24 = $0023;  // 24kbs NMS G721-variant encoding. */
  SF_FORMAT_NMS_ADPCM_32 = $0024;  // 32kbs NMS G721-variant encoding. */

  SF_FORMAT_G721_32 = $0030;  // 32kbs G721 ADPCM encoding. */
  SF_FORMAT_G723_24 = $0031;  // 24kbs G723 ADPCM encoding. */
  SF_FORMAT_G723_40 = $0032;  // 40kbs G723 ADPCM encoding. */

  SF_FORMAT_DWVW_12 = $0040;  // 12 bit Delta Width Variable Word encoding. */
  SF_FORMAT_DWVW_16 = $0041;  // 16 bit Delta Width Variable Word encoding. */
  SF_FORMAT_DWVW_24 = $0042;  // 24 bit Delta Width Variable Word encoding. */
  SF_FORMAT_DWVW_N = $0043;  // N bit Delta Width Variable Word encoding. */

  SF_FORMAT_DPCM_8 = $0050;  // 8 bit differential PCM (XI only) */
  SF_FORMAT_DPCM_16 = $0051;  // 16 bit differential PCM (XI only) */

  SF_FORMAT_VORBIS = $0060;  // Xiph Vorbis encoding. */
  SF_FORMAT_OPUS = $0064;  // Xiph/Skype Opus encoding. */

  SF_FORMAT_ALAC_16 = $0070;  // Apple Lossless Audio Codec (16 bit). */
  SF_FORMAT_ALAC_20 = $0071;  // Apple Lossless Audio Codec (20 bit). */
  SF_FORMAT_ALAC_24 = $0072;  // Apple Lossless Audio Codec (24 bit). */
  SF_FORMAT_ALAC_32 = $0073;  // Apple Lossless Audio Codec (32 bit). */

  SF_FORMAT_MPEG_LAYER_I = $0080;    // MPEG-1 Audio Layer I. */
  SF_FORMAT_MPEG_LAYER_II = $0081;   //MPEG-1 Audio Layer II. */
  SF_FORMAT_MPEG_LAYER_III = $0082;  //MPEG-2 Audio Layer III. */

  // Endian-ness options.

  SF_ENDIAN_FILE = $00000000;  // Default file endian-ness. */
  SF_ENDIAN_LITTLE = $10000000;  // Force little endian-ness. */
  SF_ENDIAN_BIG = $20000000;  // Force big endian-ness. */
  SF_ENDIAN_CPU = $30000000;  // Force CPU endian-ness. */

  SF_FORMAT_SUBMASK = $0000FFFF;
  SF_FORMAT_TYPEMASK = $0FFF0000;
  SF_FORMAT_ENDMASK = $30000000;


{*
** The following are the valid command numbers for the sf_command()
** interface.  The use of these commands is documented in the file
** command.html in the doc directory of the source code distribution.
*}

  SFC_GET_LIB_VERSION = $1000;
  SFC_GET_LOG_INFO = $1001;
  SFC_GET_CURRENT_SF_INFO = $1002;

  SFC_GET_NORM_DOUBLE = $1010;
  SFC_GET_NORM_FLOAT = $1011;
  SFC_SET_NORM_DOUBLE = $1012;
  SFC_SET_NORM_FLOAT = $1013;
  SFC_SET_SCALE_FLOAT_INT_READ = $1014;
  SFC_SET_SCALE_INT_FLOAT_WRITE = $1015;

  SFC_GET_SIMPLE_FORMAT_COUNT = $1020;
  SFC_GET_SIMPLE_FORMAT = $1021;

  SFC_GET_FORMAT_INFO = $1028;

  SFC_GET_FORMAT_MAJOR_COUNT = $1030;
  SFC_GET_FORMAT_MAJOR = $1031;
  SFC_GET_FORMAT_SUBTYPE_COUNT = $1032;
  SFC_GET_FORMAT_SUBTYPE = $1033;

  SFC_CALC_SIGNAL_MAX = $1040;
  SFC_CALC_NORM_SIGNAL_MAX = $1041;
  SFC_CALC_MAX_ALL_CHANNELS = $1042;
  SFC_CALC_NORM_MAX_ALL_CHANNELS = $1043;
  SFC_GET_SIGNAL_MAX = $1044;
  SFC_GET_MAX_ALL_CHANNELS = $1045;

  SFC_SET_ADD_PEAK_CHUNK = $1050;

  SFC_UPDATE_HEADER_NOW = $1060;
  SFC_SET_UPDATE_HEADER_AUTO = $1061;

  SFC_FILE_TRUNCATE = $1080;

  SFC_SET_RAW_START_OFFSET = $1090;

  SFC_SET_DITHER_ON_WRITE = $10A0;
  SFC_SET_DITHER_ON_READ = $10A1;

  SFC_GET_DITHER_INFO_COUNT = $10A2;
  SFC_GET_DITHER_INFO = $10A3;

  SFC_GET_EMBED_FILE_INFO = $10B0;

  SFC_SET_CLIPPING = $10C0;
  SFC_GET_CLIPPING = $10C1;

  SFC_GET_CUE_COUNT = $10CD;
  SFC_GET_CUE = $10CE;
  SFC_SET_CUE = $10CF;

  SFC_GET_INSTRUMENT = $10D0;
  SFC_SET_INSTRUMENT = $10D1;

  SFC_GET_LOOP_INFO = $10E0;

  SFC_GET_BROADCAST_INFO = $10F0;
  SFC_SET_BROADCAST_INFO = $10F1;

  SFC_GET_CHANNEL_MAP_INFO = $1100;
  SFC_SET_CHANNEL_MAP_INFO = $1101;

  SFC_RAW_DATA_NEEDS_ENDSWAP = $1110;

  // Support for Wavex Ambisonics Format
  SFC_WAVEX_SET_AMBISONIC = $1200;
  SFC_WAVEX_GET_AMBISONIC = $1201;

  {*
  ** RF64 files can be set so that on-close, writable files that have less
  ** than 4GB of data in them are converted to RIFF/WAV, as per EBU
  ** recommendations.
  *}
  SFC_RF64_AUTO_DOWNGRADE = $1210;

  SFC_SET_VBR_ENCODING_QUALITY = $1300;
  SFC_SET_COMPRESSION_LEVEL = $1301;

  SFC_SET_OGG_PAGE_LATENCY_MS = $1302;
  SFC_SET_OGG_PAGE_LATENCY = $1303;

  SFC_GET_BITRATE_MODE = $1304;
  SFC_SET_BITRATE_MODE = $1305;

  // Bitrate mode values (for use with SFC_GET/SET_BITRATE_MODE)
  SF_BITRATE_MODE_CONSTANT = 0;
  SF_BITRATE_MODE_AVERAGE = 1;
  SF_BITRATE_MODE_VARIABLE = 2;

const
  // Cart Chunk support
  SFC_SET_CART_INFO = $1400;
  SFC_GET_CART_INFO = $1401;

  // Opus files original samplerate metadata
  SFC_SET_ORIGINAL_SAMPLERATE = $1500;
  SFC_GET_ORIGINAL_SAMPLERATE = $1501;

  // Following commands for testing only.
  SFC_TEST_IEEE_FLOAT_REPLACE = $6001;

  {*
  ** These SFC_SET_ADD_* values are deprecated and will disappear at some
  ** time in the future. They are guaranteed to be here up to and
  ** including version 1.0.8 to avoid breakage of existing software.
  ** They currently do nothing and will continue to do nothing.
  *}
  SFC_SET_ADD_HEADER_PAD_CHUNK = $1051;

  SFC_SET_ADD_DITHER_ON_WRITE = $1070;
  SFC_SET_ADD_DITHER_ON_READ = $1071;


{*
** String types that can be set and read from files. Not all file types
** support this and even the file types which support one, may not support
** all string types.
*}

  SF_STR_TITLE = $01;
  SF_STR_COPYRIGHT = $02;
  SF_STR_SOFTWARE = $03;
  SF_STR_ARTIST = $04;
  SF_STR_COMMENT = $05;
  SF_STR_DATE = $06;
  SF_STR_ALBUM = $07;
  SF_STR_LICENSE = $08;
  SF_STR_TRACKNUMBER = $09;
  SF_STR_GENRE = $10;

{*
** Use the following as the start and end index when doing metadata
** transcoding.
*}

  SF_STR_FIRST = SF_STR_TITLE;
  SF_STR_LAST = SF_STR_GENRE;


  // True and false
  SF_FALSE = 0;
  SF_TRUE = 1;

  // Modes for opening files.
  SFM_READ = $10;
  SFM_WRITE = $20;
  SFM_RDWR = $30;

  SF_AMBISONIC_NONE = $40;
  SF_AMBISONIC_B_FORMAT = $41;


{* Public error values. These are guaranteed to remain unchanged for the duration
** of the library major version number.
** There are also a large number of private error numbers which are internal to
** the library which can change at any time.
*}

  SF_ERR_NO_ERROR = 0;
  SF_ERR_UNRECOGNISED_FORMAT = 1;
  SF_ERR_SYSTEM = 2;
  SF_ERR_MALFORMED_FILE = 3;
  SF_ERR_UNSUPPORTED_ENCODING = 4;


  // Channel map values (used with SFC_SET/GET_CHANNEL_MAP).
  SF_CHANNEL_MAP_INVALID = 0;
  SF_CHANNEL_MAP_MONO = 1;
  SF_CHANNEL_MAP_LEFT = 2; // Apple calls this 'Left'
  SF_CHANNEL_MAP_RIGHT = 3; // Apple calls this 'Right'
  SF_CHANNEL_MAP_CENTER = 4; // Apple calls this 'Center'
  SF_CHANNEL_MAP_FRONT_LEFT = 5;
  SF_CHANNEL_MAP_FRONT_RIGHT = 6;
  SF_CHANNEL_MAP_FRONT_CENTER = 7;
  SF_CHANNEL_MAP_REAR_CENTER = 8;
  // Apple calls this 'Center Surround', Msft calls this 'Back Center'
  SF_CHANNEL_MAP_REAR_LEFT = 9;
  // Apple calls this 'Left Surround', Msft calls this 'Back Left'
  SF_CHANNEL_MAP_REAR_RIGHT = 10;
  // Apple calls this 'Right Surround', Msft calls this 'Back Right'
  SF_CHANNEL_MAP_LFE = 11;
  // Apple calls this 'LFEScreen', Msft calls this 'Low Frequency'
  SF_CHANNEL_MAP_FRONT_LEFT_OF_CENTER = 12; // Apple calls this 'Left Center'
  SF_CHANNEL_MAP_FRONT_RIGHT_OF_CENTER = 13; // Apple calls this 'Right Center
  SF_CHANNEL_MAP_SIDE_LEFT = 14; // Apple calls this 'Left Surround Direct'
  SF_CHANNEL_MAP_SIDE_RIGHT = 15; // Apple calls this 'Right Surround Direct'
  SF_CHANNEL_MAP_TOP_CENTER = 16; // Apple calls this 'Top Center Surround'
  SF_CHANNEL_MAP_TOP_FRONT_LEFT = 17; // Apple calls this 'Vertical Height Left'
  SF_CHANNEL_MAP_TOP_FRONT_RIGHT = 18; // Apple calls this 'Vertical Height Right'
  SF_CHANNEL_MAP_TOP_FRONT_CENTER = 19; // Apple calls this 'Vertical Height Center'
  SF_CHANNEL_MAP_TOP_REAR_LEFT = 20; // Apple and MS call this 'Top Back Left'
  SF_CHANNEL_MAP_TOP_REAR_RIGHT = 21; // Apple and MS call this 'Top Back Right'
  SF_CHANNEL_MAP_TOP_REAR_CENTER = 22; // Apple and MS call this 'Top Back Center'

  SF_CHANNEL_MAP_AMBISONIC_B_W = 23;
  SF_CHANNEL_MAP_AMBISONIC_B_X = 24;
  SF_CHANNEL_MAP_AMBISONIC_B_Y = 25;
  SF_CHANNEL_MAP_AMBISONIC_B_Z = 26;

  SF_CHANNEL_MAP_MAX = 27;


type
  // A SNDFILE* pointer can be passed around much like stdio.h's FILE* pointer.
  PSNDFILE = ^_TSNDFILE;
  _TSNDFILE = record
  end;


{* The following typedef is system specific and is defined when libsndfile is
** compiled. sf_count_t will be a 64 bit value when the underlying OS allows
** 64 bit file offsets.
** On windows, we need to allow the same header file to be compiler by both GCC
** and the Microsoft compiler.
*}

  sf_count_t = cint64;

const
  SF_COUNT_MAX = cint64($7FFFFFFFFFFFFFFF);


{* A pointer to a SF_INFO structure is passed to sf_open () and filled in.
** On write, the SF_INFO structure is filled in by the user and passed into
** sf_open ().
*}
type
  PSF_INFO = ^TSF_INFO;

  TSF_INFO = record
    Frames: sf_count_t;
    SampleRate,   // Used to be called samples.  Changed to avoid confusion.
    Channels,
    Format,
    Sections,
    Seekable: cint;
  end;


{* The SF_FORMAT_INFO struct is used to retrieve information about the sound
** file formats libsndfile supports using the sf_command () interface.
**
** Using this interface will allow applications to support new file formats
** and encoding types when libsndfile is upgraded, without requiring
** re-compilation of the application.
**
** Please consult the libsndfile documentation (particularly the information
** on the sf_command () interface) for examples of its use.
*}
  PSF_FORMAT_INFO = ^TSF_FORMAT_INFO;

  TSF_FORMAT_INFO = record
    format: cint;
    Name,
    extension: PChar;
  end;

{*
** Enums and typedefs for adding dither on read and write.
** See the html documentation for sf_command(), SFC_SET_DITHER_ON_WRITE
** and SFC_SET_DITHER_ON_READ.
*}

const
  SFD_DEFAULT_LEVEL = 0;
  SFD_CUSTOM_LEVEL = $40000000;

  SFD_NO_DITHER = 500;
  SFD_WHITE = 501;
  SFD_TRIANGULAR_PDF = 502;

type
  PSF_DITHER_INFO = ^TSF_DITHER_INFO;

  TSF_DITHER_INFO = record
    _type: cint;
    level: double;
    Name: PChar;
  end;

{* Struct used to retrieve information about a file embedded within a
** larger file. See SFC_GET_EMBED_FILE_INFO.
*}
  PSF_EMBED_FILE_INFO = ^TSF_EMBED_FILE_INFO;

  TSF_EMBED_FILE_INFO = record
    Offset,
    Length: sf_count_t;
  end;

{*
**  Struct used to retrieve cue marker information from a file
*}
  PSF_CUE_POINT = ^TSF_CUE_POINT;

  TSF_CUE_POINT = record
    indx: cint32;
    position: cuint32;
    fcc_chunk,
    chunk_start,
    block_start: cint32;
    sample_offset: cuint32;
    Name: array[0..255] of cchar;
  end;

  PSF_CUES = ^TSF_CUES;

  TSF_CUES = record
    cue_count: cuint32;
    cue_points: array[0..99] of TSF_CUE_POINT;
  end;

{*
**  Structs used to retrieve music sample information from a file.
*}

const
  {*
  **  The loop mode field in SF_INSTRUMENT will be one of the following.
  *}
  SF_LOOP_NONE = 800;
  SF_LOOP_FORWARD = 801;
  SF_LOOP_BACKWARD = 802;
  SF_LOOP_ALTERNATING = 803;

type
  PSF_INSTRUMENT = ^TSF_INSTRUMENT;

  TSF_INSTRUMENT = record
    gain: cint;
    basenote, detune,
    velocity_lo, velocity_hi,
    key_lo, key_hi: cchar;
    loop_count: cint;
    loops: array[0..15] of record // make variable in a sensible way
      mode: cint;
      start,
      _end,
      Count: cuint32;
    end;
  end;



  // Struct used to retrieve loop information from a file
  PSF_LOOP_INFO = ^TSF_LOOP_INFO;

  TSF_LOOP_INFO = record
    time_sig_num,    // any positive integer > 0
    time_sig_den: cshort; // any positive power of 2 > 0
    loop_mode,      // see SF_LOOP enum
    num_beats: cint;      // this is NOT the amount of quarter notes !!!
    // a full bar of 4/4 is 4 beats */
    // a full bar of 7/8 is 7 beats */

    bpm: single;          // suggestion, as it can be calculated using other fields:
    // file's length, file's sampleRate and our time_sig_den
    // -> bpms are always the amount of _quarter notes_ per minute

    root_key: cint;       // MIDI note, or -1 for None
    future: array[0..5] of cint;
  end;


{*  Struct used to retrieve broadcast (EBU) information from a file.
**  Strongly (!) based on EBU "bext" chunk format used in Broadcast WAVE.
*}
  PSF_BROADCAST_INFO = ^SF_BROADCAST_INFO;

  SF_BROADCAST_INFO = record
    description: array[0..255] of cchar;
    originator: array[0..31] of cchar;
    originator_reference: array[0..31] of cchar;
    origination_date: array[0..9] of cchar;
    origination_time: array[0..7] of cchar;
    time_reference_low,
    time_reference_high: cuint32;
    version: cshort;
    umid: array[0..63] of cchar;
    loudness_value,
    loudness_range,
    max_true_peak_level,
    max_momentary_loudness,
    max_shortterm_loudness: cint16;
    reserved: array[0..179] of cchar;
    coding_history_size: cuint32;
    coding_history: array[0..255] of cchar;
  end;

  PSF_CART_TIMER = ^TSF_CART_TIMER;

  TSF_CART_TIMER = record
    usage: array[0..3] of cchar;
    Value: cint32;
  end;


  PSF_CART_INFO = ^SF_CART_INFO;

  SF_CART_INFO = record
    version: array[0..3] of cchar;
    title: array[0..63] of cchar;
    artist: array[0..63] of cchar;
    cut_id: array[0..63] of cchar;
    client_id: array[0..63] of cchar;
    category: array[0..63] of cchar;
    classification: array[0..63] of cchar;
    out_cue: array[0..63] of cchar;
    start_date: array[0..9] of cchar;
    start_time: array[0..7] of cchar;
    end_date: array[0..9] of cchar;
    end_time: array[0..7] of cchar;
    producer_app_id: array[0..63] of cchar;
    producer_app_version: array[0..63] of cchar;
    user_def: array[0..63] of cchar;
    level_reference: cint32;
    post_timers: array[0..7] of TSF_CART_TIMER;
    reserved: array[0..275] of cchar;
    url: array[0..1023] of cchar;
    tag_text_size: cuint32;
    tag_text: array[0..255] of cchar;
  end;


  //  Virtual I/O functionality
  sf_vio_get_filelen = function(user_data: Pointer): sf_count_t;
  sf_vio_seek = function(offset: sf_count_t; whence: cint;   user_data: Pointer): sf_count_t;
  sf_vio_read = function(ptr: Pointer; Count: sf_count_t;   user_data: Pointer): sf_count_t;
  sf_vio_write = function(const ptr: Pointer; Count: sf_count_t;   user_data: Pointer): sf_count_t;
  sf_vio_tell = function(user_data: Pointer): sf_count_t;

  PSF_VIRTUAL_IO = ^TSF_VIRTUAL_IO;

  TSF_VIRTUAL_IO = record
    get_filelen: sf_vio_get_filelen;
    seek: sf_vio_seek;
    Read: sf_vio_read;
    Write: sf_vio_write;
    tell: sf_vio_tell;
  end;

var
{* Open the specified file for read, write or both. On error, this will
** return a NULL pointer. To find the error number, pass a NULL SNDFILE
** to sf_strerror ().
** All calls to sf_open() should be matched with a call to sf_close().
*}
  sf_open: function(const path: PChar; mode: cint; sfinfo: PSF_INFO): PSNDFILE; cdecl;


{* Use the existing file descriptor to create a SNDFILE object. If close_desc
** is TRUE, the file descriptor will be closed when sf_close() is called. If
** it is FALSE, the descriptor will not be closed.
** When passed a descriptor like this, the library will assume that the start
** of file header is at the current file offset. This allows sound files within
** larger container files to be read and/or written.
** On error, this will return a NULL pointer. To find the error number, pass a
** NULL SNDFILE to sf_strerror ().
** All calls to sf_open_fd() should be matched with a call to sf_close().
*}
  sf_open_fd: function(fd, mode: cint; sfinfo: PSF_INFO; close_desc: cint): PSNDFILE; cdecl;
  sf_open_virtual: function(sfvirtual: PSF_VIRTUAL_IO; mode: cint; sfinfo: PSF_INFO; userd_data: Pointer): PSNDFILE; cdecl;


{* sf_error () returns a error number which can be translated to a text
** string using sf_error_number().
*}
  sf_error: function(sndfile: PSNDFILE): cint; cdecl;


{* sf_strerror () returns to the caller a pointer to the current error message for
** the given SNDFILE.
*}
  sf_strerror: function(sndfile: PSNDFILE): PChar; cdecl;


{* sf_error_number () allows the retrieval of the error string for each internal
** error number.
**
*}
  sf_error_number: function(errnum: cint): PChar; cdecl;


{* The following two error functions are deprecated but they will remain in the
** library for the foreseeable future. The function sf_strerror() should be used
** in their place.
*}
  sf_perror: function(sndfile: PSNDFILE): cint; cdecl;
  sf_error_str: function(sndfile: PSNDFILE; str: PChar; len: csize_t): cint; cdecl;


{* Allow the caller to retrieve information from or change aspects of the
** library behaviour.
*}
  sf_command: function(sndfile: PSNDFILE; command: cint; Data: Pointer; datasize: cint): cint; cdecl;


  // Return TRUE if fields of the SF_INFO struct are a valid combination of values.
  sf_format_check: function(info: PSF_INFO): cint; cdecl;


{* Seek within the waveform data chunk of the SNDFILE. sf_seek () uses
** the same values for whence (SEEK_SET, SEEK_CUR and SEEK_END) as
** stdio.h function fseek ().
** An offset of zero with whence set to SEEK_SET will position the
** read / write pointer to the first data sample.
** On success sf_seek returns the current position in (multi-channel)
** samples from the start of the file.
** Please see the libsndfile documentation for moving the read pointer
** separately from the write pointer on files open in mode SFM_RDWR.
** On error all of these functions return -1.
*}

const
  SF_SEEK_SET = 0;
  SF_SEEK_CUR = 1;
  SF_SEEK_END = 2;

var
  sf_seek: function(sndfile: PSNDFILE; frames: sf_count_t; whence: cint): sf_count_t; cdecl;


{* Functions for retrieving and setting string data within sound files.
** Not all file types support this features; AIFF and WAV do. For both
** functions, the str_type parameter must be one of the SF_STR_* values
** defined above.
** On error, sf_set_string() returns non-zero while sf_get_string()
** returns NULL.
*}

  sf_set_string: function(sndfile: PSNDFILE; str_type: cint; const str: PChar): cint; cdecl;
  sf_get_string: function(sndfile: PSNDFILE; str_type: cint): PChar; cdecl;


  // Return the library version string.

  sf_version_string: function(): PChar; cdecl;

{* Return the current byterate at this point in the file. The byte rate in this
** case is the number of bytes per second of audio data. For instance, for a
** stereo, 18 bit PCM encoded file with an 16kHz sample rate, the byte rate
** would be 2 (stereo) * 2 (two bytes per sample) * 16000 => 64000 bytes/sec.
** For some file formats the returned value will be accurate and exact, for some
** it will be a close approximation, for some it will be the average bitrate for
** the whole file and for some it will be a time varying value that was accurate
** when the file was most recently read or written.
** To get the bitrate, multiple this value by 8.
** Returns -1 for unknown.
*}
  sf_current_byterate: function(sndfile: PSNDFILE): cint; cdecl;

  // Functions for reading/writing the waveform data of a sound file.

  sf_read_raw: function(sndfile: PSNDFILE; ptr: Pointer; bytes: sf_count_t): sf_count_t; cdecl;
  sf_write_raw: function(sndfile: PSNDFILE; const ptr: Pointer; bytes: sf_count_t): sf_count_t; cdecl;


{* Functions for reading and writing the data chunk in terms of frames.
** The number of items actually read/written = frames * number of channels.
**     sf_xxxx_raw    read/writes the raw data bytes from/to the file
**     sf_xxxx_short  passes data in the native short format
**     sf_xxxx_int    passes data in the native int format
**     sf_xxxx_float  passes data in the native float format
**     sf_xxxx_double  passes data in the native double format
** All of these read/write function return number of frames read/written.
*}

  sf_readf_short: function(sndfile: PSNDFILE; ptr: pcshort; frames: sf_count_t): sf_count_t; cdecl;
  sf_writef_short: function(sndfile: PSNDFILE; const ptr: pcshort; frames: sf_count_t): sf_count_t; cdecl;

  sf_readf_int: function(sndfile: PSNDFILE; ptr: pcint; frames: sf_count_t): sf_count_t; cdecl;
  sf_writef_int: function(sndfile: PSNDFILE; const ptr: pcint; frames: sf_count_t): sf_count_t; cdecl;

  sf_readf_float: function(sndfile: PSNDFILE; ptr: pcfloat; frames: sf_count_t): sf_count_t; cdecl;
  sf_writef_float: function(sndfile: PSNDFILE; const ptr: pcfloat; frames: sf_count_t): sf_count_t; cdecl;

  sf_readf_double: function(sndfile: PSNDFILE; ptr: pcdouble; frames: sf_count_t): sf_count_t; cdecl;
  sf_writef_double: function(sndfile: PSNDFILE; const ptr: pcdouble; frames: sf_count_t): sf_count_t; cdecl;


{* Functions for reading and writing the data chunk in terms of items.
** Otherwise similar to above.
** All of these read/write function return number of items read/written.
*}

  sf_read_short: function(sndfile: PSNDFILE; ptr: pcshort; items: sf_count_t): sf_count_t; cdecl;
  sf_write_short: function(sndfile: PSNDFILE; const ptr: pcshort; items: sf_count_t): sf_count_t; cdecl;

  sf_read_int: function(sndfile: PSNDFILE; ptr: pcint; items: sf_count_t): sf_count_t; cdecl;
  sf_write_int: function(sndfile: PSNDFILE; const ptr: pcint; items: sf_count_t): sf_count_t; cdecl;

  sf_read_float: function(sndfile: PSNDFILE; ptr: pcfloat; items: sf_count_t): sf_count_t; cdecl;
  sf_write_float: function(sndfile: PSNDFILE; const ptr: pcfloat; items: sf_count_t): sf_count_t; cdecl;

  sf_read_double: function(sndfile: PSNDFILE; ptr: pcdouble; items: sf_count_t): sf_count_t; cdecl;
  sf_write_double: function(sndfile: PSNDFILE; const ptr: pcdouble; items: sf_count_t): sf_count_t; cdecl;


{* Close the SNDFILE and clean up all memory allocations associated with this
** file.
** Returns 0 on success, or an error number.
*}

  sf_close: function(sndfile: PSNDFILE): cint; cdecl;


{* If the file is opened SFM_WRITE or SFM_RDWR, call fsync() on the file
** to force the writing of data to disk. If the file is opened SFM_READ
** no action is taken.
*}

  sf_write_sync: procedure(sndfile: PSNDFILE); cdecl;



{* The function sf_wchar_open() is Windows Only!
** Open a file passing in a Windows Unicode filename. Otherwise, this is
** the same as sf_open().
**
** In order for this to work, you need to do the following:
**
**    #include <windows.h>
**    #define ENABLE_SNDFILE_WINDOWS_PROTOTYPES 1
**    #including <sndfile.h>
*}

{$IFDEF WINDOWS}
  sf_wchar_open: function(wpath: LPCWSTR; mode: cint; sfinfo: PSF_INFO): PSNDFILE; cdecl;
{$ENDIF}



type
{* Getting and setting of chunks from within a sound file.
**
** These functions allow the getting and setting of chunks within a sound file
** (for those formats which allow it).
**
** These functions fail safely. Specifically, they will not allow you to overwrite
** existing chunks or add extra versions of format specific reserved chunks but
** should allow you to retrieve any and all chunks (may not be implemented for
** all chunks or all file formats).
*}
  PSF_CHUNK_INFO = ^TSF_CHUNK_INFO;

  TSF_CHUNK_INFO = record
    id: array[0..63] of cchar;  // The chunk identifier
    id_size,                  // The size of the chunk identifier
    datalen: cunsigned;          // The size of that data
    Data: Pointer;    // Pointer to the data
  end;

var
{* Set the specified chunk info (must be done before any audio data is written
** to the file). This will fail for format specific reserved chunks.
** The chunk_info->data pointer must be valid until the file is closed.
** Returns SF_ERR_NO_ERROR on success or non-zero on failure.
*}
  sf_set_chunk: function(sndfile: PSNDFILE; const chunk_info: PSF_CHUNK_INFO): cint; cdecl;

type
{*
** An opaque structure to an iterator over the all chunks of a given id
*}
  PSF_CHUNK_ITERATOR = ^TSF_CHUNK_ITERATOR;
  TSF_CHUNK_ITERATOR = record
  end;

var
{* Get an iterator for all chunks matching chunk_info.
** The iterator will point to the first chunk matching chunk_info.
** Chunks are matching, if (chunk_info->id) matches the first
**     (chunk_info->id_size) bytes of a chunk found in the SNDFILE* handle.
** If chunk_info is NULL, an iterator to all chunks in the SNDFILE* handle
**     is returned.
** The values of chunk_info->datalen and chunk_info->data are ignored.
** If no matching chunks are found in the sndfile, NULL is returned.
** The returned iterator will stay valid until one of the following occurs:
**     a) The sndfile is closed.
**     b) A new chunk is added using sf_set_chunk().
**     c) Another chunk iterator function is called on the same SNDFILE* handle
**        that causes the iterator to be modified.
** The memory for the iterator belongs to the SNDFILE* handle and is freed when
** sf_close() is called.
*}
  sf_get_chunk_iterator: function(sndfile: PSNDFILE; const chunk_info: PSF_CHUNK_INFO): PSF_CHUNK_ITERATOR; cdecl;

{* Iterate through chunks by incrementing the iterator.
** Increments the iterator and returns a handle to the new one.
** After this call, iterator will no longer be valid, and you must use the
**      newly returned handle from now on.
** The returned handle can be used to access the next chunk matching
**      the criteria as defined in sf_get_chunk_iterator().
** If iterator points to the last chunk, this will free all resources
**      associated with iterator and return NULL.
** The returned iterator will stay valid until sf_get_chunk_iterator_next
**      is called again, the sndfile is closed or a new chunk us added.
*}
  sf_next_chunk_iterator: function(iterator: PSF_CHUNK_ITERATOR): PSF_CHUNK_ITERATOR; cdecl;


{* Get the size of the specified chunk.
** If the specified chunk exists, the size will be returned in the
**      datalen field of the SF_CHUNK_INFO struct.
**      Additionally, the id of the chunk will be copied to the id
**      field of the SF_CHUNK_INFO struct and it's id_size field will
**      be updated accordingly.
** If the chunk doesn't exist chunk_info->datalen will be zero, and the
**      id and id_size fields will be undefined.
** The function will return SF_ERR_NO_ERROR on success or non-zero on
** failure.
*}
  sf_get_chunk_size: function(const it: PSF_CHUNK_ITERATOR; chunk_info: PSF_CHUNK_INFO): cint; cdecl;

{* Get the specified chunk data.
** If the specified chunk exists, up to chunk_info->datalen bytes of
**      the chunk data will be copied into the chunk_info->data buffer
**      (allocated by the caller) and the chunk_info->datalen field
**      updated to reflect the size of the data. The id and id_size
**      field will be updated according to the retrieved chunk
** If the chunk doesn't exist chunk_info->datalen will be zero, and the
**      id and id_size fields will be undefined.
** The function will return SF_ERR_NO_ERROR on success or non-zero on
** failure.
*}
  sf_get_chunk_data: function(const it: PSF_CHUNK_ITERATOR; chunk_info: PSF_CHUNK_INFO): cint; cdecl;



///////////////////////////////////////////
///////////////////////////////////////////

type

  { TALSFileMetaData }

  TALSFileMetaData = record
    Title, Copyright, Software, Artist,
    Comment, Date, Album, License, TrackNumber, Genre: string;
    // Set all field to empty string
    procedure InitDefault;
    // Initialize all fields
    procedure Create(const aTitle, aCopyright, aSoftware, aArtist, aComment,
                   aDate, aAlbum, aLicense, aTrackNumber, aGenre: string);
    procedure Create(const aMetaData: TALSFileMetaData);
    // Write the metadata into a file opened in write or read/write mode.
    // Note: read/write mode don't work with MP3 file.
    // Return True if succed
    function WriteMetaDataTo(aSNDFile: PSNDFILE): boolean;
    // Read the metadata from a file opened in read or read/write mode
    procedure ReadMetaDataFrom(aSNDFile: PSNDFILE);
  end;


function LoadSndFileLibrary( const aFilename: string ): boolean;
procedure UnloadSndFileLibrary;

// open an audio file in a cross platform way
function ALSOpenAudioFile(const aFilename: string; aMode: cint; var aSFInfo: TSF_INFO): PSNDFILE;


implementation

uses SysUtils, LazUTF8;

var
  _LibSndFile_ReferenceCounter: cardinal = 0;
  _LibSndFile_Handle: TLibHandle = dynlibs.NilHandle;

function LoadSndFileLibrary( const aFilename: string ): boolean;
var
  f: UnicodeString;
  flag: boolean;

  function GetProc(const aName: string): Pointer;
  begin
    Result := DynLibs.GetProcedureAddress(_LibSndFile_Handle, PChar(aName));
    flag := flag and (Result <> nil);
  end;

begin
  flag := True;
  if _LibSndFile_Handle <> dynlibs.NilHandle then
  begin
    Inc(_LibSndFile_ReferenceCounter);
  end
  else
  begin
    if Length( aFilename ) = 0 then
      f := UnicodeString( LIBSNDFILE_LIBNAME )
    else
      f := UnicodeString( aFilename );

    _LibSndFile_Handle := DynLibs.LoadLibrary( f );
    if _LibSndFile_Handle <> DynLibs.NilHandle then
    begin
      Pointer(sf_open) := GetProc('sf_open');
      Pointer(sf_open_fd) := GetProc('sf_open_fd');
      Pointer(sf_open_virtual) := GetProc('sf_open_virtual');
      Pointer(sf_error) := GetProc('sf_error');
      Pointer(sf_strerror) := GetProc('sf_strerror');
      Pointer(sf_error_number) := GetProc('sf_error_number');
      Pointer(sf_perror) := GetProc('sf_perror');
      Pointer(sf_error_str) := GetProc('sf_error_str');
      Pointer(sf_command) := GetProc('sf_command');
      Pointer(sf_format_check) := GetProc('sf_format_check');
      Pointer(sf_seek) := GetProc('sf_seek');
      Pointer(sf_set_string) := GetProc('sf_set_string');
      Pointer(sf_get_string) := GetProc('sf_get_string');
      Pointer(sf_version_string) := GetProc('sf_version_string');
      Pointer(sf_current_byterate) := GetProc('sf_current_byterate');
      Pointer(sf_read_raw) := GetProc('sf_read_raw');
      Pointer(sf_write_raw) := GetProc('sf_write_raw');
      Pointer(sf_readf_short) := GetProc('sf_readf_short');
      Pointer(sf_writef_short) := GetProc('sf_writef_short');
      Pointer(sf_readf_int) := GetProc('sf_readf_int');
      Pointer(sf_writef_int) := GetProc('sf_writef_int');
      Pointer(sf_readf_float) := GetProc('sf_readf_float');
      Pointer(sf_writef_float) := GetProc('sf_writef_float');
      Pointer(sf_readf_double) := GetProc('sf_readf_double');
      Pointer(sf_writef_double) := GetProc('sf_writef_double');
      Pointer(sf_read_short) := GetProc('sf_read_short');
      Pointer(sf_write_short) := GetProc('sf_write_short');
      Pointer(sf_read_int) := GetProc('sf_read_int');
      Pointer(sf_write_int) := GetProc('sf_write_int');
      Pointer(sf_read_float) := GetProc('sf_read_float');
      Pointer(sf_write_float) := GetProc('sf_write_float');
      Pointer(sf_read_double) := GetProc('sf_read_double');
      Pointer(sf_write_double) := GetProc('sf_write_double');
      Pointer(sf_close) := GetProc('sf_close');
      Pointer(sf_write_sync) := GetProc('sf_write_sync');
      {$IFDEF WINDOWS}
      Pointer(sf_wchar_open) := GetProc('sf_wchar_open');
      {$ENDIF}
      Pointer(sf_set_chunk) := GetProc('sf_set_chunk');
      Pointer(sf_get_chunk_iterator) := GetProc('sf_get_chunk_iterator');
      Pointer(sf_next_chunk_iterator) := GetProc('sf_next_chunk_iterator');
      Pointer(sf_get_chunk_size) := GetProc('sf_get_chunk_size');
      Pointer(sf_get_chunk_data) := GetProc('sf_get_chunk_data');
      _LibSndFile_ReferenceCounter := 1;
    end
    else
    begin
      flag := False;
    end;
  end;
  Result := flag;
end;

procedure UnloadSndFileLibrary;
begin
  // Reference counting
  if _LibSndFile_ReferenceCounter > 0 then
    Dec(_LibSndFile_ReferenceCounter);
  if _LibSndFile_ReferenceCounter > 0 then
    exit;

  if _LibSndFile_Handle <> dynlibs.NilHandle then
  begin
    DynLibs.UnloadLibrary(_LibSndFile_Handle);
    _LibSndFile_Handle := DynLibs.NilHandle;
  end;
end;

function ALSOpenAudioFile(const aFilename: string; aMode: cint; var aSFInfo: TSF_INFO): PSNDFILE;
begin
  FillChar(aSFInfo, SizeOf(TSF_INFO), 0);
  Result := sf_open(PChar(aFilename), aMode, @aSFInfo);
end;

{ TALSFileMetaData }

procedure TALSFileMetaData.InitDefault;
begin
  Title := '';
  Copyright := '';
  Software := '';
  Artist := '';
  Comment := '';
  Date := '';
  Album := '';
  License := '';
  TrackNumber := '';
  Genre := '';
end;

procedure TALSFileMetaData.Create(const aTitle, aCopyright, aSoftware, aArtist,
  aComment, aDate, aAlbum, aLicense, aTrackNumber, aGenre: string);
begin
  Title := aTitle;
  Copyright := aCopyright;
  Software := aSoftware;
  Artist := aArtist;
  Comment := aComment;
  Date := aDate;
  Album := aAlbum;
  License := aLicense;
  TrackNumber := aTrackNumber;
  Genre := aGenre;
end;

procedure TALSFileMetaData.Create(const aMetaData: TALSFileMetaData);
begin
  Create(aMetaData.Title,
         aMetaData.Copyright,
         aMetaData.Software,
         aMetaData.Artist,
         aMetaData.Comment,
         aMetaData.Date,
         aMetaData.Album,
         aMetaData.License,
         aMetaData.TrackNumber,
         aMetaData.Genre);
end;

function TALSFileMetaData.WriteMetaDataTo(aSNDFile: PSNDFILE): boolean;
var res: boolean;
  procedure WriteStrMeta(aStrType: cint; const aValue: string);
  begin
    if aValue = '' then exit;
    {$ifdef windows}
      res := res and (sf_set_string(aSNDFile, aStrType, PChar(UTF8ToWinCP(aValue))) = 0);
    {$else}
      res := res and (sf_set_string(aSNDFile, aStrType, PChar(aValue)) = 0);
    {$endif}
  end;
begin
  Result := False;
  if aSNDFile = NIL then exit;

  res := True;
  WriteStrMeta(SF_STR_TITLE, PChar(Title));
  WriteStrMeta(SF_STR_COPYRIGHT, PChar(Copyright));
  WriteStrMeta(SF_STR_SOFTWARE, PChar(Software));
  WriteStrMeta(SF_STR_ARTIST, PChar(Artist));
  WriteStrMeta(SF_STR_COMMENT, PChar(Comment));
  WriteStrMeta(SF_STR_DATE, PChar(Date));
  WriteStrMeta(SF_STR_ALBUM, PChar(Album));
  WriteStrMeta(SF_STR_LICENSE, PChar(License));
  WriteStrMeta(SF_STR_TRACKNUMBER, PChar(TrackNumber));
  WriteStrMeta(SF_STR_GENRE, PChar(Genre));
  Result := res;
end;

procedure TALSFileMetaData.ReadMetaDataFrom(aSNDFile: PSNDFILE);
  function ReadStrMeta(aStrType: cint): string;
  begin
    {$ifdef windows}
      Result := {WinCPToUTF8}(sf_get_string(aSNDFile, aStrType));
    {$else}
      Result := sf_get_string(aSNDFile, aStrType);
    {$endif}
  end;
begin
  InitDefault;
  if aSNDFile = NIL then exit;

  Title := ReadStrMeta(SF_STR_TITLE);
  Copyright := ReadStrMeta(SF_STR_COPYRIGHT);
  Software := ReadStrMeta(SF_STR_SOFTWARE);
  Artist := ReadStrMeta(SF_STR_ARTIST);
  Comment := ReadStrMeta(SF_STR_COMMENT);
  Date := ReadStrMeta(SF_STR_DATE);
  Album := ReadStrMeta(SF_STR_ALBUM);
  License := ReadStrMeta(SF_STR_LICENSE);
  TrackNumber := ReadStrMeta(SF_STR_TRACKNUMBER);
  Genre := ReadStrMeta(SF_STR_GENRE);
end;



end.
