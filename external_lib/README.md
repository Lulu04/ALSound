A native Free Pascal class for playback of Impulse Tracker (IT) and Scream Tracker (S3M) tracker modules.

This is basically a fairly direct port of 8bitbubsy's it2play, which itself is an accurate port of 
Impulse Tracker 2.15's original IT playroutine ported from assembly to C.

8bitbubsy's C port: https://github.com/8bitbubsy/it2play

Thanks to 8bitbubsy and muzzy for helping to debug this port!

Current state:
- Supports modules using samples or instruments, envelopes, filters, etc.
- SB16 and SB16-MMX drivers implemented; choice of interpolation, volume ramping and filter support
- Lots of low-level bit twiddling making the code fast but ugly
- Some bugs likely due to porting mishaps. No AI help used.

Partial TODO:
- Example projects
- 8bitbubsy's HQ driver
- WAV writer
- Support for MMCMP compressed modules
- Refactor for more Object Pascalish code
- Test against all the OpenMPT IT tests

Bugs:
- Filter routines may need tweaking
