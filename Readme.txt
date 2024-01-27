
  HDAutils consists of 

  1. HDAstat

  Displays status of HD Audio controller and Audio Function Group (AFG) node.
  
  2. HDAplay

   Loads a PCM-coded file ( RIFF format, filenames usually having a .wav 
  extension ) into memory, searches a path to the lineout pin, then starts
  the HDA DMA engine to play and launches a shell. Exiting the shell will
  terminate the play and release all resources.

   Hints

   - For notebooks it might be necessary to add option -s to use the speaker
     instead of lineout.
   - HDAplay is no TSR. It's absolutely inactive while the shell command
     processor is running. The only thing that's running in the background is
     the HDA controller's DMA engine.
   - HDAplay won't do any format conversions. If the codec attached to the HDA
     controller doesn't support the format of the audio data supplied, then
     the output will be "strange".

  3. History

  - 27.1.2024, hdaplay: display bit depths and sample rates supported by codec.

  For license see file license.txt
  
