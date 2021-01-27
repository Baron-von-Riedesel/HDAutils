
  HDAutils consists of 

  1. HDAstat: displays status of HD Audio controller and Audio Function 
     Group (AFG) node.
  
  2. HDAplay: loads a PCM-coded file ( RIFF format, filenames usually having
     a .wav extension ) into memory, searches a path to the lineout pin, then
     starts the HDA DMA engine to play and launches a shell. Exiting the shell
     will terminate the play and release all resources.

  For license see file license.txt
  
