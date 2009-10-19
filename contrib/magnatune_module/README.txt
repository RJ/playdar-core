Erlang module to search free content from Magnatune.com
-------------------------------------------------------
To activate this module, link or move it into playdar_modules.

 $ cd playdar_modules
 $ ln -s ../contrib/playdar_module .
 $ cd ..
 $ make

On first run, this module fetches a ~4MB file from Magnatune.com listing
all the free 128kbps mp3 files they have available, and indexes them using
the library resolver module.

