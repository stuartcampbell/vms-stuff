$ genie_dir :== lib$disk:[opengenie.]	! The "." is needed here
$ mydef :== define/user
$ 'mydef' GENIE_DIR                     'genie_dir'
$ 'mydef' GENIE_SHARED_LIBRARY          genie_dir:[genie]genie.so
$ 'mydef' GENIE_SMALLTALK_IMAGE	    genie_dir:[genie]genie.im
$ 'mydef' GENIE                         genie_shared_library
$ run ftestget
