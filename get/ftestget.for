***********************************************************************************
*
*	FTESTGET.F
*
*	C. Moreton-Smith February 1998	
*
*	Example program to show the use of the new generic "Get" routines.
*
*	This program reads some data from a standard ISIS raw data file and	
*	plays with it a little by rewriting into NeXus, ASCII and Open GENIE
*	intermediate files.	
*
***********************************************************************************

	PROGRAM ftestget
	IMPLICIT NONE

	INTEGER I, SPECS, NCHAN, DEBUG, INFORM, DIMS(1), NDIMS
	CHARACTER INFO*132
	BYTE INFOB(132)
	EQUIVALENCE(INFO,INFOB)
	REAL XVALS(3000)
	DOUBLE PRECISION YVALS(100)

	debug = 1	! Set to 1 for debugging
	inform = 1	! Set to 1 for informational messages
	CALL GF_activate_session('GENIE', inform, debug)

* Bit of general reading/writing of ISIS data, associate a few data items
* with handles ready to access them
	CALL GF_select_source('hrp00273.raw')

	CALL GF_get('h_HDR',  'HDR',  0)	! Read summary header info
	CALL GF_get('h_NSP1', 'NSP1', 0)
	CALL GF_get('h_NTC1', 'NTC1', 0)
	CALL GF_get('h_DATA', ' ', 2)		! Read second spectrum

* First get some the data directly into this program
	CALL GF_transfer('h_HDR',  '-->' , 'STRING', INFOB,  0, 80)
	CALL GF_transfer('h_NSP1', '-->', 'INT32', SPECS, 0, 0)
	CALL GF_transfer('h_NTC1', '-->', 'INT32', NCHAN, 0, 0)
	CALL GF_transfer('h_DATA.X', '-->', 'FLOAT32[]',
     +				 XVALS, 1, NCHAN)
	NDIMS = 1
	DIMS(1)=100
	CALL GF_transfer('h_DATA.Y[200:299]', '-->', 'FLOAT64[]',
     +				 YVALS, 1, DIMS)

*	Put it back as well and print it out (just as a test)
	NDIMS = 2
	DIMS(1)=50
	DIMS(2)=2
	CALL GF_transfer('h_DATA.PUTBACK', '<--', 'FLOAT64[]',
     +				 YVALS, NDIMS, DIMS)
	CALL GF_assign_handle('dummy', 'printin(h_DATA.PUTBACK)')


* And write it out to prove we got it !
	WRITE(*,*)'Spectrum count = ', SPECS
	WRITE(*,'('' Time Channels = '', 5F10.2)')(XVALS(I),I=1,5)
	WRITE(*,'('' Counts = '', 5F20.14)')(YVALS(I),I=1,5)
	WRITE(*,'('' File header = '', A100)') INFO
*	WRITE(*,*)'File header = ', INFO

* Write the data out into several different formats
* note that the 'new' creates a new file, the default is to append
* to an existing file
	CALL GF_select_destination('spud.in3', ' ')
	CALL GF_put('h_DATA', 'data', 0, 'new', 'Test data points')
	CALL GF_put('h_NSP1', 'nspec', 0, ' ', 'Number of points')

	CALL GF_select_destination('spud.asc', 'ASCII')
	CALL GF_put('h_DATA', 'data', 0, 'new', 'Test data points')
	CALL GF_put('h_NSP1', 'nspec', 0, ' ', 'Number of points')

	CALL GF_select_destination('spud.nxs', 'NeXus')
	CALL GF_put('h_DATA', 'data', 0, 'new', 'Test data points')
	CALL GF_put('h_NSP1', 'nspec', 0, ' ', 'Number of points')

* Now assemble some data into proper NeXus format.  This requires that we
* create entries of the correct NeXus types.
	CALL GF_assign_handle('ENTRY1', 'Create("NXentry")')
	CALL GF_assign_handle('ENTRY1.DATA', 'Create("NXdata")')
	CALL GF_assign_handle('ENTRY1.DATA.X', 'h_DATA.X')
	CALL GF_assign_handle('ENTRY1.DATA.Y', 'h_DATA.Y')
	CALL GF_assign_handle('ENTRY1.DATA.E', 'h_DATA.E')


* Now append this data to our output files in each format (the NeXus file
* is already the selected destination so does not need re-selecting)
	CALL GF_put('ENTRY1', 'entry_1', 0, ' ', 'Test data points')

	CALL GF_select_destination('spud.asc', 'ASCII')
	CALL GF_put('ENTRY1', 'entry_1', 0, ' ', 'Test data points')

	CALL GF_select_destination('spud.in3', 'GENIE')
	CALL GF_put('ENTRY1', 'entry_1', 0, ' ', 'Test data points')

* Clear up, delete all the handles and free up memory
	CALL GF_deactivate_session()
	
	END
