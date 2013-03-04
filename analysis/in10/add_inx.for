	PROGRAM add_inx

	INTEGER debug, info

	debug = 0
	info = 0

	CALL GF_ACTIVATE_SESSION('GENIE', debug, info)

	CALL GF_ASSIGN_HANDLE('status','load("add_inx.gcl")')
	CALL GF_ASSIGN_HANDLE('tmp','add_inx()')

	CALL GF_DEACTIVATE_SESSION()

	END
