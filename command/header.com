$!
$!	Command file to output first record of a RAW file to the terminal
$!
$!	Parameter: 	P1	File name - can be just the run number
$!			P2	File extension if P1 is only the run number
$ show log mari_data
$ show log mari$disks
$ on error then exit
$ if p1.eqs."" then inquire p1 "Filename"
$ if p1.eqs."" then exit
$ if p2.eqs."" then p2="RAW"
$ if f$type(p1).eqs."INTEGER"
$	then
$		inst_name=f$trnlnm("INST_NAME")
$		if inst_name.eqs."INST_NAME" 
$			then
$				write sys$output -
			"INST_NAME not defined - give full RAW file name"
$				exit
$		endif
$		inst_abrv_name=f$trnlnm("INST_ABRV_NAME")
$		if inst_abrv_name.eqs."INST_ABRV_NAME" 
$			then
$				write sys$output -
			"INST_ABRV_NAME not defined - give full RAW file name"
$				exit
$		endif
$		len_p1=f$length(p1)
$		if len_p1.eqs.1 then p1="0000"+p1
$		if len_p1.eqs.2 then p1="000"+p1
$		if len_p1.eqs.3 then p1="00"+p1
$		if len_p1.eqs.4 then p1="0"+p1
$		p1=inst_name+"_DATA:"+inst_abrv_name+p1+"."+p2
$ 	endif
$ open/error=STOP input_file 'p1'
$ read/error=CLOSE input_file record
$ if f$locate(".COR",P1).eqs.f$length(P1)
$	then
$ 		write sys$output "Run ID : "+f$extract(0,8,record)+"        User: "-
			+f$extract(8,20,record)+"  Inst: "+f$extract(292,20,record)
$ 		write sys$output "Protons: "+f$extract(72,8,record)+" uAhrs  Date: "-
			+f$extract(52,20,record)+" to "+f$extract(436,20,record)
$ 		write sys$output f$extract(132,80,record)
$	else
$ 		write sys$output "Run ID : "+f$extract(0,8,record)+"        User: "+f$extract(8,20,record)
$ 		write sys$output "Protons: "+f$extract(72,8,record)+" uAhrs  Date: "+f$extract(52,20,record)
$ 		write sys$output "Short Title: "+f$extract(28,24,record)
$	endif
$ close input_file
$ exit
$CLOSE:
$ write sys$output "Error in reading first record of "+p1
$ close input_file
$ exit
$STOP:
$ write sys$output "Error in opening "+p1
$ exit
