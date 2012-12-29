$ user = f$user() - "[" - "]"
$ user = f$edit("''user'","COLLAPSE,LOWERCASE")
$ write sys$output "Setting up CVS access for ''user' to cvs.isis.rl.ac.uk"
$ define cvsroot ":pserver:''user'@cvs.isis.rl.ac.uk:/isis/cvs/repository"
$ define cvseditor "sys$system:decw$notepad.exe -geometry 600x400"
