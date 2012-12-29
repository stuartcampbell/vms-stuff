$ WSO :== WRITE SYS$OUTPUT
$ WSO "Nodename : ",F$GETSYI("nodename")
$ WSO "Model    : ",F$GETSYI("hw_name")
$!
