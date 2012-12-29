! #3R3 Computer Support - 0
! #4R3 Computer Support - 0
! #3R3 Coffee Room      - 1
! #4R3 Coffee Room      - 1
! #3DAC                 - 2
! #4DAC                 - 2
! #3CRISP Cabin         - 4
! #4CRISP Cabin         - 4
! #3HET Cabin           - 11
! #4HET Cabin           - 11
$ inquire printer "Which printer do you wish to send your plot to?"
> k/h
> j "plaser'printer' dec_postscript.dat"
$ end:
