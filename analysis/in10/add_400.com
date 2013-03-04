> set work 20 1000
> @i_p:re_int INX 5038 1 1 IQW
> function w1 i_p:grp_axp w1
$ ngrp=v18
$ do i=2,ngrp
> @i_p:re_int INX 5038 'i' 'i' IQW
$ end do
> w11=w1
> w12=w2
> w13=w3
> w14=w4
> w15=w5
> w16=w6
> w17=w7
> w18=w8
$ do j=5039,5046
> @i_p:re_int INX 'j' 1 1 IQW
> function w1 i_p:grp_axp w1
$ ngrp=v18
$ do i=2,ngrp
> @i_p:re_int INX 'j' 'i' 'i' IQW
$ end do
> rebin w11 w1
> w11 = w11 + w1
> rebin w12 w2
> w12 = w12 + w2
> rebin w13 w3
> w13 = w13 + w3
> rebin w14 w4
> w14 = w14 + w4
> rebin w15 w5
> w15 = w15 + w5
> rebin w16 w6
> w16 = w16 + w6
> rebin w17 w7
> w17 = w17 + w7
> rebin w18 w8
> w18 = w18 + w8
$ end do
!Write output
$ inquire filename "Filename for output?"
> write/min/open w11 scratch$disk:'filename'
$ do i=12,18
> write/min w'i'
$ end do
> write/close
