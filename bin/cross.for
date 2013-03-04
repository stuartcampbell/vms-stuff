	program cross
c
c *********************************************
c * CROSS Calculate sample cross-sections etc *
c * WSH 			      5-10-90 *
c *********************************************
c
c 12-2-90  : Statement opening file XSECT.DAT amended to be readonly so as
c            to avoid privilege conflicts. (WSH)
c 7-3-90   : Added density calculations (optional).
c 17-9-90  : Added absorption cross-section at 1.8 Angstroms.
c 1-10-90  : Change for FZ partials
c
c N         - number of elements in sample
c Zl(20)    - atomic numbers
c fnl(20)   - the number of atoms of each element in the scattering unit
c b_bar(20) - the bound coherent scattering length of each element
c
	logical there, lg0, lg1, lg2, lg3
	real fnl(20), b_bar(20), sig_scatt(20), sig_abs(20), Al(20)
	real ain(100), bin(100), ssin(100), sain(100)
	real nv
	integer zin(100), Zl(20)
	character namin(100)*2, nam(20)*2, string*20, file*100
	character n1*2, n2*2
c
	pi = asin(1.0)*2.0
	small = 1.0e-6
c
	write(*,*) ' *********************************************'
	write(*,*) ' * CROSS Calculate sample cross-sections etc *'
	write(*,*) ' *********************************************'
c
	write(*,*) ' Default cross-section data file is sears91.dat '
	write(*,6)
 6	format('  Type CR to accept, or give alternative file spec. > ',$)
	read(*,7) iq, file
 7	format(q,a100)
	if (iq.le.0) file='user$disk:[sic.bin]sears91.dat'
	inquire(file=file,exist=there)
	if (.NOT.there) then
	    write(*,*) ' Failure locating ', file
	    stop
	end if
c
c --- data input section ----------------------------------------------
 4	write(*,1)
 1	format('  How many elements in the sample? ',$)
	read(*,*) N
	if (N.gt.20) then
	    write(*,*) ' Program can only cope with 20 elements! '
	    goto 4
	end if
	fN = real(N)
c
	write(*,*)' For each element in turn give chemical symbol and x where'
	write(*,*)' x = number of atoms of element in one scattering unit'
	write(*,*)' (x need not be an integer : eg a 1-atom scattering unit)'
	write(*,*)
	Nsu = 0
	do i=1,N
	    write(*,3) i
 3	    format('  Element',i2,' > ',$)
	    read(*,2) string
 2	    format(a20)
	    i1 = 0
 5	    i1 = i1 + 1
	    if (string(i1:i1).eq.' ') goto 5
	    kk = 1
	    if (string(i1+1:i1+1).eq.' ') kk=0
	    nam(i) = string(i1:i1+kk)
	    read(string(i1+kk+1:len(string)),*) fnl(i)
	    fNsu = fNsu + fnl(i)
	end do
	write(*,*) ' Give density in gcm**-3, 0 if not known or '
	write(*,8)
 8	format('  a negative value if in atoms per cubic Angstrom > ',$)
	read(*,*) rho
	write(*,*)
	write(*,*) ' Units: b in fm, sigma in barns'
	write(*,*)
	write(*,*) ' PRIMARY DATA...'
	write(*,*) '  Z    A         Symbol b_bar      sig_scatt    sig_abs'
c
c --- data read section -----------------------------------------------
	open(unit=99,file=file,status='old',readonly)
c
	read(99,*,err=98) nskip
	do i=1,nskip
	    read(99,*,err=98)
	end do
c
	do i=1,100
	    read(99,11) zin(i), ain(i), namin(i), bin(i), ssin(i), sain(i)
 11	    format(1x,i3,2x,f10.6,2x,a2,2x,f10.5,2x,f9.5,2x,f11.5)
	end do
	close(99)
c
	m2 = 1	! flag for one of elements not found - start assuming OK
	do i=1,N
	m1 = 0	! flag for this element found
	do j=1,100
	    ic1 = ichar(nam(i)(1:1))
	    icin1 = ichar(namin(j)(1:1))
	    ic2 = ichar(nam(i)(2:2))
	    icin2 = ichar(namin(j)(2:2))
	    lg0 = (nam(i).eq.namin(j))
	    lg1 = (abs(ic1-icin1).eq.32.AND.ic2.eq.icin2)	! these checks 
	    lg2 = (abs(ic1-icin1).eq.32.AND.abs(ic2-icin2).eq.32) ! allow for
	    lg3 = (ic1.eq.icin1.AND.abs(ic2-icin2).eq.32)	! capitals
	    if (lg0.OR.lg1.OR.lg2.OR.lg3) then
		write(*,11) zin(j), ain(j), namin(j), bin(j), ssin(j), sain(j)
		nam(i) = namin(j)
		Zl(i) = zin(j)
		Al(i) = ain(j)
		b_bar(i) = bin(j)
		sig_scatt(i) = ssin(j)
		sig_abs(i) = sain(j)
		m1 = 1
	    end if
	end do
	    if (m1.eq.0) m2=0
	end do
	if (m2.eq.0) then
	    write(*,*) ' An element was not found in the file ', file
	    stop
	end if
c
c --- calculation section -------------------------------------------------
	a22_to_a1 = 2200.0/sqrt(81.787/5.2276e-6)	! 'blue card' values
c factor to convert absorption cross-section from 2200ms**-1 to 1 Angstrom
c
	A_tot = 0.0
	sa1 = 0.0
	bb = 0.0
	bf = 0.0
	ssb = 0.0
	ssf = 0.0
	do ix=1,N
	    A_tot = A_tot + Al(ix)*fnl(ix)
	    sa1 = sa1 + sig_abs(ix)*fnl(ix)
	    bb = bb + b_bar(ix)*fnl(ix)
	    bf = bf + fnl(ix)*fn_bf(b_bar(ix),Al(ix))
	    ssb = ssb + sig_scatt(ix)*fnl(ix)
	    ssf = ssf + fnl(ix)*fn_sf(sig_scatt(ix),Al(ix))
	end do
	sa2 = sa1*a22_to_a1
	fden = 1.6605655
c
c --- data output section -------------------------------------------------
	write(*,*)
	open(unit=69,file='xsect.lis',status='new')
	write(69,*) ' *********************************************'
	write(69,*) ' * CROSS Calculate sample cross-sections etc *'
	write(69,*) ' *********************************************'
	write(69,*)
	write(69,*) ' Units: b in fm, sigma in barns'
	write(69,*)
	write(69,*) ' PRIMARY DATA...'
	write(69,*) '  Z    A         Symbol b_bar      sig_scatt    sig_abs'
	do j=1,N
	  write(69,11) Zl(j),Al(j),nam(j),b_bar(j),sig_scatt(j),sig_abs(j)
	end do
	write(69,*)
	iunit = 5
	do jj=1,2
	write(iunit,*) ' Number of atoms in scattering unit  = ', fNsu
	if (rho.gt.small) then
	write(iunit,*) ' Density in gcm**-3                  = ', rho
	else if (rho.lt.-small) then
	write(iunit,21) -rho
 21	format('  Density in atoms per cubic Angstrom = ',f10.6)
	else
	write(iunit,*) ' Density not known '
	end if
	write(iunit,*)
	write(iunit,*) '          Atomic   '
	write(iunit,*) ' Element  Fraction '
	do i=1,N
	    write(iunit,20) nam(i), fnl(i)/fNsu
 20	    format('   ',a2,'    ',f9.5)
	end do
	write(iunit,*)
	write(iunit,*) ' VALUES PER SCATTERING UNIT... '
	write(iunit,40) A_tot
	nv = 0.0
	if (rho.gt.small) then
	    nv = rho/(A_tot*fden)
	else if (rho.lt.-small) then
	    nv = -rho/fNsu
	end if
	if (nv.gt.small) then
	    write(iunit,47) nv
	    write(iunit,48) nv*0.04*pi*bb*bb
	end if
	write(iunit,41) bb
	write(iunit,42) bf
	write(iunit,43) ssb
	write(iunit,53) ssb/(4.0*pi)
	write(iunit,44) ssf
	write(iunit,54) ssf/(4.0*pi)
	write(iunit,45) sa1
	write(iunit,46) sa2
	write(iunit,49) sa2*1.8
	write(iunit,*)
	write(iunit,*) ' The total correlation function is a sum of the'
	write(iunit,*) ' FZ partial correlation functions with the'
	write(iunit,*) ' following coefficients;'
	do i=1,N
	  coeff=0.01*fnl(i)*fnl(i)*b_bar(i)*b_bar(i)
	  write(iunit,111) nam(i), nam(i), coeff
	end do
	do i=1,N
	do i2=i+1,N
	    n1 = nam(i)
	    n2 = nam(i2)
	    coeff = 0.02*fnl(i)*fnl(i2)*b_bar(i)*b_bar(i2)
	    write(iunit,111) n1, n2, coeff
	end do
	end do
	write(iunit,*)
	write(iunit,*) ' AVERAGE VALUES PER ATOM... '
	write(iunit,40) A_tot/fNsu
	nv = 0.0
	if (rho.gt.small) then
	    nv = rho*fNsu/(A_tot*fden)
	else if (rho.lt.-small) then
	    nv = -rho
	end if
	if (nv.gt.small) then
	    write(iunit,47) nv
	    write(iunit,48) nv*0.04*pi*bb*bb/(fNsu*fNsu)
	end if
	write(iunit,41) bb/fNsu
	write(iunit,42) bf/fNsu
	write(iunit,43) ssb/fNsu
	write(iunit,53) ssb/(fNsu*4.0*pi)
	write(iunit,44) ssf/fNsu
	write(iunit,54) ssf/(fNsu*4.0*pi)
	write(iunit,45) sa1/fNsu
	write(iunit,46) sa2/fNsu
	write(iunit,49) (1.8*sa2)/fNsu
	write(iunit,*)
	write(iunit,*) ' The total correlation function is a sum of the'
	write(iunit,*) ' FZ partial correlation functions with the'
	write(iunit,*) ' following coefficients;'
	do i=1,N
	  coeff=0.01*fnl(i)*fnl(i)*b_bar(i)*b_bar(i)/(fNsu*fNsu)
	  write(iunit,111) nam(i), nam(i), coeff
	end do
	do i=1,N
	do i2=i+1,N
	    n1 = nam(i)
	    n2 = nam(i2)
	    coeff = 0.02*fnl(i)*fnl(i2)*b_bar(i)*b_bar(i2)/(fNsu*fNsu)
	    write(iunit,111) n1, n2, coeff
	end do
	end do
	write(iunit,*)
	write(iunit,*) ' (bound sig_scatt/4pi is self scattering level) '
	iunit = 69
	end do
	close(69)
	write(*,*)
	write(*,*) ' *** OUTPUT IS IN FILE XSECT.LIS *** '
	write(*,*)
c
	stop
c
 98	write(*,*) ' Error finding file ', file
	stop
 40	format('  Atomic weight                            = ',f10.4)
 41	format('  Bound coherent scattering length         = ',f10.4)
 42	format('  Free coherent scattering length          = ',f10.4)
 43	format('  Bound scattering cross-section           = ',f10.4)
 53	format('  Bound scattering cross-section/4pi       = ',f10.4)
 44	format('  Free scattering cross-section            = ',f10.4)
 54	format('  Free scattering cross-section/4pi        = ',f10.4)
 45	format('  Absorption cross-section at 2200ms**-1   = ',f10.4)
 46	format('  Absorption cross-section at 1 Angstrom   = ',f10.4)
 47	format('  Density (units per cubic Angstrom)       = ',f10.6)
 48	format('  T0(r) Constant (4*pi*n*b**2)             = ',f10.4)
 49	format('  Absorption cross-section at 1.8 Angstrom = ',f10.4)
 111	format(2x,a2,'-',a2,' :',f10.4)
c
	end
c
	Function fn_bf(fn_bb,A)
	real fn_bb, A
	fn_bf = fn_bb*A/(A+1.0)
	end
c
	Function fn_sf(fn_sb,A)
	real fn_sb, A
	fn_sf = fn_sb*A*A/((A+1.0)*(A+1.0))
	end
c
