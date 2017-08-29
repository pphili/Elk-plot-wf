
! Copyright (C) 2009 J. K. Dewhurst, S. Sharma and E. K. U. Gross
! This file is distributed under the terms of the GNU Lesser General Public
! License. See the file COPYING for license details.

!BOP
! !ROUTINE: psish
! !INTERFACE:
subroutine psish
! !USES:
use modmain
use modmain2
! !DESCRIPTION:
!   Converts the muffin-tin density and magnetisation from spherical coordinates
!   to a spherical harmonic expansion. See {\tt rhomagk}.
!
! !REVISION HISTORY:
!   Created January 2009 (JKD)
!EOP
!BOC
implicit none
! local variables
integer idm,is,ias
integer nr,nri,nrc,nrci
! allocatable arrays
real(8), allocatable :: rfmt(:,:)
!$OMP PARALLEL DEFAULT(SHARED) &
!$OMP PRIVATE(rfmt,is,nr,nri) &
!$OMP PRIVATE(nrc,nrci,idm)
!$OMP DO
do ias=1,natmtot
  is=idxis(ias)
  nr=nrmt(is)
  nri=nrmtinr(is)
  nrc=nrcmt(is)
  nrci=nrcmtinr(is)
  allocate(rfmt(lmmaxvr,nrcmtmax))
! convert the density to spherical harmonics
  call rfcpy(nr,nri,rpsiupmt(:,:,ias),rfmt)
  call rfsht(nrc,nrci,1,rfmt,lradstp,rpsiupmt(:,:,ias))
  deallocate(rfmt)
  allocate(rfmt(lmmaxvr,nrcmtmax))
  call rfcpy(nr,nri,ipsiupmt(:,:,ias),rfmt)
  call rfsht(nrc,nrci,1,rfmt,lradstp,ipsiupmt(:,:,ias))
  deallocate(rfmt)  

  allocate(rfmt(lmmaxvr,nrcmtmax))
  call rfcpy(nr,nri,rpsidownmt(:,:,ias),rfmt)
  call rfsht(nrc,nrci,1,rfmt,lradstp,rpsidownmt(:,:,ias))
  deallocate(rfmt)  

  allocate(rfmt(lmmaxvr,nrcmtmax))
  call rfcpy(nr,nri,ipsidownmt(:,:,ias),rfmt)
  call rfsht(nrc,nrci,1,rfmt,lradstp,ipsidownmt(:,:,ias))
  deallocate(rfmt)   
  
  allocate(rfmt(lmmaxvr,nrcmtmax))
  call rfcpy(nr,nri,densmt(:,:,ias),rfmt)
  call rfsht(nrc,nrci,1,rfmt,lradstp,densmt(:,:,ias))
  deallocate(rfmt)   

  allocate(rfmt(lmmaxvr,nrcmtmax))
  call rfcpy(nr,nri,densupmt(:,:,ias),rfmt)
  call rfsht(nrc,nrci,1,rfmt,lradstp,densupmt(:,:,ias))
  deallocate(rfmt) 

  allocate(rfmt(lmmaxvr,nrcmtmax))
  call rfcpy(nr,nri,densdownmt(:,:,ias),rfmt)
  call rfsht(nrc,nrci,1,rfmt,lradstp,densdownmt(:,:,ias))
  deallocate(rfmt) 

  !allocate(rfmt(lmmaxvr,nrcmtmax))
  !call rfcpy(nr,nri,psiupmt(:,:,ias),rfmt)
  !call rfsht(nrc,nrci,1,rfmt,lradstp,psiupmt(:,:,ias))
  !deallocate(rfmt) 
end do
!$OMP END DO
!$OMP END PARALLEL
return

contains

subroutine rfcpy(nr,nri,rfmt1,rfmt2)
implicit none
! arguments
integer, intent(in) :: nr,nri
real(8), intent(in) :: rfmt1(lmmaxvr,nrmtmax)
real(8), intent(out) :: rfmt2(lmmaxvr,nrcmtmax)
! local variables
integer ir,irc
irc=0
do ir=1,nri,lradstp
  irc=irc+1
  call dcopy(lmmaxinr,rfmt1(:,ir),1,rfmt2(:,irc),1)
end do
do ir=nri+lradstp,nr,lradstp
  irc=irc+1
  call dcopy(lmmaxvr,rfmt1(:,ir),1,rfmt2(:,irc),1)
end do
return
end subroutine

end subroutine
!EOC
