
! Copyright (C) 2002-2005 J. K. Dewhurst, S. Sharma and C. Ambrosch-Draxl.
! This file is distributed under the terms of the GNU General Public License.
! See the file COPYING for license details.

subroutine psiplot
use modmain
use modmain2
implicit none
! local variables
integer ik,ist,ispn
real(8) x,t1
! allocatable arrays
complex(8), allocatable :: apwalm(:,:,:,:,:)
complex(8), allocatable :: evecfv(:,:),evecsv(:,:)
! external functions
real(8) sdelta
external sdelta
! initialise universal variables
call init0
call init1
call init9
! read the density and potentials from file
call readstate
! read Fermi energy from file
call readfermi
! find the new linearisation energies
call linengy
! generate the APW radial functions
call genapwfr
! generate the local-orbital radial functions
call genlofr
! set the occupancies
  ik=kstlist(1,1)
  ist=kstlist(2,1)
  if ((ik.lt.1).or.(ik.gt.nkpt)) then
    write(*,*)
    write(*,'("Error(wfplot): k-point out of range : ",I8)') ik
    write(*,*)
    stop
  end if
  if ((ist.lt.1).or.(ist.gt.nstsv)) then
    write(*,*)
    write(*,'("Error(wfplot): state out of range : ",I8)') ist
    write(*,*)
    stop
  end if
! plotting a single wavefunction
  occsv(:,:)=0.d0
  occsv(ist,ik)=1.d0/wkpt(ik)
! set the wavefunction to zero
rpsiupmt(:,:,:)=0.d0
rpsiupir(:)=0.d0
ipsiupmt(:,:,:)=0.d0
ipsiupir(:)=0.d0
rpsidownmt(:,:,:)=0.d0
rpsidownir(:)=0.d0
ipsidownmt(:,:,:)=0.d0
ipsidownir(:)=0.d0
densmt(:,:,:)=0.d0
densir(:)=0.d0
densupmt(:,:,:)=0.d0
densupir(:)=0.d0
densdownmt(:,:,:)=0.d0
densdownir(:)=0.d0

! compute the charge density with the new occupancies
allocate(apwalm(ngkmax,apwordmax,lmmaxapw,natmtot,nspnfv))
allocate(evecfv(nmatmax,nstfv),evecsv(nstsv,nstsv))
do ik=1,nkpt
! get the eigenvectors from file
  call getevecfv(filext,vkl(:,ik),vgkl(:,:,:,ik),evecfv)
  call getevecsv(filext,vkl(:,ik),evecsv)
! find the matching coefficients
  do ispn=1,nspnfv
    call match(ngk(ispn,ik),gkc(:,ispn,ik),tpgkc(:,:,ispn,ik), &
     sfacgk(:,:,ispn,ik),apwalm(:,:,:,:,ispn))
  end do
! add to the density
  call psik(ngk(:,ik),igkig(:,:,ik),wkpt(ik),occsv(:,ik),apwalm,evecfv, &
   evecsv)
end do
deallocate(apwalm,evecfv,evecsv)
! convert muffin-tin density/magnetisation to spherical harmonics
call psish
! convert the density from a coarse to a fine radial mesh
call rfmtctof(rpsiupmt)
call rfmtctof(ipsiupmt)
call rfmtctof(rpsidownmt)
call rfmtctof(ipsidownmt)
call rfmtctof(densmt)
call rfmtctof(densupmt)
call rfmtctof(densdownmt)
! write the wavefunction modulus squared plot to file
select case(task)
case(961)
  open(50,file='rpsiup1D.OUT',action='WRITE',form='FORMATTED')
  open(51,file='WFLINES.OUT',action='WRITE',form='FORMATTED')
  call plot1d(50,51,1,rpsiupmt,rpsiupir)
  close(50)
  close(51)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'(" The real part of the up component of the KS spinor along 1 direction written to rpsiup1D.OUT")')
  open(50,file='ipsiup1D.OUT',action='WRITE',form='FORMATTED')
  open(51,file='WFLINES.OUT',action='WRITE',form='FORMATTED')
  call plot1d(50,51,1,ipsiupmt,ipsiupir)
  close(50)
  close(51)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'(" The imaginary part of the up component of the KS spinor along 1 direction written to ipsiup1D.OUT")')
  open(50,file='rpsidown1D.OUT',action='WRITE',form='FORMATTED')
  open(51,file='WFLINES.OUT',action='WRITE',form='FORMATTED')
  call plot1d(50,51,1,rpsidownmt,rpsidownir)
  close(50)
  close(51)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'(" The real part of the down component of the KS spinor along 1 direction written to rpsidown1D.OUT")')
  open(50,file='ipsidown1D.OUT',action='WRITE',form='FORMATTED')
  open(51,file='WFLINES.OUT',action='WRITE',form='FORMATTED')
  call plot1d(50,51,1,ipsidownmt,ipsidownir)
  close(50)
  close(51)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'(" The imaginary part of the down component of the KS spinor along 1 direction written to ipsidown1D.OUT")')
  open(50,file='dens1D.OUT',action='WRITE',form='FORMATTED')
  open(51,file='WFLINES.OUT',action='WRITE',form='FORMATTED')
  call plot1d(50,51,1,densmt,densir)
  close(50)
  close(51)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'(" The density of the KS spinor along 1 direction written to dens1D.OUT")')
  open(50,file='densup1D.OUT',action='WRITE',form='FORMATTED')
  open(51,file='WFLINES.OUT',action='WRITE',form='FORMATTED')
  call plot1d(50,51,1,densupmt,densupir)
  close(50)
  close(51)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'(" The density of the up component of KS spinor along 1 direction written to densup1D.OUT")')
  open(50,file='densdown1D.OUT',action='WRITE',form='FORMATTED')
  open(51,file='WFLINES.OUT',action='WRITE',form='FORMATTED')
  call plot1d(50,51,1,densdownmt,densdownir)
  close(50)
  close(51)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'(" The density of the spin down KS spinor along 1 direction written to densdown1D.OUT")')
  write(*,'(" vertex location lines written to WFLINES.OUT")')


case(962)
  open(50,file='rpsiup2D.OUT',action='WRITE',form='FORMATTED')
  call plot2d(50,1,rpsiupmt,rpsiupir)
  close(50)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'(" The real part of the up component of the KS spinor in a plane written to rpsiup2D.OUT")')
  open(50,file='ipsiup2D.OUT',action='WRITE',form='FORMATTED')
  call plot2d(50,1,ipsiupmt,ipsiupir)
  close(50)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'(" The imaginary part of the up component of the KS spinor in a plane written to ipsiup2D.OUT")')
  open(50,file='rpsidown2D.OUT',action='WRITE',form='FORMATTED')
  call plot2d(50,1,rpsidownmt,rpsidownir)
  close(50)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'(" The real part of the down component of the KS spinor in a plane written to rpsidown2D.OUT")')
  open(50,file='ipsidown2D.OUT',action='WRITE',form='FORMATTED')
  call plot2d(50,1,ipsidownmt,ipsidownir)
  close(50)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'(" The imaginary part of the down component of the KS spinor in a plane written to ipsidown2D.OUT")')
case(963)
  open(50,file='rpsiup3D.OUT',action='WRITE',form='FORMATTED')
  call plot3d(50,1,rpsiupmt,rpsiupir)
  close(50)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'(" The real part of the up component of the KS spinor in 3D written to rpsiup3D.OUT")')
 open(50,file='ipsiup3D.OUT',action='WRITE',form='FORMATTED')
  call plot3d(50,1,ipsiupmt,ipsiupir)
  close(50)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'(" The imaginary part of the up component of the KS spinor in 3D written to ipsiup3D.OUT")')
 open(50,file='rpsidown3D.OUT',action='WRITE',form='FORMATTED')
  call plot3d(50,1,rpsidownmt,rpsidownir)
  close(50)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'("The real part of the down component of the KS spinor in 3D written to rpsidown3D.OUT")')
 open(50,file='ipsidown3D.OUT',action='WRITE',form='FORMATTED')
  call plot3d(50,1,ipsidownmt,ipsidownir)
  close(50)
  write(*,*)
  write(*,'("Info(wfplot):")')
  write(*,'(" The imaginary part of the down component of the KS spinor in 3D written to ipsidown3D.OUT")')
end select
if (task.ne.162) then
  write(*,'(" for k-point ",I8," and state ",I6)') kstlist(1,1),kstlist(2,1)
end if
return
end subroutine

