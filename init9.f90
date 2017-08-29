subroutine init9

use modmain
use modmain2


! allocate charge wavefunction arrays
if (allocated(rpsiupmt)) deallocate(rpsiupmt)
allocate(rpsiupmt(lmmaxvr,nrmtmax,natmtot))
if (allocated(rpsiupir)) deallocate(rpsiupir)
allocate(rpsiupir(ngtot))

if (allocated(ipsiupmt)) deallocate(ipsiupmt)
allocate(ipsiupmt(lmmaxvr,nrmtmax,natmtot))
if (allocated(ipsiupir)) deallocate(ipsiupir)
allocate(ipsiupir(ngtot))

if (allocated(rpsidownmt)) deallocate(rpsidownmt)
allocate(rpsidownmt(lmmaxvr,nrmtmax,natmtot))
if (allocated(rpsidownir)) deallocate(rpsidownir)
allocate(rpsidownir(ngtot))

if (allocated(ipsidownmt)) deallocate(ipsidownmt)
allocate(ipsidownmt(lmmaxvr,nrmtmax,natmtot))
if (allocated(ipsidownir)) deallocate(ipsidownir)
allocate(ipsidownir(ngtot))
!allocate other arrays

if (allocated(densmt)) deallocate(densmt)
allocate(densmt(lmmaxvr,nrmtmax,natmtot))
if (allocated(densir)) deallocate(densir)
allocate(densir(ngtot))

if (allocated(densupmt)) deallocate(densupmt)
allocate(densupmt(lmmaxvr,nrmtmax,natmtot))
if (allocated(densupir)) deallocate(densupir)
allocate(densupir(ngtot))

if (allocated(densdownmt)) deallocate(densdownmt)
allocate(densdownmt(lmmaxvr,nrmtmax,natmtot))
if (allocated(densdownir)) deallocate(densdownir)
allocate(densdownir(ngtot))

return
end subroutine
!EOC
