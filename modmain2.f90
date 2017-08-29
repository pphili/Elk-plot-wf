
module modmain2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! muffin-tin wavefunction
real(8), allocatable :: rpsiupmt(:,:,:)
real(8), allocatable :: ipsiupmt(:,:,:)
real(8), allocatable :: rpsidownmt(:,:,:)
real(8), allocatable :: ipsidownmt(:,:,:)
! interstitial real-space wavefunction
real(8), allocatable :: rpsiupir(:)
real(8), allocatable :: ipsiupir(:)
real(8), allocatable :: rpsidownir(:)
real(8), allocatable :: ipsidownir(:)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real(8), allocatable :: densmt(:,:,:)
real(8), allocatable :: densir(:)

real(8), allocatable :: densupmt(:,:,:)
real(8), allocatable :: densupir(:)

real(8), allocatable :: densdownmt(:,:,:)
real(8), allocatable :: densdownir(:)



end module
