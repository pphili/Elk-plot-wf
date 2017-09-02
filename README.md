# Elk-plot-wf
## Output the Kohn-Sham orbitals from the open source DFT code Elk

The subroutines contained in this repository are modifications of existing Elk subroutines. These files were created for Elk 3.1.12. After downloading the Elk code (http://elk.sourceforge.net/#download), these files can be added to the elk-3.1.12/src. If the code is then compiled, you will be able to extract the Kohn-Sham (KS) orbitals of a specific band of interest at a specific k point on a grid in real space in one, two or three dimensions.

The new subroutines are init9.f90, psik.f90, psiplot.f90, and psish.f90 and they require the use of the module modmain2.f90. The main elk.f90 file must also be updated so that the new subroutines can be accessed.

In order to use extract the KS orbitals on a grid in real (position) space, set the tasks in the input file to 961 for plotting in 1D, 962 for plotting in 2D and 963 for 3D plotting. These tasks are very similar to tasks 61, 62, and 63 which plot the density ( norm squared of the wavefunction) in 1D, 2D or 3D. To specify the regions where plotting is wanted, look at specifications for the plot1d, plot2d and plot3d subroutines in the elk manual situated in the elk-3.1.12/docs directory. 

Tasks 961, 962 and 963 output the real and imaginary parts of the up and down components of the KS spinors separately. Each task therefore outputs 4 files, one for each of the components in the specified region.  
