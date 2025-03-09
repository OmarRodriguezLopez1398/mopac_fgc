# Classical molecular-mechanics corrections

This directory contains all of the classical interatomic potential terms that are used to
correct the quantum mechanical calculations performed by MOPAC. The oldest corrections were
introduced to fix very specific problems in certain covalent bonds (e.g. triple bonds), and
the more recent corrections have focused on weak intermolecular interactions, mainly
dispersion and hydrogen bonding.

The D3 dispersion model implementation was derived and adapted from the DFTD3 library
[https://github.com/dftbplus/dftd3-lib] with permission from Stephan Grimme. The files
derived from this source are `copyc6.F90`, `dftd3.F90`, `dftd3_bits.F90`, and `gdisp.F90`.

The H4 model implementation was derived and adapted from the h_bonds4 library written by
Jan Rezac, available either as a standalone C implementation [https://www.rezacovi.cz]
or in Ruby as part of the Cuby4 framework [http://cuby4.molecular.cz]. The file derived from
this source is `H_bonds4.F90`.

PM6-FGC Correction
Analytical corrections were developed to improve the accuracy of the PM6 semiempirical quantum 
mechanical method for the evaluation of noncovalent interaction energies. In the FGC (from functional 
group corrections) approach, corrections were made from atom-atom pairs that depend on the nature of 
the interacting functional groups. The file derived from this source is `fgc_correction.F90`.
To run calculations using this correction you must first have a setup file where the pairs of interacting 
atom types are shown. Run the `assistant_script.py` python script first to get the necessary setup file.

