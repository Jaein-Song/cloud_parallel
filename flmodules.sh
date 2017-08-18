#!/bin/bash
cat <<eof >inputlists.f90
    module inputlists
        implicit none
            character(len=*), parameter     :: ceilifl='$current_dir/ceil/ifl$ppn'
            character(len=*), parameter     :: covifl1='$current_dir/covupCFrad/ifl$ppn'
            character(len=*), parameter     :: covofl1='$current_dir/covupCFrad/ofl$ppn'
            character(len=*), parameter     :: covofl2='$current_dir/covupCFrad/ofl14$ppn'
            character(len=*), parameter     :: covofl3='$current_dir/covupCFrad/ofl15$ppn'
            character(len=*), parameter     :: totifl1='$current_dir/totalQC/ifl$ppn'
            character(len=*), parameter     :: totcfl1='$current_dir/totalQC/cfl$ppn'
            
    end module inputlists
eof
$FC -c inputlists.f90
