! ------------------------------------------------
! Simple Hello World Example
!
! Berlin Brown
!
! Note: the following meta files are included
! for this project.
! authors.txt myhelloworld.factor summary.txt tags.txt
!
! Additional Help:
!
! At any time, use the help system to look up 
! information on words and vocabularies:
!
! \ dup help
! ------------------------------------------------
USE: io

! IN: - definition:
! Sets the current vocabulary where new words will be defined, 
! creating the vocabulary first if it does not exist. After the vocabulary has been created, 
! it can be listed in USE: and USING: declarations.
IN: myhelloworld 

! ---------------------
! Hello word definition
! ---------------------
: hello ( -- ) 
  "Hello world" print ;

! MAIN - definition:
! Defines the main entry point for the current vocabulary. 
! This word will be executed when this vocabulary is passed to run.
MAIN: hello


