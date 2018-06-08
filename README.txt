So there is an error in the PELT.c file from what I can tell. Im fairly sure its something to do with ndone and nupdate. I have already tried lots of things and I’m using the PELT.online() R function in PELT.online_one_func.R and PELT.online.initialise in wrapper.R to try and get outputs. 

The update function needs to be done but when the C was working that was the only R function not running everything else output answers.

 As for as the other errors we discussed (the big number and zeros have been dealt with so when it does run it runs correctly)

As a side note, I assume for the update one we now want to call PELT.c instead and so needs to be in the same format as initialise is above?

The below error kept popping up also: PELT.c:140:19: warning: incompatible integer to pointer conversion assigning to 'int *' from 'int' [-Wint-conversion]

Other warnings on check are to do with missing .rd files for specific functions or rd files for functions that don't exist

okay so currently I am nearly 100% sure the error in the c code is with cptsout which isn't being defined properly on the last line to be printed out or changed so it is just letting out cptsout as a vector of zeros. Trying to solve this now!