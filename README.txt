{\rtf1\ansi\ansicpg1252\cocoartf1561\cocoasubrtf400
{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx6644\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 So there is an error in the PELT.c file from what I can tell. Im fairly sure its something to do with ndone and nupdate. I have already tried lots of things and I\'92m using the PELT.online() R function in PELT.online_one_func.R and PELT.online.initialise in wrapper.R to try and get outputs. \
\
The update function needs to be done but when the C was working that was the only R function not running everything else output answers.\
\
 As for as the other errors we discussed (the big number and zeros have been dealt with so when it does run it runs correctly)\
\
As a side note, I assume for the update one we now want to call PELT.c instead and so needs to be in the same format as initialise is above?}