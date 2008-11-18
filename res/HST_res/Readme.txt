From sbaggett@stsci.edu Fri May 30 15:12:35 1997
Received: Fri, 30 May 97 15:12:35 EDT from tib.stsci.edu by nemesis.stsci.edu (4.1)
Received: from papageno.stsci.edu by tib.stsci.edu (SMI-8.6/SMI-SVR4)
	id PAA19236; Fri, 30 May 1997 15:12:34 -0400
Received: by papageno.stsci.edu (SMI-8.6/SMI-SVR4)
	id PAA28460; Fri, 30 May 1997 15:12:34 -0400
Date: Fri, 30 May 1997 15:12:34 -0400
From: sbaggett@stsci.edu (Sylvia Baggett)
Message-Id: <199705301912.PAA28460@papageno.stsci.edu>
To: wiggs@stsci.edu
Subject: readme.txt
Status: RO


Throughput Curves for WFPC2:

Last updated 30/05/97 by S. Baggett, S. Casertano, and M. Wiggs.

This directory contains ASCII versions of the WFPC2 filter 
throughput tables.

We provide tables for each filter in isolation as well as for each
filter including the system response and CCD QE.

All tables were created using the synphot package of STSDAS (23 April
1997 version). The tables each contain two columns, wavelength and    
throughput. Note that with the May 1997 update, the chip dqe's are no 
longer identical and so, system throughputs are chip-dependent.
 
The tables have the following naming convention:

filtername.txt           Throughput tables for each
                         filter in isolation
ccd_filtername_sys.txt   Throughput tables for each filter
                         including the system response
                         and CCD QE.
									   
Questions or comments may be sent to help@stsci.edu

