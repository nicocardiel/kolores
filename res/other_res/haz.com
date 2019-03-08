#!/bin/bash
\rm -f other_res
cat kpno_U.dat \
    kpno_B.dat \
    kpno_V.dat \
    kpno_R.dat \
    kpno_I.dat \
    U3_pickles98.dat \
    B2_pickles98.dat \
    B3_pickles98.dat \
    V_pickles98.dat \
    Rc_pickles98.dat \
    Ic_pickles98.dat \
    J_pickles98.dat \
    H_pickles98.dat \
    K_pickles98.dat \
    L_pickles98.dat \
    Lp_pickles98.dat \
    M_pickles98.dat \
    box3900.dat \
    box4500.dat \
    box5100.dat \
    box5800.dat \
    box6400.dat \
    box7000.dat \
    box4393.dat \
    box5439.dat \
    > other_res
