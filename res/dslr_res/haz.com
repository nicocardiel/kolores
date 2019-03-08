#!/bin/bash
\rm -f dslr_res
cat respB4_D3sReldiodosicor.csv \
    respG2_D3sReldiodosicor.csv \
    respG3_D3sReldiodosicor.csv \
    respR1_D3sReldiodosicor.csv \
    > dslr_res
