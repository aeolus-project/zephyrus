#!/bin/bash

awk '{ sum=0; sumsq=0; min=$1; max=$1;
       for(i=1; i<=NF;i++) {sum+=$i; sumsq+=$i*$i; if($i < min) min=$i; if($i > max) max=$i }
       printf("sumsq=%f,sum=%f,mean=%f,stddev=%f,min=%f,max=%f\n", sumsq, sum, sum/NF, sqrt((sumsq-(NF*((sum/NF))*(sum/NF)))/NF), min, max) } ' $1