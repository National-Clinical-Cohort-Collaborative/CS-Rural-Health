EUG Presentation 2023-02-03
=================

Jerrod Anzalone & Will Beasley

Shawn's prompt: 
> EUG is back! This week at the Enclave Users’ Group we’ll discuss some of the special considerations for running R transforms in Code Workbooks, especially around data type issues and performance. Stop by to share your own experiences and see how other are getting their stats done.


~30 min of presentation

Jerrod 

* plot output 
  * dimensions
  * file format
  * resolution
* environment profiles

Will

* RDS
* sparkr
* asserts for data 
* R analysis transform produces (a) regression table and (b) graph
* For each column, identify the smallest data type that appropriately contains the variable's info.  The smaller the rds, the quicker the files is written & read.  The smaller the data.frame, the less RAM is required and more efficient the CPU & retrieval/caching mechanism can be.  

  * boolean *vs* integer/factor.  R internally represents factors with 32-bit integers
  * date *vs* datetime
  * integer *vs* floating point (32 vs 64 bits)
