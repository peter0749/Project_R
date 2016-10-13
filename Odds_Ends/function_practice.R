rm(list=ls(all=TRUE))##initialization
##prefixsum
prefixsum <- function(vec,from=2,to=length(vec))
{
  if(to>length(vec) || to < 1) to = length(vec)
  if(from<2 || from>to ) from = 2
  returnval = as.vector(rep(c(0),to))
  returnval[1] = vec[1]
  for(i in from:to)
  {
    returnval[i] = returnval[i-1] + vec[i]
  }
  return (returnval);
}
##end of prefixsum

vec = 1:10
sum = prefixsum(vec)
print(sum)

## A cool qsort example
quickSort <- function(v) {
  if(length(v)<=1)    return (v);
  element = v[1];
  partition = v[-1];
  v1 <- partition[ partition<element ];
  v2 <- partition[ partition>=element ];
  quickSort(v1);
  quickSort(v2);
  return (c(v1,element,v2));
}

vec = -sum

quickSort(vec)
