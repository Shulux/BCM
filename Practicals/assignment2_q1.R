## Formative Question 1 code

MH=function(N=1000,sigma=1,x0=0)
{
  x=rep(0,N)
  x[1]=x0
  for(i in 2:N)
  {
    y=rnorm(1,x[i-1],sigma)
    lalpha=abs(x[i-1])-abs(y)
    if(log(runif(1))<lalpha)
    {
      x[i]=y
    }else{
      x[i]=x[i-1]
    }
  }
  return(x)
}

