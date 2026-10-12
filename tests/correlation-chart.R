source(file.path("helpers", "native-guard.R"))
if(Sys.getenv('MEASUREMENTPKG_SOURCE')!='')for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(Sys.getenv('MEASUREMENTPKG_SOURCE')!='')get(n)else getFromNamespace(n,'bmeas')
chart<-private('.bh_correlation_chart');inverse<-private('.bh_correlation_inverse')
passed<-0L;ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
edges<-which(lower.tri(matrix(0,4,4)),arr.ind=TRUE)
for(bits in 0:63) {
  mask<-matrix(FALSE,4,4)
  for(i in 1:6)mask[edges[i,1],edges[i,2]]<-mask[edges[i,2],edges[i,1]]<-bitwAnd(bits,bitwShiftL(1L,i-1L))!=0L
  R<-diag(4)
  for(i in 1:6)if(mask[edges[i,1],edges[i,2]])R[edges[i,1],edges[i,2]]<-R[edges[i,2],edges[i,1]]<-.12*sin(i)
  raw<-inverse(R,mask);result<-chart(raw,mask)
  ok(length(raw)==sum(mask[lower.tri(mask)]) && max(abs(result$R-R))<1e-12,paste('all4node masks',bits))
}
empty<-chart(numeric(),matrix(FALSE,1,1))
ok(identical(empty$R,matrix(1,1,1)) && empty$log_jacobian==0,'one component has no correlation unknown/Jacobian')
mask<-matrix(FALSE,6,6);mask[1:3,4:6]<-TRUE;mask[4:6,1:3]<-TRUE
mask[2,6]<-mask[6,2]<-mask[3,5]<-mask[5,3]<-FALSE
C<-matrix(c(.2,.1,-.05,.08,.25,0,-.03,0,.15),3,byrow=TRUE)
R<-rbind(cbind(diag(3),C),cbind(t(C),diag(3)))
raw<-inverse(R,mask);actual<-chart(raw,mask)
ok(length(raw)==7L && max(abs(actual$R-R))<1e-12,'S17 all seven final marginal coordinates, including cross zeros')
step<-1e-6
jac<-vapply(seq_along(raw),function(i){a<-b<-raw;a[i]<-a[i]+step;b[i]<-b[i]-step;(chart(a,mask)$R-chart(b,mask)$R)[lower.tri(mask)&mask]/(2*step)},numeric(length(raw)))
ok(abs(actual$log_jacobian-as.numeric(determinant(jac,logarithm=TRUE)$modulus))<1e-8,'reduced Jacobian relative to unique final marginal coordinates')
permutation<-c(3,6,1,4,2,5)
other<-chart(inverse(R[permutation,permutation],mask[permutation,permutation]),mask[permutation,permutation])
ok(abs(actual$log_determinant-other$log_determinant)<1e-12,'same final-R determinant kernel under simultaneous relabeling')
ok(max(abs(actual$R[!mask & row(mask)!=col(mask)]))<1e-12 && min(eigen(actual$R,symmetric=TRUE,only.values=TRUE)$values)>0,'prescribed zeros and strict positive definiteness without clipping')
moderately_extreme<-chart(100*cos(seq_len(7)*.73),mask)
ok(max(abs(tcrossprod(moderately_extreme$L)-moderately_extreme$R))<1e-10,'ill-conditioned but numerically resolved state preserves actual covariance')
for(scale in c(1000,1e5,1e160)) {
  e<-tryCatch(chart(scale*cos(seq_len(7)*.73),mask),error=identity)
  ok(inherits(e,'measurement_error')&&identical(e$code,'E_CORRELATION_NUMERIC'),paste('explicit numerical failure without clipping',scale))
}
full3<-matrix(TRUE,3,3);diag(full3)<-FALSE
derivative<-(chart(c(step,.3,.7),full3)$R[1,3]-chart(c(-step,.3,.7),full3)$R[1,3])/(2*step)
ok(abs(derivative-.7/sqrt(1+.3^2+.7^2))<1e-8,'zero-axis forward map derivative oracle; native AD is checked separately')
cat('Correlation chart tests:',passed,'passed; no Stan or sampling.\n')
