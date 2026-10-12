source(file.path('helpers','native-guard.R'))
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L
ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(expr,code){e<-tryCatch(force(expr),error=identity);ok(inherits(e,'measurement_error')&&identical(e$code,code),code)}
# Two 3x3 blocks: fixed a and free rho imply |rho|<sqrt(1-a^2).
make_table<-function(shared=FALSE) {
  pairs<-t(combn(9L,2L));x<-data.frame(row=pairs[,1],col=pairs[,2],state='zero',fixed_value=0,parameter=NA_character_)
  set<-function(a,b,state,value=NA_real_,id=NA_character_) {
    i<-which(x$row==a&x$col==b);x$state[i]<<-state;x$fixed_value[i]<<-value;x$parameter[i]<<-id
  }
  set(1,2,'fixed',.2);set(1,3,'free',id='rho1')
  set(4,5,'fixed',.3);set(4,6,'free',id=if(shared)'rho1'else'rho2')
  set(7,8,'fixed',.4);set(7,9,'free',id=if(shared)'rho2'else'rho3');x
}
tab<-make_table();domain<-private('.bh_cor_domain')(tab,9L)
ok(identical(domain$product_status,'available')&&domain$dimension==3L,'three independent scientific labels obtain a product domain')
widths<-sqrt(1-c(.2,.3,.4)^2)
ok(max(abs(vapply(domain$product_domains,`[[`,numeric(1),'half_width')-widths))<1e-10,'full analytic intervals; no inner-support clipping')
z<-c(.2,-.4,.3);state<-private('.bh_cor_product_chart')(z,domain)
ok(max(abs(state$theta-widths*tanh(z)))<1e-10,'product chart scientific coordinates')
ok(abs(state$log_jacobian-sum(log(widths)+log(1-tanh(z)^2)))<1e-10,'one Jacobian per unique label')
ok(abs(det(state$R)-prod(1-c(.2,.3,.4)^2-state$theta^2))<1e-10,'complete determinant equals block product')
shared<-private('.bh_cor_domain')(make_table(TRUE),9L);x<-private('.bh_cor_product_chart')(c(.2,-.4),shared)
ok(shared$dimension==2L&&max(abs(vapply(shared$product_domains,`[[`,numeric(1),'half_width')-sqrt(1-c(.3,.4)^2)))<1e-10,'shared label uses intersection interval across blocks')
ok(x$R[1,3]==x$R[4,6],'shared label retains exact expanded equality')
ok(abs(x$log_jacobian-sum(log(sqrt(1-c(.3,.4)^2))+log(1-tanh(c(.2,-.4))^2)))<1e-10,'shared label Jacobian not counted once per block')
joined<-tab;i<-which(joined$row==3&joined$col==4);joined$state[i]<-'fixed';joined$fixed_value[i]<-.1
ok(is.null(private('.bh_cor_product_domain')(joined,9L)),'fixed nonzero bridge prevents false factorization')
err(private('.bh_cor_product_chart')(c(1000,0,0),domain),'E_CORRELATION_NUMERIC')
err(private('.bh_cor_product_chart')(c(0,0),domain),'E_CORRELATION_NUMERIC')
# A constant matrix need not itself be PD: the complete interval can lie away from zero.
nonzero<-tab
for(pair in list(c(1L,2L),c(1L,3L),c(2L,3L))) {
 i<-which(nonzero$row==pair[1]&nonzero$col==pair[2]);nonzero$parameter[i]<-NA_character_
 nonzero$state[i]<-'fixed';nonzero$fixed_value[i]<-.9
}
i<-which(nonzero$row==1&nonzero$col==2);nonzero$state[i]<-'free';nonzero$fixed_value[i]<-NA_real_;nonzero$parameter[i]<-'rho1'
nonzero_domain<-private('.bh_cor_domain')(nonzero,9L)
ok(identical(nonzero_domain$product_status,'available')&&abs(nonzero_domain$product_domains[['rho1']]$lower-.62)<1e-10,'product support includes nonzero-only intervals')
ok(all(eigen(private('.bh_cor_product_chart')(c(0,0,0),nonzero_domain)$R,symmetric=TRUE,only.values=TRUE)$values>0),'product midpoint certifies complete PD matrix')
# Finite differences independently check the unique-label Jacobian.
h<-1e-5
J<-vapply(seq_along(z),function(j) {
 plus<-minus<-z;plus[j]<-plus[j]+h;minus[j]<-minus[j]-h
 (private('.bh_cor_product_chart')(plus,domain)$theta-private('.bh_cor_product_chart')(minus,domain)$theta)/(2*h)
},numeric(length(z)))
ok(abs(log(abs(det(J)))-state$log_jacobian)<1e-8,'analytic chart log Jacobian agrees with independent finite difference')
cat('PASS analytic product intervals, unique-label Jacobians and strict factorization boundaries\n')
