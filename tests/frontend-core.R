source(file.path("helpers", "native-guard.R"))
# Deterministic declaration/IR tests; no Stan compiler, MCMC or predictions.
if (Sys.getenv("MEASUREMENTPKG_SOURCE") != "") {
  for(f in list.files(file.path(Sys.getenv("MEASUREMENTPKG_SOURCE"),"R"),full.names=TRUE)) source(f)
} else library(bmeas)
passed <- 0L
ok <- function(x,label) { stopifnot(isTRUE(x)); passed <<- passed+1L; cat("PASS",label,"\n") }
err <- function(expr,code) { e <- tryCatch({force(expr);NULL},error=identity);ok(inherits(e,"measurement_error")&&identical(e$code,code),code) }
d <- data.frame(person=paste0("p",1:8))
for(j in 1:18) d[[paste0("x",j)]] <- sin((1:8)*j/4)+j/10
one <- bhmm(bhmf(Ability=~x1+x2+x3),d,subject="person",fit=FALSE)
ok(nrow(model_table(one,"targets"))==3L && nrow(model_table(one,"loadings"))==3L,"S01 scalar component and exact membership")
ok(all(model_table(one,"components")$variance==1) && one$formula$residual=="none","unit latent variance and no hidden parameter residual")
ok(sum(vapply(one$data_map$blocks,nrow,integer(1)))==24L,"one likelihood row per actual response")
ok(check_model(one)$identification$status=="local_rank_full_not_global_proof","local identification status is qualified")
ok(check_model(one)$priors$status=="complete" && all(nzchar(get_prior(one)$source)),"proper visible basic-family default profile")
ok(is.list(attr(get_prior(one),"calibration")),"calibration inputs retained")
ok(check_model(one)$backend$status=="deterministic_gaussian_qualified","deterministic compiler scope is separate from HMC/inference")
err(bhmm(one),"E_RUN_CONFIG")
err(bhmm(one,chains=1,iter=10,warmup=10,resources='development'),"E_RUN_CONFIG")
err(bhmm(one,chains=1,iter=101,warmup=50,resources='development'),"E_SMOKE_BUDGET")
err(bhmm(one,chains=2,iter=20,warmup=10,cores=2,threads=2,resources='development'),"E_RUN_BUDGET")
err(bhmm(one,data=d),"E_INPUT_AMBIGUITY")
err(bhmm(bhmf(A=~x1+x2+x3),d,subject="person",family=stats::gaussian("log"),fit=FALSE),"E_FRONTEND_UNSUPPORTED")
duplicate_columns <- d;names(duplicate_columns)[3] <- names(duplicate_columns)[2]
err(bhmm(bhmf(A=~x1+x2+x3),duplicate_columns,subject="person",fit=FALSE),"E_DATA_COLUMN")
err(bhmf(A=~x1*x2),"E_FORMULA")
err(bhmf(A=~1+x1),"E_FORMULA")
err(bhmf(A=~log(x1)),"E_FORMULA")
err(bhmf(A=~x1+x1),"E_DUPLICATE_LOADING")
ok(is.list(bhmf(A=~x1,residual=list(targets=~x1))$residual),"explicit additive parameter residual declaration")
err(component(~x1,basis=diag(1),loading="shared"),"E_BASIS_NAMES")
bad <- one;bad$data_map$blocks$scores$y[1]<-99
err(model_table(bad),"E_SPEC_CHANGED")
m <- bhmf(A=~x1+x2+x3);m$components$A[[1]]$column <- "x4"
err(bhmm(m,d,subject="person",fit=FALSE),"E_SPEC_CHANGED")
ordinary <- bhmf(A=~x1+x2+x3,B=~x4+x5+x6,C=~x7+x8+x9)
multi <- bhmm(ordinary,d,subject="person",fit=FALSE)
ok(sum(model_table(multi,"correlations")$state=="free")==3L,"S02 three freely correlated factors")
ok(sum(get_prior(multi)$class=="cor")==1L,"one joint correlation prior, not one per free entry")
ok(identical(ordinary$correlations,bhmf(A=~x1+x2+x3,B=~x4+x5+x6,C=~x7+x8+x9,cor=list(default="free"))$correlations),"free shorthand equals default-free background")
bifactor <- bhmm(bhmf(G=~x1+x2+x3+x4+x5+x6+x7+x8+x9,
  A=~x1+x2+x3,B=~x4+x5+x6,C=~x7+x8+x9,cor="independent"),d,subject="person",fit=FALSE)
ok(nrow(model_table(bifactor,"loadings"))==18L && nrow(model_table(bifactor,"targets"))==9L,"S03 bifactor adds18 loading cells to9 targets")
ok(sum(vapply(bifactor$data_map$blocks,nrow,integer(1)))==72L,"bifactor does not duplicate likelihood rows")
ok(all(model_table(bifactor,"correlations")$state=="zero"),"bifactor exact independence")
cross <- bhmm(bhmf(A=~x1+x2+x3+x4,B=~x4+x5+x6+x7+x8),d,subject="person",fit=FALSE)
ok(nrow(model_table(cross,"loadings"))==9L && nrow(model_table(cross,"targets"))==8L,"S04 only one requested crossloading")
ok(!anyDuplicated(model_table(cross,"parameters")$parameter),"free parameter IDs are distinct")
independent <- bhmm(bhmf(A=~x1+x2+x3,B=~x4+x5+x6,cor="independent"),d,subject="person",fit=FALSE)
ok(sum(model_table(independent,"correlations")$state=="free")==0L,"S13 no free correlation parameters")
err(bhmf(A=~x1,B=~x2,cor=list(default="free",zero=list(~A+B),free=list(~A+B))),"E_CORRELATION_CONFLICT")
err(bhmf(A=~x1,B=~x2,cor=list(default="free",zero=list(~A+Unknown))),"E_CORRELATION_TARGET")
# Fixed/equal rules now have a scalar-pair R/IR contract; the historical
# misspelled singular field remains invalid instead of acquiring a meaning.
err(bhmf(A=~x1,B=~x2,cor=list(default="zero",fixed=list(list(pair=~A+B,value=.3)))),"E_CORRELATION_SYNTAX")
graph <- bhmm(bhmf(aG=~x1+x2+x3,aP=~x4+x5+x6,aC=~x7+x8+x9,
  cG=~x10+x11+x12,cP=~x13+x14+x15,cC=~x16+x17+x18,
  cor=list(default="free",zero=list(~aG+aP+aC,~cG+cP+cC,~aP+cC,~aC+cP))),d,subject="person",fit=FALSE)
mask <- model_table(graph,"correlation_mask")
ok(sum(mask[upper.tri(mask)]=="free")==7L,"S17 graph module exact seven marginal free edges")
ok(identical(mask,t(mask)) && all(diag(mask)=="fixed_one"),"full symmetric marginal mask")
ok(mask["aP","cC"]=="zero" && mask["aC","cP"]=="zero" && mask["aG","cG"]=="free","only intended cross zeros")
# Freeze a character task selector; formula must not keep its environment.
long <- demo_gaussian_cfa(); block <- observe(gaussian_model(),long,"person","item","value_z",units="z")
tasks <- c("v1","v2","v3","s1","s2","s3")
m <- bhmf(G=~scores$mu[tasks]); tasks <- "changed"
bound <- bhmm(m,data=list(scores=block),fit=FALSE)
ok(nrow(model_table(bound,"targets"))==6L,"cognitive-style target selector values frozen at declaration")
err(bhmf(G=~scores$mu[paste0("v",1:3)]),"E_SELECTOR")
err(bhmm(bhmf(G=~scores$mu[c("v1","v2","v3")]),data=list(scores=block),fit=FALSE),"E_PARAMETER_COVERAGE")
blocks <- list(a=block,b=block)
a_tasks <- b_tasks <- c("v1","v2","v3","s1","s2","s3")
disjoint_m <- bhmf(A=~a$mu[a_tasks],B=~b$mu[b_tasks],cor="independent")
err(bhmm(disjoint_m,data=blocks,fit=FALSE),"E_SUBJECTS")
separate <- bhmm(disjoint_m,data=blocks,subjects="separate",fit=FALSE)
ok(length(separate$data_map$subject_keys)==16L && check_model(separate)$identification$status!="proven_unidentified","S22 independent disjoint samples accepted")
free_disjoint <- bhmm(bhmf(A=~a$mu[a_tasks],B=~b$mu[b_tasks]),data=blocks,subjects="separate",fit=FALSE)
ok(check_model(free_disjoint)$identification$status=="proven_unidentified","S23 uninformed free correlation rejected for fitting")
err(bhmm(free_disjoint,chains=1,iter=20,warmup=10,resources='development'),"E_IDENTIFICATION")
# Zero-trial likelihood units stay in data but never create an information bridge.
count_data<-function(ids){x<-expand.grid(person=ids,task=c('t1','t2','t3'),stringsAsFactors=FALSE);x$successes<-1L;x$trials<-2L;x}
ba<-count_data(c('a1','a2'));bb<-count_data(c('b1','b2'));bridge<-count_data('bridge');bridge$successes<-bridge$trials<-0L
bind_count<-function(x)observe(binomial_model(),x,'person','task',fields=list(successes='successes',trials='trials'),units='count')
count_formula<-bhmf(A=~a$p[c('t1','t2','t3')],B=~b$p[c('t1','t2','t3')])
zero_bridge<-bhmm(count_formula,data=list(a=bind_count(rbind(ba,bridge)),b=bind_count(rbind(bb,bridge))),subjects='shared',fit=FALSE)
ok(check_model(zero_bridge)$identification$status=='proven_unidentified','zero trials cannot falsely identify a cross-component correlation')
ok(sum(vapply(zero_bridge$data_map$blocks,nrow,integer(1)))==18L && sum(!model_table(zero_bridge,'information')$informative)==6L,'zero-trial observation rows retained unchanged')
bridge$successes<-1L;bridge$trials<-2L
positive_bridge<-bhmm(count_formula,data=list(a=bind_count(rbind(ba,bridge)),b=bind_count(rbind(bb,bridge))),subjects='shared',fit=FALSE)
ok(check_model(positive_bridge)$identification$status=='not_assessed','positive trials provide overlap without claiming mixed-family identification')
all_zero<-ba;all_zero$successes<-all_zero$trials<-0L
no_information<-bhmm(bhmf(A=~a$p[c('t1','t2','t3')]),data=list(a=bind_count(all_zero)),fit=FALSE)
ok(length(check_model(no_information)$identification$uninformed_targets)==3L,'all-zero targets expose lack of likelihood information')
ragged <- data.frame(person=1:6,x1=c(1,2,NA,NA,NA,NA),x2=c(2,3,4,5,NA,NA),x3=c(NA,NA,5,6,NA,NA))
s25 <- bhmm(bhmf(G=~x1+x2+x3),ragged,subject="person",missing="observed_only",fit=FALSE)
id <- check_model(s25)$identification
ok(id$observable_covariance_moments==5L && id$free_covariance_parameters==6L && id$covariance_df== -1L,"S25 actual missingness has negative covariance df")
err(bhmm(s25,chains=1,iter=20,warmup=10,resources='development'),"E_IDENTIFICATION")
ragged$x1[5:6] <- 1:2;ragged$x3[5:6] <- 3:4
control <- bhmm(bhmf(G=~x1+x2+x3),ragged,subject="person",missing="observed_only",fit=FALSE)
ok(all(check_model(control)$identification$probe_ranks==6L),"S25 completed-pair control full local rank")
p <- c(set_prior("normal(0,2)",class="Intercept"),set_prior("normal(0,.5)",class="loading"),set_prior("half_normal(1)",class="sigma"))
custom_prior <- bhmm(bhmf(G=~x1+x2+x3),d,subject="person",prior=p,fit=FALSE)
ok(all(get_prior(custom_prior)$source[get_prior(custom_prior)$scope=="population"]=="user"),"explicit priors replace population defaults")
ok(get_prior(custom_prior)$parameter_count[get_prior(custom_prior)$class=="structural_latent"]==8L,"structural standard-normal person priors explicitly displayed once with multiplicity")
err(set_prior("normal(system('false'),1)",class="Intercept"),"E_PRIOR_SYNTAX")
err(set_prior("normal(1,1)",class="loading"),"E_PRIOR_ORIENTATION")
err(bhmm(bhmf(G=~x1+x2+x3),d,subject="person",prior=c(set_prior("normal(0,1)","loading"),set_prior("normal(0,2)","loading")),fit=FALSE),"E_PRIOR_CONFLICT")
err(bhmm(bhmf(G=~x1+x2+x3),d,subject="person",prior=set_prior("normal(0,1)","loading",component="unknown"),fit=FALSE),"E_PRIOR_SELECTOR")
grouped<-bhmf(A=~(x1+x2)+x3,B=~x4+x5+x6,C=~x7+x8+x9,D=~x10+x11+x12,
  cor=list(default='free',zero=list(~(A+B):(C+D))))
ok(sum(grouped$correlations$state=='zero')==4L,'safe parentheses and Cartesian groups preserve only cross edges')
rotated<-bhmm(bhmf(A=~x1+x2+x3+x4+x5+x6+x7+x8+x9,B=~x1+x2+x3+x4+x5+x6+x7+x8+x9,cor='independent'),d,subject='person',fit=FALSE)
ri<-check_model(rotated)$identification
ok(ri$status=='proven_unidentified'&&ri$latent_covariance_parameters==18L&&ri$latent_covariance_dimension_bound==17L,'rank-aware intrinsic covariance dimension proof')
err(bhmm(rotated,chains=1,iter=20,warmup=10,exploratory=TRUE,resources='development'),'E_IDENTIFICATION')
small_eta<-bhmm(ordinary,d,subject='person',prior=set_prior('lkj(.1)','cor'),fit=FALSE)
ok(check_model(small_eta)$numerical_qualification$status=='execution_unqualified_prior'&&any(grepl('lkj(.1)',get_prior(small_eta)$prior,fixed=TRUE)),'mathematically valid eta<1 stays inspectable unchanged')
ok(is.character(stancode(small_eta)),'eta<1 code inspection remains available')
err(bhmm(small_eta,chains=1,iter=20,warmup=10,resources='development'),'E_NUMERICAL_PRIOR_UNQUALIFIED')
cat("Frontend core tests:",passed,"passed. No compiler or sampler invoked.\n")
