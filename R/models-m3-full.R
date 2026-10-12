m3_reference_domain <- function() {
  contents<-c("color","image","position")
  ac<-unlist(lapply(c("CS","MU_ori","MU_new"),function(p)paste(p,contents,sep="_")))
  rbind(data.frame(parameter="a",task=ac),data.frame(parameter="c",task=ac),
    data.frame(parameter="ra",task=paste("CS_ra",contents,sep="_")),
    data.frame(parameter="rc",task=c(paste("CS_rc",contents,sep="_"),paste("MU_rc",contents,sep="_"))))
}
m3_reference_bindings <- function(data, observation_id="observation_id", route="route", content="content", version="csmu_exp1_full_v1") {
  if(version!="csmu_exp1_full_v1") .mp_abort("E_MODEL_VERSION","unsupported full M3 binding version")
  if(!all(c(observation_id,route,content)%in%names(data))) .mp_abort("E_PARAMETER_BINDING","provide observation_id, integer route and content columns")
  contents<-c("color","image","position");rows<-list()
  for(o in seq_len(nrow(data))) {
    r<-data[[route]][o];cc<-data[[content]][o]
    if(!r%in%1:3||!cc%in%1:3) .mp_abort("E_ROUTE","M3 route/content values are outside the reference design")
    cn<-contents[cc];rn<-c("CS","MU_no","MU_yes")[r]
    binding<-switch(r,
      list(a=c("a",paste0("CS_",cn)),c=c("c",paste0("CS_",cn)),ra=c("ra",paste0("CS_ra_",cn)),rc=c("rc",paste0("CS_rc_",cn))),
      list(a1=c("a",paste0("MU_ori_",cn)),a2=c("a",paste0("MU_new_",cn)),c1=c("c",paste0("MU_ori_",cn)),ra_fixed=NULL),
      list(a1=c("a",paste0("MU_ori_",cn)),a2=c("a",paste0("MU_new_",cn)),c2=c("c",paste0("MU_new_",cn)),c1=c("c",paste0("MU_ori_",cn)),ra_fixed=NULL,rc=c("rc",paste0("MU_rc_",cn))))
    for(a in names(binding)) {
      fixed<-is.null(binding[[a]])
      rows[[length(rows)+1L]]<-data.frame(observation_id=as.character(data[[observation_id]][o]),route=rn,argument=a,
        kind=if(fixed)"fixed"else"target",target_parameter=if(fixed)NA_character_ else binding[[a]][1L],
        target_task=if(fixed)NA_character_ else binding[[a]][2L],fixed_value=if(fixed)100 else NA_real_,
        fixed_scale=if(fixed)"logit"else NA_character_,stringsAsFactors=FALSE)
    }
  }
  parameter_binding_map(m3_reference_domain(),do.call(rbind,rows))
}
m3_model <- function(file, version="csmu_exp1_full_v1") {
  if(missing(file)) .mp_abort("E_RESOURCE","full M3 requires an explicit hash-pinned external source file")
  if(version!="csmu_exp1_full_v1") .mp_abort("E_MODEL_VERSION","unsupported full M3 reference version")
  base<-m3_cs_model(file);base$name<-"m3_reference_full";base$version<-version
  base$data_schema$route<-data_field("integer",lower=1,upper=3)
  base$data_schema$content<-data_field("integer",lower=1,upper=3)
  base$validate_data<-.mp_stable_callback(.mp_validate_full_m3)
  base$provenance$source<-"External pinned full CS/MU M3 likelihood; explicit per-observation branch bindings; hierarchy is separately declared"
  wrappers<-c(base$adapter$function_code,
    "real measurement_m3_mu_no_lpmf(array[] int counts, array[] int options, real a1, real a2, real c1, real ra_fixed) {",
    "array[5] int y={counts[5],counts[6],counts[7],counts[9],counts[10]}; vector[10] log_options;",
    "for(k in 1:10) log_options[k]=options[k]>0 ? log(options[k]) : 0;",
    "return multinomial_logit_lpmf(y | mu_no_update_logits(a1,a2,c1,ra_fixed,log_options));","}",
    "real measurement_m3_mu_yes_lpmf(array[] int counts, array[] int options, real a1, real a2, real c2, real c1, real ra_fixed, real rc) {",
    "array[6] int y={counts[5],counts[6],counts[7],counts[8],counts[9],counts[10]}; vector[10] log_options;",
    "for(k in 1:10) log_options[k]=options[k]>0 ? log(options[k]) : 0;",
    "return multinomial_logit_lpmf(y | mu_update_logits(a1,a2,c2,c1,ra_fixed,rc,log_options));","}")
  routes<-list(CS=density_route(1,"measurement_m3_cs_lpmf",c("counts","options","a","c","ra","rc"),c("int_array","int_array",rep("real",4)),list(a="a",c="c",ra="ra",rc="rc")),
    MU_no=density_route(2,"measurement_m3_mu_no_lpmf",c("counts","options","a1","a2","c1","ra_fixed"),c("int_array","int_array",rep("real",4)),list(a1="a",a2="a",c1="c",ra_fixed="ra")),
    MU_yes=density_route(3,"measurement_m3_mu_yes_lpmf",c("counts","options","a1","a2","c2","c1","ra_fixed","rc"),c("int_array","int_array",rep("real",6)),list(a1="a",a2="a",c2="c",c1="c",ra_fixed="ra",rc="rc")))
  base<-register_routed_adapter(base,"route",routes,base$adapter$resources,wrappers,c("measurement_m3_cs_lpmf","measurement_m3_mu_no_lpmf","measurement_m3_mu_yes_lpmf"))
  base$validate_bindings<-.mp_stable_callback(.mp_validate_m3_bindings)
  base$informative_rows<-.mp_stable_callback(.mp_m3_full_informative)
  base$capabilities<-model_capabilities(fit=TRUE,simulate=TRUE,posterior_predict=TRUE)
  simulator<-.mp_stable_callback(simulate_m3_responses)
  base$methods<-list(simulate=simulator,posterior_predict=simulator)
  base$limitations<-"Full reference CS/MU branches; fixed MU-ra predictor100; explicit external simulator supports only bounded exact-reference component group PPC; no conditional-person PPC mode; hierarchy separately declared"
  base
}
.mp_validate_full_m3 <- function(data) {
  active<-list(c(1:4,10),c(5:7,9:10),5:10)
  for(o in seq_len(nrow(data))) {
    r<-data$route[o]
    if(!r%in%1:3) .mp_abort("E_ROUTE","unknown M3 route")
    cols<-active[[r]];inactive<-setdiff(1:10,cols)
    if(any(data$options[o,cols]<=0)) .mp_abort("E_M3_OPTIONS","active option multiplicities must be positive")
    if(any(data$counts[o,inactive]!=0,na.rm=TRUE)||any(data$options[o,inactive]!=0)) .mp_abort("E_M3_BRANCH","inactive counts/options must be zero for the declared branch")
  }
  TRUE
}
.mp_validate_m3_bindings <- function(data,map) {
  expected<-m3_reference_bindings(data,observation_id=".observation_id")
  key<-function(x)paste(x$observation_id,x$route,x$argument,sep="::")
  a<-map$bindings;b<-expected$bindings
  if(!setequal(key(a),key(b))||nrow(a)!=nrow(b)) .mp_abort("E_REFERENCE_BINDING","M3 reference binding coverage differs from declared route/content")
  a<-a[match(key(b),key(a)),names(b),drop=FALSE];rownames(a)<-rownames(b)<-NULL
  if(!isTRUE(all.equal(a,b,check.attributes=FALSE))) .mp_abort("E_REFERENCE_BINDING","M3 reference bindings no longer match route/content, shared tasks or fixed predictor100")
  TRUE
}

.mp_m3_full_informative <- function(d) rowSums(d$counts,na.rm=TRUE)>0
