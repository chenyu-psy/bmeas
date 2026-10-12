source(file.path("helpers", "native-guard.R"))
# Declared-rank initialization and reference execution profiles; no native calls.
source_root<-Sys.getenv("MEASUREMENTPKG_SOURCE");source_mode<-nzchar(source_root)
if(source_mode)for(f in list.files(file.path(source_root,"R"),full.names=TRUE))source(f)else {
 library(bmeas)
 for(n in c(".bh_centering_basis",".bh_centered_final_marginal_mode",".bh_materialize_init",".bh_initialization",".bh_execution_gate",".bh_compile_identity",".bh_compile_scalar",".bh_reference_compiler_environment",".mp_toolchain_fingerprint"))assign(n,getFromNamespace(n,"bmeas"),envir=.GlobalEnv)
}
count<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);count<<-count+1L;cat("PASS",label,"\n")}
tasks<-c('x1','x2','x3');people<-paste0('p',1:4)
d<-expand.grid(person=people,task=tasks,repeat_id=1:2,stringsAsFactors=FALSE);d$y<-sin(seq_len(nrow(d))*.41)+seq_len(nrow(d))*.03;d$obs<-paste0('synthetic_',seq_len(nrow(d)))
block<-function(parameters=NULL,data=d)observe(gaussian_model(),data,'person','task','y',units='points',observation_id='obs',parameters=parameters)
person<-list(distribution='sample_centered',ids=people);augmentation<-list(type='task_cholesky')
raw<-function(blocks,zero=NULL)list(default='free',zero=zero,coordinates=list(type='ordered_blocks',blocks=blocks))
empty_raw<-raw(list())
prepare<-function(m,b=block()) {
 first<-bhmm(m,list(score=b),fit=FALSE);pr<-get_prior(first);rules<-list()
 distributions<-c(Intercept='normal(0,1)',loading='normal(0,.8)',component_sd='half_normal(.9)',amplitude='half_normal(.8)',parameter_sd='half_normal(.7)',sigma='half_normal(1)',total_sd='half_student_t(3,0,2.5)',share='normal(.15,.8)',cor_raw='normal(0,.25)')
 for(cl in names(distributions))if(any(pr$class==cl))rules[[length(rules)+1L]]<-set_prior(distributions[[cl]],cl)
 for(layer in c('component','parameter'))if(any(pr$class=='cor'&pr$layer==layer))rules[[length(rules)+1L]]<-set_prior('lkj(2)','cor',layer=layer)
 bhmm(m,list(score=b),prior=if(length(rules))do.call(c,rules)else NULL,fit=FALSE)
}
cases<-list()
cases$raw_noncontiguous_fixed<-prepare(bhmf(A=~score$mu['x1'],B=~score$mu[tasks],C=~score$mu['x2'],D=~score$mu['x3'],
 loadings=list(fixed=list(list(value=1,cells=list(A=~score$mu['x1'],C=~score$mu['x2'],D=~score$mu['x3'])),
 list(value=.4,cells=list(B=~score$mu['x1'])),list(value=.6,cells=list(B=~score$mu['x2'])),list(value=.8,cells=list(B=~score$mu['x3'])))),
 residual=list(targets=~score$mu[tasks]),cor=raw(list(first=~C+A+D,second=~B),list(~C+A+D)),person=person,augmentation=augmentation))
cases$direct_parameter_correlation<-prepare(bhmf(residual=list(targets=~score$mu[c('x1','x2')],cor='free'),cor=empty_raw,person=person,augmentation=list(type='task_cholesky',targets=~score$mu[c('x2','x1')])),block(data=d[d$task!='x3',]))
cases$marked_component<-prepare(bhmf(G=~score$mu[tasks],scale=list(G=list(marker=~score$mu['x1'],value=-2)),residual=list(targets=~score$mu[tasks]),cor=raw(list(one=~G)),person=person,augmentation=augmentation))
Q<-sqrt(3/2)*.bh_centering_basis(3);dimnames(Q)<-list(tasks,c('one','two'))
cases$shared_amplitude<-prepare(bhmf(P=component(~score$mu[tasks],Q,'shared'),residual=list(targets=~score$mu[tasks]),cor=raw(list(one=~P),list(~P)),person=person,augmentation=augmentation))
owner<-list(type='variance_share',targets=~score$mu[tasks],components=~G,residual=TRUE,shares=~1)
cases$owned_total_scale<-prepare(bhmf(G=~score$mu[tasks],decomposition=list(all=owner),cor=raw(list(one=~G)),person=person,augmentation=augmentation))
empty<-bhmf(cor=empty_raw,person=person,augmentation=list(type='task_cholesky',targets=~0))
cases$task_means_known_noise<-prepare(empty,block(list(mu='task',sigma=list(fixed=1))))
cases$fixed_means_free_noise<-prepare(empty,block(list(mu=list(fixed=.2))))
cases$fully_fixed<-prepare(empty,block(list(mu=list(fixed=.2),sigma=list(fixed=1))))

# Ordinary final-marginal common MGS now has its own bounded runtime profile.
final_common<-list(A=~score$mu['x1'],B=~score$mu['x2'],C=~score$mu['x3'],
 loadings=list(fixed=list(list(value=1,cells=list(A=~score$mu['x1'],B=~score$mu['x2'],C=~score$mu['x3'])))),
 cor=list(default='free',zero=list(~A:C)),person=person,augmentation=augmentation)
for(kind in c('ordinary','fixed','one_label')) {
 final_args<-final_common
 final_args$residual<-list(targets=~score$mu[tasks],cor=switch(kind,ordinary='independent',
  fixed=list(default='zero',fixed=list(list(pairs=~score$mu['x1']:score$mu['x2'],value=.2))),
  one_label=list(default='zero',equal=list(shared=list(~score$mu['x1']:score$mu['x2'],~score$mu['x1']:score$mu['x3'])))))
 cases[[paste0('final_marginal_',kind)]]<-prepare(do.call(bhmf,final_args))
}
for(name in names(cases)) {
 s<-cases[[name]];expected<-if(startsWith(name,'final_marginal_'))'experimental_centered_task_final_marginal'else'experimental_centered_task_raw'
 ok(check_model(s)$backend$status==expected,paste(name,'retains the correct distinct execution profile'))
}

if(requireNamespace("cmdstanr",quietly=TRUE)) {
for(name in names(cases)) {
 s<-cases[[name]];x<-.bh_materialize_init(s,.bh_initialization(s));d<-standata(s)
 if(!length(x)){ok(sum(get_prior(s)$parameter_count)==0L,'empty init has no dummy parameter');next}
 path<-tempfile(fileext='.json');cmdstanr::write_stan_json(x,path);j<-jsonlite::fromJSON(path,simplifyVector=FALSE)
 for(n in names(x)) {
  if(n=='z_task')ok(length(j[[n]])==d$JT&&all(lengths(j[[n]])==d$N),paste(name,'task score array rank'))else ok(is.list(j[[n]])&&length(j[[n]])==length(x[[n]]),paste(name,n,'Stan vector rank preserved'))
 }
 # Exercise CmdStanR's real R-only init pipeline with the actual declared ranks.
 vars<-list(parameters=stats::setNames(lapply(names(x),function(n)list(dimensions=if(n=='z_task')2L else 1L)),names(x)))
 paths<-cmdstanr:::process_init(function()x,num_procs=1L,model_variables=vars)
 actual<-jsonlite::fromJSON(paths[[1]],simplifyVector=FALSE)
 ok(identical(actual,j),paste(name,'actual CmdStanR init processing preserves rank-safe representation'))
}
# Distinct source-supported combinations keep an honest unavailable profile.
dat<-data.frame(person=as.character(1:4),x1=c(.2,.4,.1,.7),x2=c(.1,.5,.3,-.1))
raw<-list(default='free',coordinates=list(type='ordered_blocks',blocks=list(a=~A,b=~B)))
other<-list(raw_iid=bhmf(A=~x1,B=~x2,cor=raw))
for(n in names(other)) {
 p<-if(n=='raw_iid')set_prior('normal(0,.2)','cor_raw')else NULL
 s<-bhmm(other[[n]],dat,subject='person',prior=p,fit=FALSE)
 ok(check_model(s)$backend$status=='reference_combination_not_runtime_qualified',paste(n,'has separate unsupported capability status'))
 e<-tryCatch(suppressWarnings(.bh_execution_gate(s,execution='experimental')),error=identity)
 ok(e$code=='E_FRONTEND_BACKEND_UNSUPPORTED',paste(n,'cannot inherit another augmentation profile'))
}
# Compiler context restores inherited flags on error; this ends at a fake model
# constructor and never starts stanc, make, a compiler or a model.
s<-cases$marked_component;before<-Sys.getenv(c('CPPFLAGS','CXXFLAGS'),unset=NA_character_)
restore_env<-function(){for(n in names(before))if(is.na(before[[n]]))Sys.unsetenv(n)else do.call(Sys.setenv,setNames(list(before[[n]]),n))}
flag_controls<-list(c('-O2 -O3',''),c('-Og -O2 -g','-g'),c('-O -O0 -O1 -Os -Oz -Ofast',''),
 c('-DLEVEL=-O2 -Wl,-O2 -ObjC','-DLEVEL=-O2 -Wl,-O2 -ObjC'),c('\t-O2\t-O3\t-g','-g'),
 c('-DNOTE="keep -O2 inside" -O3','-DNOTE="keep -O2 inside"'),
 c("-DNOTE='keep -Ofast inside' -Og","-DNOTE='keep -Ofast inside'"),
 c('-DNOTE=keep\\ -O2\\ inside -O3','-DNOTE=keep\\ -O2\\ inside'),
 c('"-O2" \'-Ofast\' -O\\3 -g','-g'))
for(control in flag_controls) {
 Sys.setenv(CPPFLAGS=control[1],CXXFLAGS=control[1]);actual<-.bh_reference_compiler_environment()
 ok(identical(unname(actual),rep(control[2],2L)),'remove complete optimization tokens while preserving other arguments')
}
for(flags in c('-DNOTE="unterminated','-DNOTE=trailing\\')) {
 Sys.setenv(CPPFLAGS=flags,CXXFLAGS='-O2');e<-tryCatch(.bh_reference_compiler_environment(),error=identity)
 ok(inherits(e,'measurement_error')&&e$code=='E_COMPILER_FLAGS','malformed compiler quoting refuses')
 ok(identical(Sys.getenv('CPPFLAGS'),flags),'refused compiler syntax remains unchanged')
}
Sys.setenv(CPPFLAGS='-DTEST=1 -O2 -O3',CXXFLAGS='-Og -O2 -g')
scope<-if(source_mode).GlobalEnv else asNamespace('bmeas')
old_fingerprint<-get('.mp_toolchain_fingerprint',scope)
locked<-bindingIsLocked('.mp_toolchain_fingerprint',scope)
if(locked)unlockBinding('.mp_toolchain_fingerprint',scope)
assign('.mp_toolchain_fingerprint',function(...)list(environment=Sys.getenv(c('CC','CXX','CPPFLAGS','CXXFLAGS','LDFLAGS')),fake='compiler trap test'),scope)
if(locked)lockBinding('.mp_toolchain_fingerprint',scope)
id<-.bh_compile_identity(s,FALSE)
ok(!grepl('(^| )-O',id$toolchain$environment[['CPPFLAGS']])&&!grepl('(^| )-O',id$toolchain$environment[['CXXFLAGS']]),'reference cache identity uses cleaned compiler environment')
local({
 ns<-asNamespace('cmdstanr');old_model<-get('cmdstan_model',ns);seen<-NULL;seen_code<-NULL;seen_args<-NULL
 on.exit({
  unlockBinding('cmdstan_model',ns);assign('cmdstan_model',old_model,ns);lockBinding('cmdstan_model',ns)
  if(locked)unlockBinding('.mp_toolchain_fingerprint',scope)
  assign('.mp_toolchain_fingerprint',old_fingerprint,scope)
  if(locked)lockBinding('.mp_toolchain_fingerprint',scope)
  restore_env()
 },add=TRUE)
 fake<-function(stan_file,...) {
  seen<<-Sys.getenv(c('CPPFLAGS','CXXFLAGS'));seen_code<<-paste(readLines(stan_file,warn=FALSE),collapse='\n');seen_args<<-list(...)
  stop(structure(list(message='expected no-compiler trap',call=NULL),class=c('compiler_test_trap','error','condition')))
 }
 unlockBinding('cmdstan_model',ns);assign('cmdstan_model',fake,ns);lockBinding('cmdstan_model',ns)
 for(name in c('marked_component','final_marginal_ordinary','final_marginal_fixed','final_marginal_one_label')) {
  s<-cases[[name]];before_spec<-serialize(s,NULL,version=2)
  for(log_lik in c(FALSE,TRUE)) {
   e<-tryCatch(.bh_compile_scalar(s,tempfile('reference-compile-trap-'),save_log_lik=log_lik),error=identity)
   ok(inherits(e,'compiler_test_trap')&&identical(unname(seen),c('-DTEST=1','-g')),paste(name,log_lik,'actual compiler helper reaches only terminal constructor with cleaned flags'))
   expected_code<-as.character(stancode(s,threads=2L,log_lik=log_lik))
   ok(identical(seen_code,paste(strsplit(expected_code,'\n',fixed=TRUE)[[1L]],collapse='\n'))&&isTRUE(seen_args$cpp_options$stan_threads)&&seen_args$cpp_options$O==0,paste(name,log_lik,'constructor receives the exact emitted source and bounded compiler options'))
   ok(identical(unname(Sys.getenv(c('CPPFLAGS','CXXFLAGS'))),c('-DTEST=1 -O2 -O3','-Og -O2 -g'))&&identical(before_spec,serialize(s,NULL,version=2)),paste(name,log_lik,'terminal constructor restores inherited flags and sealed context bytes'))
  }
 }
})
restore_env()
cat('INIT/PROFILE',count,'checks passed; no compiler or model calls\n')

}else cat("CmdStanR unavailable: optional serializer/compiler-trap checks skipped\n")
