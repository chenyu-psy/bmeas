source(file.path('helpers','native-guard.R'))
# Handwritten shape-only engineering records. No fitting, native compilation,
# sampling, simulation, source acquisition or historical serialized input.
root <- Sys.getenv('MEASUREMENTPKG_SOURCE')
if(nzchar(root))for(f in list.files(file.path(root,'R'),full.names=TRUE))source(f)else library(bmeas)
ns<-if(nzchar(root)).GlobalEnv else asNamespace('bmeas')
private<-function(name)get(name,ns,inherits=FALSE)
passed<-0L
ok<-function(value,label){if(!isTRUE(value))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
refuses<-function(x)inherits(tryCatch({private('.bh_pf_ppc')(x,'cache.ppc');NULL},error=identity),'measurement_error')
local({
  D<-21L;S<-225L
  fields<-c('predictor_slots','generated_coordinate_slots','centered_coordinate_slots','loading_slots',
    'covariance_slots','scalar_name_slots','row_map_slots','statistic_slots')
  allocation<-setNames(as.list(c(124,62,0,1,1,35,40040,(D+40)*S+31*18+8*1001+18^2*31)),fields)
  source_fields<-c('availability','required_reader','observed_rows','maximum_source_rows','recorded_path_count','expected_chains',
    'policy','policy_origin','numeric_buffer_known_lower_bound','numeric_buffer_scope','consumer_buffers','selection','validation_scope','original_fit_authentication')
  plan<-structure(list(version='bmeas_basic_ppc_plan_v3',level='population',law='iid_common_plus_parameter_residual',
    replications=D,observations_per_rep=1001L,simulated_values=21021L,latent_values=651L,
    predictor_values_per_rep=31L,scatter_values=651L,geometry=list(N=31L,J=1L,K=0L,U=1L,S=1L,JT=0L),
    roster=data.frame(person_key=paste0('person',1:31)),active_target_index=1L,
    required_variables=c('alpha[1]','obs_sd[1]','D_parameter[1,1]'),required_columns=3L,
    budget=list(max_draws=32L,max_observations=1100L,max_simulated_values=22000L,max_latent_values=1000L,
      max_statistics=300L,max_pair_terms=1000L,max_work_values=2000000L,posterior=list(reader='streaming')),
    budget_origin=character(),allocation_accounting=allocation,source=setNames(rep(list(NULL),length(source_fields)),source_fields),
    statistics=data.frame(statistic_id=paste0('s',seq_len(S))),
    statistic_counts=list(groups=18L,marginal_statistics=72L,pair_statistics=153L,statistics=S,pair_overlap=0L,pair_terms=0L),
    model_identity=list(specification_fingerprint='engineering',saved_fit_model_fingerprint='engineering',data_identity=NULL),
    scientific_scope='Handwritten engineering controls only',cost_scope='No computation admitted by this record',
    assessment_status='not_run',screening_only=TRUE),class='bmeas_ppc_plan')
  private('.bh_pf_ppc_plan')(plan,'engineering.plan')
  observed<-private('.bh_ppc_stats_empty')()[rep(NA_integer_,S),]
  observed$statistic_id<-plan$statistics$statistic_id;observed$domain<-'marginal';observed$level<-'population';observed$unit<-'response'
  observed$estimate<-0;observed$status<-'computed';observed$degenerate<-FALSE;observed$low_information<-FALSE
  result<-private('.bh_ppc_aggregate')(observed,matrix(0,D,S,dimnames=list(NULL,observed$statistic_id)))
  a<-data.frame(chain_id=c(2L,5L,9L,12L),source_rows=rep(32L,4),selected_rows=c(6L,5L,5L,5L),omitted_rows=c(26L,27L,27L,27L))
  receipt<-list(version=1L,status='acquired',reader=list(package='bmeas',function_name='private_streaming_csv'),
    source_rows=128L,selected_rows=D,selected_required_values=3L*D,selection=list(method='balanced',allocations=a))
  keys<-private('.bh_ppc_source_selection')(a$chain_id,32L,D)
  result$metadata<-list(version='bmeas_basic_ppc_result_v2',level=plan$level,law=plan$law,seed=1L,
    rng_kind=c('Mersenne-Twister','Inversion','Rejection'),R_version=R.version.string,requested_replications=D,completed_replications=D,
    plan=plan,source_qualification='Handwritten engineering control',
    upstream=list(computation=NULL,inference=NULL,identification=NULL,posterior_propriety=NULL,posterior_normalizability=NULL),
    screening_only=TRUE,source_origin_scope='No actual acquisition',interpretation='No posterior evidence',
    posterior_selection=receipt$selection,source_receipt=receipt,draw_keys=keys)
  before<-serialize(result,NULL);set.seed(1107);rng<-.Random.seed
  private('.bh_pf_ppc')(result,'cache.ppc')
  project<-function(x)private('.bh_report_ppc')(x,list(record=list(status='computed',level='population')))
  view<-project(result)
  ok(view$table$availability=='retained'&&view$replications$requested$record==D&&view$coverage$record$planned==S,
    'result v2 admits21 draws and225 statistics through explicit plan budgets')
  ok(view$posterior_source$availability=='retained'&&private('.bh_report_ppc_streaming_display')(view)$availability=='retained',
    'receipt v1 admits21 selected rows only through validated v2 parent context')
  ok(!private('.bh_report_ppc_streaming_source')(receipt),'standalone historical receipt validator still refuses21 rows')
  ok(!private('.bh_report_ppc_table')(result$table)&&!private('.bh_report_ppc_coverage')(result$coverage),
    'standalone historical table and coverage validators preserve200-statistic caps')
  ok(length(capture.output(private('.bh_print_report_ppc')(view)))>0&&identical(before,serialize(result,NULL))&&identical(rng,.Random.seed),
    'scalable passive printing preserves record bytes and RNG')
  bad<-result;bad$metadata$version<-'bmeas_basic_ppc_result_v1'
  ok(refuses(bad)&&project(bad)$table$availability=='unavailable_invalid_metadata','result v1 cannot relabel scalable dimensions')
  bad<-result;bad$metadata$version<-'bmeas_m3cs_ppc_result_v2'
  ok(refuses(bad)&&project(bad)$table$availability=='unavailable_invalid_metadata','result and plan families must match')
  for(field in c('max_draws','max_observations','max_simulated_values','max_latent_values','max_statistics','max_work_values')) {
    bad<-result;bad$metadata$plan$budget[[field]]<-1L
    ok(refuses(bad)&&project(bad)$table$availability=='unavailable_invalid_metadata',paste('explicit plan budget refuses',field))
  }
  bad<-result;bad$metadata$plan$statistic_counts$pair_overlap<-2L;bad$metadata$plan$statistic_counts$pair_terms<-44L;bad$metadata$plan$budget$max_pair_terms<-43L
  ok(refuses(bad),'explicit plan pair work budget refuses')
  for(field in fields) {
    bad<-result;bad$metadata$plan$allocation_accounting[[field]]<-NULL
    ok(refuses(bad),paste('allocation accounting requires exact component',field))
  }
  for(field in fields[unlist(allocation)>0]) {
    bad<-result;bad$metadata$plan$allocation_accounting[[field]]<-0
    ok(refuses(bad),paste('allocation accounting refuses forged zero',field))
  }
  bad<-result;bad$metadata$plan$geometry$JT<-2L
  ok(refuses(bad),'active task dimension cannot exceed declared target dimension')
  for(field in c('simulated_values','predictor_values_per_rep','scatter_values')) {
    bad<-result;bad$metadata$plan[[field]]<-0
    ok(refuses(bad),paste('saved deterministic work matches admitted dimensions',field))
  }
  bad<-result;bad$metadata$plan$allocation_accounting$statistic_slots<-D*S
  ok(refuses(bad),'statistic work admission includes the retained table as well as the matrix')
  bad<-result;bad$metadata$plan$statistic_counts$statistics<-S-1L
  ok(refuses(bad),'plan statistic count matches sum and retained table dimension')
  bad<-result;bad$metadata$completed_replications<-D+1L
  ok(refuses(bad)&&project(bad)$table$availability=='unavailable_invalid_metadata','completed count cannot exceed admitted draws')
  bad<-result;bad$replicate_statistics<-bad$replicate_statistics[-1,,drop=FALSE]
  ok(refuses(bad),'portable scalable result matrix retains exact planned dimensions')
  bad<-result;bad$metadata$plan$version<-'bmeas_basic_ppc_plan_v2'
  ok(refuses(bad)&&project(bad)$posterior_source$availability=='unavailable_invalid_metadata','v2 result requires v3 plan without widening historical plan schema')
  legacy<-plan;legacy$version<-'bmeas_basic_ppc_plan_v2';legacy$allocation_accounting<-NULL;legacy$budget$max_work_values<-NULL
  legacy$replications<-8L;legacy$statistics<-legacy$statistics[1:4,,drop=FALSE];legacy$statistic_counts$statistics<-4L
  private('.bh_pf_ppc_plan')(legacy,'historical.plan')
  ok(TRUE,'historical plan v2 retains its original shape validation')
  for(family in c('basic','m3cs'))for(v in 1:2) {
    old<-legacy;old$version<-paste0('bmeas_',family,'_ppc_plan_v',v)
    if(v==1L) {
      old$budget$posterior<-NULL;old$budget$max_source_bytes<-1048576L;old$budget$max_posterior_rows<-100L;old$budget$max_draw_values<-50000L
      old$source<-list(availability=NULL,required_reader=NULL,observed_rows=NULL,maximum_admissible_source_rows=NULL,
        maximum_materialized_values=NULL,original_fit_authentication=NULL)
    }
    private('.bh_pf_ppc_plan')(old,'historical.plan')
    ok(TRUE,paste('historical',family,'plan v',v,'schema remains accepted'))
  }
})
cat('PASS',passed,'scalable PPC metadata controls\n')
