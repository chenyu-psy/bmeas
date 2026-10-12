# One ordinary/basis/share/marker expression map is used by both augmentations.
# Task augmentation factors whole-row owner scales before required arithmetic.
.bh_coefficient_transform_stan <- function(task=FALSE) {
  code<-c(
    "if(L>0) for(l in 1:L) {",
    "  if(loading_anchor_index[l]>0) loading[l]=loading_anchor[loading_anchor_index[l]];",
    "  else loading[l]=loading_free[loading_free_index[l]];",
    "}",
    "if(LC>0) for(cell in 1:LC) {",
    "  if(loading_cell_primitive[cell]>0) Lambda[loading_cell_target[cell],loading_cell_component[cell]]=loading[loading_cell_primitive[cell]];",
    "  else Lambda[loading_cell_target[cell],loading_cell_component[cell]]=loading_cell_fixed[cell];",
    "}","if(K>0) for(k in 1:K) if(basis_amplitude_index[k]>0) Lambda[,k]=amplitude[basis_amplitude_index[k]]*basis_multiplier[,k];",
    "if(V>0) for(v in 1:V) {",
    "  int w=share_cell_weight[v]; real value=total_sd[share_weight_total_sd[w]]*sqrt(share_weight[w])*share_cell_multiplier[v];",
    "  if(is_nan(value)||is_inf(value)||value==0) reject(\"derived share loading cannot be represented\");",
    "  Lambda[share_cell_target[v],share_cell_component[v]]=value;",
    "}",
    "if(U>0) for(r in 1:U) {",
    "  if(parameter_sd_index[r]>0) parameter_row_sd[r]=parameter_sd[parameter_sd_index[r]];",
    "  else { int w=parameter_share_weight[r]; parameter_row_sd[r]=total_sd[share_weight_total_sd[w]]*sqrt(share_weight[w]); }",
    "  if(is_nan(parameter_row_sd[r])||is_inf(parameter_row_sd[r])||parameter_row_sd[r]<=0) reject(\"derived residual SD cannot be represented\");",
    "  D_parameter[parameter_target[r],r]=parameter_row_sd[r];",
    "}",
    "if(K>0) for(k in 1:K) {",
    "  if(component_sd_index[k]>0) component_row_sd[k]=component_sd[component_sd_index[k]];",
    "  if(is_nan(component_row_sd[k])||is_inf(component_row_sd[k])||component_row_sd[k]<=0) reject(\"component SD cannot be represented\");",
    "  if(J>0) for(j in 1:J) {",
    "    real value=Lambda[j,k]*component_row_sd[k];",
    "    if(is_nan(value)||is_inf(value)||(Lambda[j,k]!=0 && value==0)) reject(\"effective loading cannot be represented; no clipping\");",
    "    effective_loadings[j,k]=value;",
    "  }",
    "}")
  if(!task)return(code)
  code<-gsub("total_sd[share_weight_total_sd[w]]*sqrt(share_weight[w])","sqrt(share_weight[w])",code,fixed=TRUE)
  for(name in c("Lambda","effective_loadings","D_parameter","parameter_row_sd"))
    code<-gsub(paste0("\\b",name,"\\b"),paste0(name,"_work"),code,perl=TRUE)
  code
}
