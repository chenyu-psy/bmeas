# 窄范围联合成分总体先验

新增 component_prior_draws(spec, draws, seed, control=list())。输入当前 prepared specification；输出与 .36 后验 adapter 完全同模型、同 prior、同坐标与方向定义的 bmeas_population_draws。配合 structural_draws() 得到同一解析回归 functional 的诱导先验/后验。生成器本身不计算密度或 BF，不调用 lavaan、不增加 likelihood、不重拟合，也不运行 Stan/HMC。独立后处理的可选 lavaan 映射和 S4a 密度诊断分别见 STRUCTURAL_LAVAAN_zh.md 与 STRUCTURAL_SDDR_DIAGNOSTICS_zh.md；正式 BF 与 density MCSE 仍不可用。

## 支持范围

- 完整声明的 K 个普通 scalar 成分，iid/common augmentation
- 每个非对角相关 pair 恰好出现一次，全部自由且标签互异；final_marginal_mask
- 单个 LKJ(eta) 联合 determinant prior，有限固定 1≤eta≤10000；K=1 为确定性单位矩阵
- 所有成分总体 SD 固定为 1，方向已解析
- 完整且独立、适当的已解析 nuisance primitive priors：支持的截距、噪声、零中心 Normal loading（positive anchor 为正规化 half-Normal）与标准 Normal white coordinates；每个 primitive 只计一次

eta 的上限 10000 是本实现对 Beta 变换数值精度的保守支持限制，不是 LKJ 适当性的数学上限。更大的有限形状可能使 2*Beta−1 在浮点数上静默退化；当前明确拒绝，不另换先验。

首版不支持受限/固定/相等/零/product 相关图、raw-B、marker、basis、share、loading constraints、parameter-residual 层或 portable specification 输入。所有 parameter-residual 层的排除只是审计范围限制：普通独立且适当的 residual 因子数学上可以积分为 1，不改变成分 R 的边缘先验。

该输出是完整 K 维成分总体协方差的正规化联合边缘先验，不是完整 nuisance 参数样本。已记录的数据校准先验超参数保持固定，不重新校准、不生成 prior-predictive 数据。参数化专属先验不自动在 marker/unit-SD 之间转移。

## 算法与来源

按列 j 生成 canonical partial correlations：Beta(eta+(K-j-1)/2, eta+(K-j-1)/2) 经 2x−1 变换，再按剩余行长度重构 Cholesky 因子，R=LL'、Sigma=R。这些独立量是 canonical partial correlations，不是边缘相关。完整 K 不得换成下游变量子集后仍用相同 eta。

数学与算法核对来源：Stan Math lkj_corr_cholesky_rng.hpp、read_corr_L.hpp 及官方 Correlation Matrix Distributions 文档。参考执行日期为 2026-10-10；链接为活动文档，并非声称固定上游提交。

- https://raw.githubusercontent.com/stan-dev/math/develop/stan/math/prim/prob/lkj_corr_cholesky_rng.hpp
- https://mc-stan.org/math/prim_2fun_2read__corr___l_8hpp_source.html
- https://mc-stan.org/docs/functions-reference/correlation_matrix_distributions.html

## 可复现性、失败与证据

所有语义与数值预算在 RNG 变更前检查；退出时恢复 RNGkind 与原 .Random.seed（含原先不存在）。seed 必须显式给出。draw_keys 的 chain_id=1 仅表示一个 IID batch，不是假造 HMC 链。失败保留 draw ID 与不可用矩阵，不修补、不裁剪、不重抽、不静默筛选。

first-party prior source_kind 与 external/synthetic 分开；后者仍为用户声明、未经来源验证。原生结果通过 prepared-spec seal 和保留生成结果/receipt 的完整性检查重建，不在结构映射时重跑 RNG。完整性检查不是外部来源认证。receipt 保存算法/代码摘要、K/eta、完整坐标、模型/先验身份、审计范围、seed/RNG/R 版本、成功失败数及预算。receipt 位于 scientific parameterization 之外，避免改变与后验完全相同的科学映射。

numeric/work 预算计入矩阵副本、availability、keys、canonical-coordinate 与矩阵工作空间；不声称是 RSS、总耗时或 metadata serialization 上限。prepared spec seal 与生成证据摘要的序列化成本单列披露。

## 验证解释

独立参考分布检查覆盖普通 LKJ 的边缘矩/分布、determinant expectation、联合三角乘积依赖、RNG 恢复和失败 ID。当前包仍需针对当前提交验证，历史参考结果不替代完整包检查。

正确先验抽样不证明后验适当性、全局识别、HMC 合格、真实研究有效或 SDDR 条件先验兼容。当前限定 lavaan 映射和 S4a 单系数密度诊断见对应技术文档；正式 BF 与 density MCSE 仍不可用。
