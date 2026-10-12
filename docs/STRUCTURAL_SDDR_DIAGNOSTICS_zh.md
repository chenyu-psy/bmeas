# .39：S4a 单系数零点密度诊断

本增量只读取已完成的结构映射，计算实验性 Gaussian point-KDE 与 log 密度比。
不生成抽样、不调用 lavaan 重拟合、不改变 measurement model；正式 BF 字段始终 NA。
历史 522 项检查验证了正态共轭解析恒等式和部分协方差代数，没有验证 KDE、MCSE 或实际测量模型 BF。

接口：structural_sddr(x, predictor, outcome, null=0,
null_prior="encompassing_conditional", control=list())。

- predictor/outcome 是单个稳定变量 ID；只支持一个普通自由系数。固定或遗漏路径、乘积、多维零约束不支持。
- 两侧 draw 数可以不同，不逐行配对。不删除失败或重复行。任一映射失败阻止密度计算。
- 显式带宽采用 control$bandwidth=c(prior=...,posterior=...)。默认使用 0.9*min(sd,IQR/1.349)*n^(-1/5) 的参考启发式，明确不等于 ESS 调整。
- 出现重复系数或总体状态时，默认带宽保守拒绝；显式带宽仍可诊断，保留原权重。重复可能是正常 MCMC 拒绝，不是伪造结论。
- 两侧带宽分别乘以 .5/.75/1/1.5/2，完整返回 25 个组合。密度用 log-sum-exp 直接算零点值，不假设系数正态分布。
- 常数抽样拒绝。零点不在观测范围内、最窄带宽左右各不足 25 个值时标为局部支持不足。这里是观测计数，不是有效访问数，无法验证真实支持或边界。
- 可用时调用 posterior 的系数 Rhat、bulk/tail ESS；要求至少两条各 20 行、等长、等间隔、保留原顺序且非完全相同的链。它们不是 kernel-density ESS，也不能验证真实后验、独立链或全局识别。MCSE 一律 not_assessed。
- 原生 prior 的正规化联合边缘生成证据与已保留后验的推断状态分别记录。实验性、not_assessed 等上游状态不能由 KDE 升级。
- null_prior 只是待比较模型的定义，不能用一个开关证明其余参数的兼容条件先验。未知 SD 标准化可能改变该条件先验。多系数同时为零必须用联合密度，不能相乘边缘结果。

## 创建时科学证据绑定

新 structural_draws 结果通过已有 canonical plain-data codec 计数后写入并散列。
上限为 64 MiB canonical bytes、400 万 nodes、深度 64；这不声称限制整个 R 进程 RSS。
结果数组、keys/status、固定映射、诊断、输入科学证据均纳入绑定。
原生 prior 的完整参数化、resolved prior、generation 数组及 receipt 保留；旧 prepared spec 在绑定中只包含创建时已验证 fingerprint 和全部 scientific checks。
其余旧 spec（包括 callback captures）保留在返回 provenance，但 S4 不遍历、不认证其后续变化。
此处绑定检测意外修改，不能认证有意且一致重写的对象。旧的未绑定结果拒绝，不允许事后重封。

## 状态与预算

主状态可为 blocked_unbound_result、blocked_missing_prior、blocked_fixed_coefficient、
blocked_incomplete_mapping、density_unavailable 或 experimental_density_ratio_only。
formal BF 始终 not_assessed，log_bf01/bf01/bf10 始终 NA。
成功的数值计算不等于密度稳定、后验有效或 BF 可用。

每侧默认最多 10000 draws，总计最多 100000 次 kernel evaluations；每侧最多 32 chains。
lavaan 当前默认 max_fits=100 是两侧合计的映射预算，本接口不自动提高预算或启动新拟合。

## 方法依据与验证边界

- R 的 Gaussian reference bandwidth：https://stat.ethz.ch/R-manual/R-devel/library/stats/html/bandwidth.html
- 条件 nuisance prior 与修正项：https://arxiv.org/html/2004.09899
- 零测度约束的条件密度版本：https://arxiv.org/abs/0910.1452
- Rhat/ESS 的适用解释：https://sites.stat.columbia.edu/gelman/research/published/Vehtari_etal_2020_rhat_ess.pdf

本增量的确定性分位数 fixture 只用于积分/数值核对，不能当作独立模拟样本；
正态 KDE 先对照已知 Gaussian convolution，再单独区分平滑偏差与精确共轭 BF。
未运行新的随机模拟、HMC、native prior 生成、lavaan fits 或大规模 MCSE 验证。
